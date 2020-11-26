#include "il-clean-pass.hpp"

#include "module/module.hpp"

namespace Noctis
{
	ILRemoveUntouchableBlockPass::ILRemoveUntouchableBlockPass(Context* pCtx)
		: ILPass("remove unreachable blocks", pCtx)
	{
	}

	void ILRemoveUntouchableBlockPass::Process(ILModule& mod)
	{
		for (ILFuncDefSPtr funcDef : mod.funcs)
		{
			StdUnorderedMap<u32, u32> remapping;
			
			for (u32 newId = 0, curId = 0; newId < funcDef->blocks.size(); ++curId)
			{
				ILBlockDependencyNodeSPtr depNode = funcDef->graph.GetOrAddBlockDependency(curId);

				remapping.try_emplace(curId, newId);

				if (!depNode->canTouch)
				{	
					funcDef->blocks.erase(funcDef->blocks.begin() + newId);
				}
				else
				{
					funcDef->blocks[newId].label = newId;
					++newId;
				}
			}

			for (ILBlock& block : funcDef->blocks)
			{
				switch (block.terminal->kind)
				{
				case ILKind::If:
				{
					ILIf& term = static_cast<ILIf&>(*block.terminal);
					auto trueIt = remapping.find(term.trueLabel);
					auto falseIt = remapping.find(term.falseLabel);
					term.trueLabel = trueIt->second;
					term.falseLabel = falseIt->second;
					break;
				}
				case ILKind::Switch:
				{
					ILSwitch& term = static_cast<ILSwitch&>(*block.terminal);
					for (StdPair<ILVar, u32>& case_ : term.cases)
					{
						auto it = remapping.find(case_.second);
						case_.second = it->second;
					}

					auto it = remapping.find(term.defCase);
					term.defCase = it->second;
					
					break;
				}
				case ILKind::Goto:
				{
					ILGoto& term = static_cast<ILGoto&>(*block.terminal);
					auto it = remapping.find(term.label);
					term.label = it->second;
					break;
				}
				default: ;
				}
			}
		}
	}

	ILBlockMergePass::ILBlockMergePass(Context* pCtx)
		: ILPass("block merge pass", pCtx)
	{
	}

	void ILBlockMergePass::Process(ILModule& mod)
	{
		for (ILFuncDefSPtr funcDef : mod.funcs)
		{
			for (ILBlock& block : funcDef->blocks)
			{
				ILBlockDependencyNodeSPtr depNode = funcDef->graph.GetOrAddBlockDependency(block.label);
				if (!depNode->canMerge)
					continue;

				ILBlock& nextBlock = funcDef->blocks[(*depNode->references.begin()).first];

				block.elems.insert(block.elems.end(), nextBlock.elems.begin(), nextBlock.elems.end());
				block.terminal = nextBlock.terminal;

				ILBlockDependencyNodeSPtr nextDepNode = funcDef->graph.GetOrAddBlockDependency(nextBlock.label);
				depNode->references = nextDepNode->references;

				nextDepNode->refBy.clear();
				nextDepNode->references.clear();
				nextDepNode->canMerge = false;
				nextDepNode->canTouch = false;
			}
		}
	}

	ILRemoveGotoOnlyPass::ILRemoveGotoOnlyPass(Context* pCtx)
		: ILPass("remove goto only", pCtx)
	{
	}

	void ILRemoveGotoOnlyPass::Process(ILModule& mod)
	{
		for (ILFuncDefSPtr funcDef : mod.funcs)
		{
			StdUnorderedMap<u32, u32> gotoMapping;

			for (ILBlock& block : funcDef->blocks)
			{
				ILBlockDependencyNodeSPtr depNode = funcDef->graph.GetOrAddBlockDependency(block.label);
				if (!depNode->onlyGoto)
					continue;

				ILGoto& term = static_cast<ILGoto&>(*block.terminal);
				gotoMapping.try_emplace(block.label, term.label);

				depNode->canTouch = false;
			}

			auto getGoto = [&](u32 id)
			{
				auto getGotoImpl = [&](u32 id, auto& getFun)
				{
					auto it = gotoMapping.find(id);
					if (it == gotoMapping.end())
						return id;

					return getFun(it->second, getFun);
				};
				return getGotoImpl(id, getGotoImpl);
			};

			for (ILBlock& block : funcDef->blocks)
			{
				ILBlockDependencyNodeSPtr depNode = funcDef->graph.GetOrAddBlockDependency(block.label);

				
				switch (block.terminal->kind)
				{
				case ILKind::If:
				{
					ILIf& term = static_cast<ILIf&>(*block.terminal);
					
					u32 trueLabel = getGoto(term.trueLabel);
					depNode->ChangeRef(term.trueLabel, trueLabel);
					term.trueLabel = trueLabel;
					
					u32 falseLabel = getGoto(term.falseLabel);
					depNode->ChangeRef(term.falseLabel, falseLabel);
					term.falseLabel = falseLabel;
					break;
				}
				case ILKind::Switch:
				{
					ILSwitch& term = static_cast<ILSwitch&>(*block.terminal);

					for (StdPair<ILVar, u32>& case_ : term.cases)
					{
						u32 newId = getGoto(case_.second);
						depNode->ChangeRef(case_.second, newId);
						case_.second = newId;
					}

					u32 newId = getGoto(term.defCase);
					depNode->ChangeRef(term.defCase, newId);
					term.defCase = newId;
					
					break;
				}
				case ILKind::Goto:
				{
					ILGoto& term = static_cast<ILGoto&>(*block.terminal);
					u32 newId = getGoto(term.label);
					depNode->ChangeRef(term.label, newId);
					term.label = newId;
					break;
				}
				default: ;
				}
			}
		}
	}
}
