#include "il-marker-pass.hpp"

#include "module/module.hpp"

namespace Noctis
{
	ILMarkerPass::ILMarkerPass()
		: ILPass("IL marker pass")
	{
	}

	void ILMarkerPass::Process(ILModule& mod)
	{
		for (ILFuncDefSPtr funcDef : mod.funcs)
		{
			for (ILBlock& block : funcDef->blocks)
			{
				ILBlockDependencyNodeSPtr depNode = funcDef->graph.GetOrAddBlockDependency(block.label);

				depNode->PropagateCanTouch();

				if (block.terminal->kind == ILKind::Goto)
				{
					depNode->onlyGoto = block.elems.empty();


					if (depNode->references.size() == 1)
					{
						u32 nextId = (*depNode->references.begin()).first;
						ILBlockDependencyNodeSPtr nextDepNode = funcDef->graph.GetOrAddBlockDependency(nextId);
						depNode->canMerge = nextDepNode->refBy.size() == 1;
					}
				}
			}
		}
		
	}
}
