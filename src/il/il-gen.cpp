#include "il-gen.hpp"

#include "il.hpp"
#include "itr/itr.hpp"
#include "common/context.hpp"
#include "module/module.hpp"
#include "module/function.hpp"

namespace Noctis
{
	template<typename T>
	void ILGen::AddElem(T* elem, u32 label)
	{
		static_assert(std::is_base_of_v<ILElem, T>, "");
		if (label == u32(-1))
			m_pCurBlock->elems.emplace_back(elem);
		else
			m_Def->blocks[label].elems.emplace_back(elem);
	}
	
	template<typename T>
	void ILGen::SetTerminal(T* terminal, u32 label)
	{
		static_assert(std::is_base_of_v<ILTerminal, T>, "");
		if (label == u32(-1))
			m_pCurBlock->terminal.reset(terminal);
		else
			m_Def->blocks[label].terminal.reset(terminal);
	}
	
	ILGen::ILGen(Context* pCtx)
		: ITrSemanticPass("il gen", pCtx)
		, m_pILMod(nullptr)
		, m_pCurBlock(nullptr)
		, m_CurLabel(0)
		, m_CurDeferLabel(0)
		, m_CurVarId(0)
		, m_FallThrough(false)
		, m_CurSwitchPatternDepth(0)
	{
	}

	void ILGen::Process(ITrModule& mod)
	{
		SetModule(mod);

		m_pILMod = &m_pCtx->activeModule->ilMod;

		Foreach(ITrVisitorDefKind::Any, [&, this](ITrFunc& node)
		{
			if (node.funcKind == ITrFuncKind::EmptyMethod)
				return;

			m_CurLabel = 0;
			m_CurVarId = 0;

			StdVector<ILGeneric> generics;
			if (node.genDecl)
			{
				for (ITrGenParamSPtr param : node.genDecl->params)
				{
					if (param->isType)
					{
						const ITrGenTypeParam& genType = static_cast<ITrGenTypeParam&>(*param);
						generics.push_back(ILGeneric{ genType.iden });
					}
					else
					{
						const ITrGenValParam& genVal = static_cast<ITrGenValParam&>(*param);
						generics.push_back(ILGeneric{ genVal.iden, genVal.type->handle });
					}
				}
			}

			if (node.impl && node.impl->genDecl)
			{
				for (ITrGenParamSPtr param : node.impl->genDecl->params)
				{
					if (param->isType)
					{
						const ITrGenTypeParam& genType = static_cast<ITrGenTypeParam&>(*param);
						generics.push_back(ILGeneric{ genType.iden });
					}
					else
					{
						const ITrGenValParam& genVal = static_cast<ITrGenValParam&>(*param);
						generics.push_back(ILGeneric{ genVal.iden, genVal.type->handle });
					}
				}
			}

			QualNameSPtr qualName = node.sym.lock()->qualName;
			m_Def.reset(new ILFuncDef{ m_pCtx, qualName, std::move(generics) });
			m_Def->sym = node.sym;
			m_FuncScope = node.qualName;

			// params
			for (ITrParamSPtr param : node.params)
			{
				u32 id = m_CurVarId++;
				ILVar var{ ILVarKind::Copy, id, param->type->handle };


				auto it = m_VarMapping.try_emplace(param->iden, StdVector<ILVar>{}).first;
				it->second.push_back(var);

				m_Def->params.push_back(var);

				m_pILMod->types.insert(var.type.Type());
			}

			if (node.retType)
			{
				m_Def->retType = node.retType->handle;
				m_pILMod->types.insert(m_Def->retType.Type());
			}
			
			m_FuncCtx = node.ctx;

			m_FuncCtx->localVars.Foreach([this](LocalVarDataSPtr varData)
			{
				if (varData->isParam)
					return;
				
				u32 id = m_CurVarId++;
				ILVar var{ ILVarKind::Copy, id, varData->typeInfo.handle };
				varData->ilVar = var;
				m_Def->localVars.push_back(var);

				m_pILMod->types.insert(var.type.Type());
			});

			AddNewBlock();
			Walk(node);

			if (node.attribs && !node.attribs->atAttribs.empty())
			{
				for (ITrAtAttribSPtr atAttrib : node.attribs->atAttribs)
				{
					if (!atAttrib->isCompAttrib)
						continue;

					if (atAttrib->iden->Name() == "compintrin")
					{
						StdString intrinName = static_cast<ITrLiteral&>(*atAttrib->args[0]->expr).lit.Text();
						intrinName.erase(intrinName.begin());
						intrinName.erase(intrinName.end() - 1);
						ImplementCompilerIntrin(intrinName);
					}
				}
			}

			if (!m_pCurBlock->terminal)
				SetTerminal(new ILReturn{});

			m_pCtx->activeModule->ilMod.funcs.push_back(m_Def);
		});
	}

	void ILGen::Visit(ITrBlock& node)
	{
		u32 oldId = m_pCurBlock->label;
		u32 newId = AddNewBlock();
		m_ITrBlockMapping.try_emplace(&node, newId);

		SetTerminal(new ILGoto{ newId }, oldId);
		
		m_ScopeNames.push_back(node.scopeName);
		SetCurBlock(newId);
		Walk(node);
		m_ScopeNames.pop_back();
	}

	void ILGen::Visit(ITrIf& node)
	{
		ITrVisitor::Visit(node.cond);
		ILVar cond = PopTmpVar();

		u32 oldId = m_pCurBlock->label;
		u32 trueId = AddNewBlock();
		u32 falseId = node.fBlock ? AddNewBlock() : 0;
		u32 endId = AddNewBlock();
		if (!node.fBlock)
			falseId = endId;

		SetTerminal(new ILIf{ cond, trueId, falseId }, oldId);

		SetCurBlock(trueId);
		Visit(*node.tBlock);

		if (node.fBlock)
		{ 
			SetCurBlock(falseId);
			Visit(*node.fBlock);
		}
		
		SetCurBlock(endId);
	}

	void ILGen::Visit(ITrLoop& node)
	{
		u32 startId = m_pCurBlock->label;
		u32 loopId = AddNewBlock();
		u32 endId = AddNewBlock();

		SetTerminal(new ILGoto{ loopId }, startId);

		m_LoopBeginLabels.push(loopId);
		m_LoopEndLabels.push(endId);
		m_ScopeNames.push_back(node.scopeName);
		
		SetCurBlock(loopId);
		Walk(node);
		
		m_ScopeNames.pop_back();
		m_LoopEndLabels.pop();
		m_LoopBeginLabels.pop();

		SetTerminal(new ILGoto{ endId });
		SetCurBlock(endId);
	}

	void ILGen::Visit(ITrSwitch& node)
	{
		ITrVisitor::Visit(node.expr);
		ILVar cond = PopTmpVar();
		m_SwitchVars.push(cond);

		ProcessSwitchGroup(node, node.baseGroup);

		for (StdPair<usize, StdVector<u32>> pair : m_CaseToBodyBlocks)
		{
			ITrSwitchCase& case_ = node.cases[pair.first];
			u32 caseId = AddNewBlock();
			AssignValueBinds(pair.first);
			Visit(*case_.block);
			m_CaseToTermBlocks.push_back(m_pCurBlock->label);
			
			for (u32 blockId : pair.second)
			{
				SetTerminal(new ILGoto{ caseId }, blockId);
			}
		}

		u32 endBlock = AddNewBlock();
		for (u32 blockId : m_CaseToTermBlocks)
		{
			SetTerminal(new ILGoto{ endBlock }, blockId);
		}

		SetCurBlock(endBlock);
	}

	void ILGen::Visit(ITrLabel& node)
	{
		AddNewBlock();
		m_LabelMapping.try_emplace(node.label, m_pCurBlock->label);
	}

	void ILGen::Visit(ITrBreak& node)
	{
		ILTerminalSPtr term;
		if (node.label)
		{
			auto it = m_LabelMapping.find(node.label);
			SetTerminal(new ILGoto{ it->second });
		}
		else
		{
			SetTerminal(new ILGoto{ m_LoopEndLabels.top() });
		}
		AddNewBlock();
	}

	void ILGen::Visit(ITrContinue& node)
	{
		ILElemSPtr elem;
		if (node.label)
		{
			auto it = m_LabelMapping.find(node.label);
			SetTerminal(new ILGoto{ it->second });
		}
		else
		{
			SetTerminal(new ILGoto{ m_LoopBeginLabels.top() });
		}
		AddNewBlock();
	}

	void ILGen::Visit(ITrFallthrough& node)
	{
		m_FallThrough = true;
	}

	void ILGen::Visit(ITrGoto& node)
	{
		auto it = m_LabelMapping.find(node.label);
		SetTerminal(new ILGoto{ it->second });
		AddNewBlock();
	}

	void ILGen::Visit(ITrReturn& node)
	{
		Walk(node);

		ILElemSPtr elem;
		if (node.expr)
			SetTerminal(new ILReturn{ PopTmpVar() });
		else
			SetTerminal(new ILReturn{});
	}

	void ILGen::Visit(ITrThrow& node)
	{
	}

	void ILGen::Visit(ITrDefer& node)
	{
		/*u32 curLabel = m_pCurBlock->label;

		u32 deferLabel = AddNewBlock();

		// TODO

		// Reset
		SetCurBlock(curLabel);*/
	}

	void ILGen::Visit(ITrLocalVar& node)
	{
		Walk(node);

		StdVector<ILVar> curVars;
		for (IdenSPtr iden : node.idens)
		{
			LocalVarDataSPtr varData = m_FuncCtx->localVars.GetLocalVarData(m_ScopeNames, iden);
			ILVar dst = varData->ilVar;
			MapVar(iden, dst);

			// TODO: multiple init
			if (node.init)
				AddElem(new ILAssign{ dst, PopTmpVar() });
		}
	}

	void ILGen::Visit(ITrAssign& node)
	{
		Walk(node);
		ILVar src = PopTmpVar();
		ILVar dst = PopTmpVar();

		if (src.type.AsBase().hasFuzzyCompare || dst.type.AsBase().hasFuzzyCompare)
			m_pCurBlock->elems.emplace_back(new ILCompIntrin{ ILVar{}, ILCompIntrinKind::FuzzyTypeComp, {}, {src.type, dst.type} });

		ILElemSPtr elem;
		if (node.op == OperatorKind::Eq)
		{
			AddElem(new ILAssign{ dst, src });
		}
		else if (node.operator_.isBuiltin)
		{
			AddElem(new ILPrimAssign{ node.op, dst, src });
		}
		else
		{
			AddElem(new ILCompIntrin{ dst, ILCompIntrinKind::BytewiseCopy, { src }, {} });
			
			// TODO: What if a function like Copy() exist
		}

		m_TmpVars.push(dst);
	}

	void ILGen::Visit(ITrTernary& node)
	{
		Walk(node);
		ILVar src1 = PopTmpVar();
		ILVar src0 = PopTmpVar();
		ILVar cond = PopTmpVar();
		ILVar dst = CreateDstVar(node.typeInfo.handle);

		if (src1.type.AsBase().hasFuzzyCompare || src0.type.AsBase().hasFuzzyCompare)
			AddElem(new ILCompIntrin{ ILVar{}, ILCompIntrinKind::FuzzyTypeComp, {}, {src0.type, src1.type} });

		AddElem(new ILTernary{ dst, cond, src0, src1 });
		m_TmpVars.push(dst);
	}

	void ILGen::Visit(ITrBinary& node)
	{
		Walk(node);
		ILVar right = PopTmpVar();
		ILVar left = PopTmpVar();
		ILVar dst = CreateDstVar(node.typeInfo.handle);

		ILElemSPtr elem;
		if (node.operator_.isBuiltin)
			AddElem(new ILPrimBinary{ node.op, dst, left, right });
		else
			AddElem(new ILFuncCall{ dst, node.operator_.sym->qualName, { right, left } });
	}

	void ILGen::Visit(ITrUnary& node)
	{
		Walk(node);
		ILVar var = PopTmpVar();
		ILVar dst = CreateDstVar(node.typeInfo.handle);

		ILElemSPtr elem;
		if (node.operator_.isBuiltin)
			AddElem(new ILPrimUnary{ node.op, dst, var });
		else
			AddElem(new ILFuncCall{ dst, node.operator_.sym->qualName, { var } });
	}

	void ILGen::Visit(ITrQualNameExpr& node)
	{
		// First look for a local var
		LocalVarDataSPtr local = m_FuncCtx->localVars.GetLocalVarData(m_ScopeNames, node.qualName->LastIden());
		if (local)
		{
			ILVar var;
			auto it = m_VarMapping.find(local->iden);
			if (it == m_VarMapping.end())
			{
				var = ILVar{ ILVarKind::Copy, std::numeric_limits<u32>::max(), local->typeInfo.handle };
				m_pILMod->types.insert(local->typeInfo.handle.Type());
			}
			else
			{
				var = it->second.back();
				m_pILMod->types.insert(var.type.Type());
			}

			m_TmpVars.push(var);
		}
		else
		{
			SymbolSPtr sym = m_pCtx->activeModule->symTable.Find(GetCurScope(), node.qualName);

			if (sym->kind == SymbolKind::ValEnumMember)
			{
				ILVar dst = CreateDstVar(node.typeInfo.handle);
				AddElem(new ILValEnumInit{ dst, node.qualName->LastIden()->Name() });
			}
			else if (sym->kind == SymbolKind::AdtEnumMember)
			{
				ILVar dst = CreateDstVar(node.typeInfo.handle);
				AddElem(new ILAdtEnumInit{ dst, node.qualName->LastIden()->Name(), {} });
			}
			else
			{
				// TODO: Global var
			}
		}
	}

	void ILGen::Visit(ITrIndexSlice& node)
	{
		Walk(node);

		if (node.to)
		{
			// TODO
		}
		else if (node.explicitSlice)
		{
			// TODO
		}
		else
		{
			ILVar idx = PopTmpVar();
			ILVar src = PopTmpVar();
			
			ILVar dst = CreateDstVar(node.typeInfo.handle);
			AddElem(new ILIndex{ dst, src, idx });
		}
	}

	void ILGen::Visit(ITrFuncCall& node)
	{
		Walk(node);
		
		usize argCnt = node.args.size();
		StdVector<ILVar> args;
		args.reserve(argCnt);
		for (usize i = 0; i < argCnt; ++i)
		{
			args.push_back(PopTmpVar());
		}
		std::reverse(args.begin(), args.end());

		if (node.isMethod)
		{
			ILVar caller = PopTmpVar();

			const StdString& name = node.sym->qualName->LastIden()->Name();

			if (node.typeInfo.handle.IsValid())
			{
				ILVar dst = CreateDstVar(node.typeInfo.handle);
				AddElem(new ILMethodCall{ dst, caller, name, args });
			}
			else
			{
				AddElem(new ILMethodCall{ caller, name, args });
			}
		}
		else
		{
			if (node.sym)
			{
				TypeHandle retType = node.typeInfo.handle;
				if (retType.IsValid())
				{
					ILVar dst = CreateDstVar(node.typeInfo.handle);
					AddElem(new ILFuncCall{ dst, node.sym->qualName, args });
				}
				else
				{
					AddElem(new ILFuncCall{ node.sym->qualName, args });
				}
			}
			else
			{
				ILVar func = PopTmpVar();

				TypeHandle retType = node.typeInfo.handle;
				if (retType.IsValid())
				{
					ILVar dst = CreateDstVar(node.typeInfo.handle);
					AddElem(new ILIndirectCall{ dst, func, args });
				}
				else
				{
					AddElem(new ILIndirectCall{ func, args });
				}
			}
		}
		
	}

	void ILGen::Visit(ITrMemberAccess& node)
	{
		Walk(node);

		m_pILMod->names.insert(node.iden->Name());
		
		ILVar src = PopTmpVar();
		ILVar dst = CreateDstVar(node.typeInfo.handle);

		AddElem(new ILMemberAccess{ dst, src, node.iden->Name() });
	}

	void ILGen::Visit(ITrTupleAccess& node)
	{
		Walk(node);
		ILVar src = PopTmpVar();
		ILVar dst = CreateDstVar(node.typeInfo.handle);

		AddElem(new ILTupleAccess{ dst, src, node.index });
	}

	void ILGen::Visit(ITrLiteral& node)
	{
		ILVar var = CreateLitVar(node.lit);
		m_TmpVars.push(var);
	}

	void ILGen::Visit(ITrStructInit& node)
	{
		Walk(node);

		ILVar defVar;
		if (node.defExpr)
			defVar = PopTmpVar();

		StdVector<ILVar> unorderedArgs;
		bool namedArgs = false;
		for (usize i = 0; i < node.args.size(); ++i)
		{
			if (i == 0)
				namedArgs = !!node.args[i]->iden;

			unorderedArgs.push_back(PopTmpVar());
		}
		std::reverse(unorderedArgs.begin(), unorderedArgs.end());

		StdVector<ILVar> args;
		if (namedArgs)
		{
			args.resize(node.sym->orderedVarChildren.size());

			for (usize i = 0; i < unorderedArgs.size(); ++i)
			{
				u32 idx = node.argOrder[i];
				args[idx] = unorderedArgs[i];
			}

			if (unorderedArgs.size() < node.sym->orderedVarChildren.size())
			{
				if (!node.defExpr)
				{
					defVar = CreateDstVar(node.typeInfo.handle);

					// TODO: Gen Default
				}

				for (usize i = 0; i < node.sym->orderedVarChildren.size(); ++i)
				{
					if (args[i].kind == ILVarKind::Lit ||
						args[i].type.IsValid())
						continue;
					
					SymbolSPtr child = node.sym->orderedVarChildren[i].lock();
					ILVar tmpDst = CreateDstVar(child->type);
					AddElem(new ILMemberAccess{ tmpDst, defVar, child->qualName->LastIden()->Name() });

					args[i] = tmpDst;
				}
			}
		}
		else
		{
			args = unorderedArgs;
			if (args.size() < node.sym->orderedVarChildren.size())
			{
				if (!node.defExpr)
				{
					defVar = CreateDstVar(node.typeInfo.handle);

					// TODO: Gen Default
				}
				
				for (usize i = args.size(); i < node.sym->orderedVarChildren.size(); ++i)
				{
					SymbolSPtr child = node.sym->orderedVarChildren[i].lock();
					ILVar tmpDst = CreateDstVar(child->type);
					AddElem(new ILMemberAccess{ tmpDst, defVar, child->qualName->LastIden()->Name() });

					args.push_back(tmpDst);
				}
			}
		}

		ILVar dst = CreateDstVar(node.typeInfo.handle);
		
		AddElem(new ILStructInit{ dst, args });
	}

	void ILGen::Visit(ITrUnionInit& node)
	{
		Walk(node);

		ILVar arg = PopTmpVar();
		ILVar dst = CreateDstVar(node.typeInfo.handle);
		AddElem(new ILUnionInit { dst, arg });
	}

	void ILGen::Visit(ITrAdtTupleEnumInit& node)
	{
		StdVector<ILVar> args;
		for (ITrArgSPtr arg : node.args)
		{
			ITrVisitor::Visit(arg->expr);
			args.push_back(PopTmpVar());
		}
		
		ILVar dst = CreateDstVar(node.typeInfo.handle);
		AddElem(new ILAdtEnumInit{ dst, node.sym->qualName->LastIden()->Name(), args });
	}

	void ILGen::Visit(ITrAdtAggrEnumInit& node)
	{
		Walk(node);

		ILVar defVar;
		if (node.defExpr)
			defVar = PopTmpVar();

		StdVector<ILVar> unorderedArgs;
		bool namedArgs = false;
		for (usize i = 0; i < node.args.size(); ++i)
		{
			if (i == 0)
				namedArgs = !!node.args[i]->iden;

			unorderedArgs.push_back(PopTmpVar());
		}
		std::reverse(unorderedArgs.begin(), unorderedArgs.end());

		IdenType& idenType = node.sym->type.AsIden();
		SymbolSPtr structSym = idenType.sym.lock();

		StdVector<ILVar> args;
		if (namedArgs)
		{
			args.resize(structSym->orderedVarChildren.size());

			StdVector<SymbolSPtr> children;
			structSym->children->Foreach([&children](SymbolSPtr sym, QualNameSPtr iface)
			{
				if (!iface && sym->kind == SymbolKind::Var)
					children.push_back(sym);
			});
			std::sort(children.begin(), children.end(), [](const SymbolSPtr& child0, const SymbolSPtr& child1)
			{
				return child0->offset < child1->offset;
			});

			for (usize i = 0; i < unorderedArgs.size(); ++i)
			{
				const StdString& name = node.args[i]->iden->Name();
				usize idx = 0;
				while (children[i]->qualName->LastIden()->Name() != name)
					++idx;
				args[idx] = unorderedArgs[i];
			}

			if (unorderedArgs.size() < structSym->orderedVarChildren.size())
			{
				if (!node.defExpr)
				{
					defVar = CreateDstVar(node.typeInfo.handle);

					// TODO: Gen Default
				}

				for (usize i = 0; i < structSym->orderedVarChildren.size(); ++i)
				{
					if (args[i].kind == ILVarKind::Lit ||
						args[i].type.IsValid())
						continue;

					SymbolSPtr child = structSym->orderedVarChildren[i].lock();
					ILVar tmpDst = CreateDstVar(child->type);
					AddElem(new ILMemberAccess{ tmpDst, defVar, child->qualName->LastIden()->Name() });

					args[i] = tmpDst;
				}
			}
		}
		else
		{
			args = unorderedArgs;
			if (args.size() < structSym->orderedVarChildren.size())
			{
				if (!node.defExpr)
				{
					defVar = CreateDstVar(node.typeInfo.handle);

					// TODO: Gen Default
				}

				for (usize i = args.size(); i < structSym->orderedVarChildren.size(); ++i)
				{
					SymbolSPtr child = structSym->orderedVarChildren[i].lock();
					ILVar tmpDst = CreateDstVar(child->type);
					AddElem(new ILMemberAccess{ tmpDst, defVar, child->qualName->LastIden()->Name() });

					args.push_back(tmpDst);
				}
			}
		}

		ILVar tmpStruct = CreateDstVar(structSym->type);
		AddElem(new ILStructInit{ tmpStruct, args });
		tmpStruct = PopTmpVar();

		ILVar dst = CreateDstVar(node.typeInfo.handle);
		AddElem(new ILAdtEnumInit{ dst, node.sym->qualName->LastIden()->Name(), { tmpStruct } });
	}

	void ILGen::Visit(ITrTupleInit& node)
	{
		Walk(node);

		StdVector<ILVar> args;
		for (usize i = 0; i < node.exprs.size(); ++i)
		{
			args.push_back(PopTmpVar());
		}
		std::reverse(args.begin(), args.end());

		ILVar dst = CreateDstVar(node.typeInfo.handle);
		
		AddElem(new ILTupInit{ dst, args });
	}

	void ILGen::Visit(ITrArrayInit& node)
	{
		Walk(node);
		
		StdVector<ILVar> args;
		for (usize i = 0; i < node.exprs.size(); ++i)
			args.push_back(PopTmpVar());
		std::reverse(args.begin(), args.end());

		ILVar dst = CreateDstVar(node.typeInfo.handle);
		
		AddElem(new ILArrInit{ dst, args });
	}

	void ILGen::Visit(ITrCast& node)
	{
		Walk(node);

		ILVar src = PopTmpVar();
		ILVar dst = CreateDstVar(node.typeInfo.handle);

		if (node.castKind == ITrCastKind::Transmute)
		{
			AddElem(new ILTransmute{ dst, src });
		}
		else if (node.operator_.isBuiltin)
		{
			AddElem(new ILPrimCast{ dst, src });
		}
		else
		{
			AddElem(new ILFuncCall{ dst, node.operator_.sym->qualName, { src } });
		}
	}

	void ILGen::Visit(ITrBlockExpr& node)
	{
		// TODO
	}

	u32 ILGen::AddNewBlock()
	{
		m_Def->blocks.push_back(ILBlock{ m_CurLabel++ });
		m_pCurBlock = &m_Def->blocks.back();
		return m_Def->blocks.back().label;
	}

	void ILGen::SetCurBlock(u32 label)
	{
		m_pCurBlock = &m_Def->blocks[label];
	}

	void ILGen::MapVar(IdenSPtr iden, ILVar var)
	{
		usize curDepth = m_ScopeNames.size();

		auto it = m_VarMapping.find(iden);
		if (it == m_VarMapping.end())
		{
			it = m_VarMapping.try_emplace(iden, StdVector<ILVar>{}).first;
		}
		it->second.resize(curDepth + 1);
		it->second[curDepth] = var;
		
	}

	ILVar ILGen::CreateDstVar(TypeHandle type, bool addToTmp)
	{
		ILVar dst{ ILVarKind::Copy, m_CurVarId++, type };
		m_Def->tmpVars.push_back(dst);

		if (addToTmp)
		{
			ILVar movDst = dst;
			movDst.kind = ILVarKind::Move;
			m_TmpVars.push(movDst);
		}
		
		m_pILMod->types.insert(dst.type.Type());
		
		return dst;
	}

	ILVar ILGen::PopTmpVar()
	{
		ILVar tmp = m_TmpVars.top();
		m_TmpVars.pop();
		return tmp;
	}

	QualNameSPtr ILGen::GetCurScope()
	{
		QualNameSPtr qualname = m_FuncScope;
		for (StdString& scopeName : m_ScopeNames)
		{
			qualname = QualName::Create(qualname, scopeName);
		}
		return qualname;
	}

	void ILGen::ImplementCompilerIntrin(const StdString& intrinName)
	{
		static StdUnorderedMap<StdString, std::function<void()>> implMapping;

		if (implMapping.empty())
		{
			implMapping.try_emplace("sizeof", [this]()
			{
				ILVar dst = CreateDstVar(m_pCtx->typeReg.Builtin(TypeMod::None, BuiltinTypeKind::USize));
				AddElem(new ILCompIntrin{ dst, ILCompIntrinKind::SizeOf, {}, { m_FuncScope->LastIden()->Generics()[0].type } });
				SetTerminal(new ILReturn{ PopTmpVar() });
			});
			implMapping.try_emplace("alignof", [this]()
			{
				ILVar dst = CreateDstVar(m_pCtx->typeReg.Builtin(TypeMod::None, BuiltinTypeKind::U16));
				AddElem(new ILCompIntrin{ dst, ILCompIntrinKind::AlignOf, {}, { m_FuncScope->LastIden()->Generics()[0].type } });
				SetTerminal(new ILReturn{ PopTmpVar() });
			});
			implMapping.try_emplace("alignofval", [this]()
			{
				ILVar dst = CreateDstVar(m_pCtx->typeReg.Builtin(TypeMod::None, BuiltinTypeKind::U16));
				ILVar src = m_Def->params[0];
				AddElem(new ILCompIntrin{ dst, ILCompIntrinKind::AlignOfVal, { src }, {} });
				SetTerminal(new ILReturn{ PopTmpVar() });
			});
		}

		auto it = implMapping.find(intrinName);
		if (it != implMapping.end())
			it->second();
	}

	ILVar ILGen::CreateLitVar(Token& lit)
	{
		switch (lit.Type())
		{
		case TokenType::False:
			return { false };
		case TokenType::Null:
			return { ILLitType::Null, StdVector<u8>{} };
		case TokenType::True:
			return { true };
		case TokenType::CharLit:
		{
			// TODO: UTF-8
			return { ILLitType::Char, lit.Unsigned() };
		}
		case TokenType::F16Lit:
			// TODO
			return { ILLitType::F32, 0.0 };
		case TokenType::F32Lit:
			return { ILLitType::F32, lit.Fp() };
		case TokenType::F64Lit:
			return { ILLitType::F64, lit.Fp() };
		case TokenType::F128Lit:
			// TODO
			return { ILLitType::F64, 0.0 };
		case TokenType::I8Lit:
			return { ILLitType::I8, lit.Signed() };
		case TokenType::I16Lit:
			return { ILLitType::I16, lit.Signed() };
		case TokenType::I32Lit:
			return { ILLitType::I32, lit.Signed() };
		case TokenType::I64Lit:
			return { ILLitType::I64, lit.Signed() };
		case TokenType::I128Lit:
			// TODO
			return { ILLitType::I64, 0ll };
		case TokenType::StringLit:
		{
			const StdString& text = lit.Text();
			StdVector<u8> data{ text.begin() + 1, text.end() - 1 }; // trim begin and end "
			data.push_back(0);
			return { ILLitType::String, data };
		}
		case TokenType::U8Lit:
			return { ILLitType::U8, lit.Unsigned() };
		case TokenType::U16Lit:
			return { ILLitType::U16, lit.Unsigned() };
		case TokenType::U32Lit:
			return { ILLitType::U32, lit.Unsigned() };
		case TokenType::U64Lit:
			return { ILLitType::U64, lit.Unsigned() };
		case TokenType::U128Lit:
			// TODO
			return { ILLitType::U64, 0ull };
		default:
			assert(false);
			return { false };
		}
	}

	template <typename T>
	Noctis::ILVar Noctis::ILGen::CreateLitVar(TypeHandle type, const T& val)
	{
		if (type.Kind() != TypeKind::Builtin)
			return ILVar{ ILLitType::Null, 0ull };

		switch (type.AsBuiltin().builtin)
		{
		case BuiltinTypeKind::Bool: return ILVar{ bool(val) };
		case BuiltinTypeKind::Char: return ILVar{ ILLitType::Char, u64(val) };
		case BuiltinTypeKind::I8: return ILVar{ ILLitType::I8, i64(val) };
		case BuiltinTypeKind::I16: return ILVar{ ILLitType::I16, i64(val) };
		case BuiltinTypeKind::I32: return ILVar{ ILLitType::I32, i64(val) };
		case BuiltinTypeKind::I64: return ILVar{ ILLitType::I64, i64(val) };
		case BuiltinTypeKind::I128: return ILVar{ ILLitType::I128, i64(val) };
		case BuiltinTypeKind::ISize: return ILVar{ ILLitType::I64, i64(val) };
		case BuiltinTypeKind::U8: return ILVar{ ILLitType::U8, u64(val) };
		case BuiltinTypeKind::U16: return ILVar{ ILLitType::U16, u64(val) };
		case BuiltinTypeKind::U32: return ILVar{ ILLitType::U32, u64(val) };
		case BuiltinTypeKind::U64: return ILVar{ ILLitType::U64, u64(val) };
		case BuiltinTypeKind::U128: return ILVar{ ILLitType::U128, u64(val) };
		case BuiltinTypeKind::USize: return ILVar{ ILLitType::U64, u64(val) };
		//case BuiltinTypeKind::F16: return ILVar{ ILLitType::F16, f64(val) };
		case BuiltinTypeKind::F32: return ILVar{ ILLitType::F32, f64(val) };
		case BuiltinTypeKind::F64: return ILVar{ ILLitType::F64, f64(val) };
		//case BuiltinTypeKind::F128: return ILVar{ ILLitType::F128, f64(val) };
		default: return ILVar{ ILLitType::Null, 0ull };
		}
	}

	void ILGen::ProcessSwitchGroup(ITrSwitch& node, ITrSwitchGroup& group)
	{
		switch (group.kind)
		{
		case ITrSwitchGroupKind::Base: ProcessSwitchBase(node, group); break;
		case ITrSwitchGroupKind::Leaf: ProcessSwitchLeaf(node, group); break;
		case ITrSwitchGroupKind::Range: ProcessRange(node, group); break;
		case ITrSwitchGroupKind::LitMatch: ProcessLitMatch(node, group); break;
		case ITrSwitchGroupKind::EnumMatch: ProcessEnumMatch(node, group); break;
		case ITrSwitchGroupKind::Tuple: ProcessTuple(node, group); break;
		case ITrSwitchGroupKind::TupleIndex: ProcessTupleIndex(node, group); break;
		case ITrSwitchGroupKind::Aggr: ProcessAggr(node, group); break;
		case ITrSwitchGroupKind::AggrMember: ProcessAggrMember(node, group); break;
		case ITrSwitchGroupKind::Slice: ProcessSlice(node, group); break;
		case ITrSwitchGroupKind::SliceIndex: ProcessSliceIndex(node, group); break;
		default: ;
		}

		if (!group.bindName.empty())
			ValueBindSwitchVar(group.bindName, group.cases);
	}

	void ILGen::ProcessSwitchBase(ITrSwitch& node, ITrSwitchGroup& group)
	{
		ProcessSwitchGroup(node, group.subGroups[0]);
		m_CaseToTermBlocks.push_back(m_pCurBlock->label);
	}

	void ILGen::ProcessSwitchLeaf(ITrSwitch& node, ITrSwitchGroup& group)
	{
		u32 curId = m_pCurBlock->label;
		if (group.cases.size() == 1)
		{
			ITrSwitchCase& case_ = node.cases[group.cases[0]];
			u32 caseId = AddCaseToBody(group.cases[0]);

			if (case_.expr)
			{
				u32 termId = AddCaseToTerm();
				ITrVisitor::Visit(case_.expr);
				ILVar cond = PopTmpVar();
				SetTerminal(new ILIf{ cond, caseId, termId });
			}
			else
			{
				SetTerminal(new ILGoto{ caseId });
			}
		}
		else
		{
			usize finalCaseIdx = usize(-1);
			for (usize caseIdx : group.cases)
			{
				ITrSwitchCase& case_ = node.cases[caseIdx];
				if (!case_.expr)
				{
					finalCaseIdx = caseIdx;
					continue;
				}

				u32 caseId = AddCaseToBody(caseIdx);
				u32 termId = AddNewBlock();

				SetCurBlock(curId);
				ITrVisitor::Visit(case_.expr);
				ILVar cond = PopTmpVar();
				SetTerminal(new ILIf{ cond, caseId, termId });
			}

			if (finalCaseIdx != usize(-1))
			{
				u32 caseId = AddCaseToBody(finalCaseIdx);
				SetTerminal(new ILGoto{ caseId });
			}
			else
			{
				m_CaseToTermBlocks.push_back(curId);
			}
		}
		SetCurBlock(curId);
	}

	void ILGen::ProcessRange(ITrSwitch& node, ITrSwitchGroup& group)
	{
		u32 curId = m_pCurBlock->label;
		u32 lowerId = AddNewBlock();
		u32 trueId = AddNewBlock();
		u32 falseId = AddNewBlock();

		TypeHandle boolType = m_pCtx->typeReg.Builtin(TypeMod::None, BuiltinTypeKind::Bool);

		SetCurBlock(curId);
		ILVar lowerBound = CreateLitVar(m_SwitchVars.top().type, group.valOrFrom);
		ILVar lowerCmp = CreateDstVar(boolType, false);
		AddElem(new ILPrimBinary{ OperatorKind::Ge, lowerCmp, m_SwitchVars.top(), lowerBound});
		SetTerminal(new ILIf{ lowerCmp, lowerId, falseId });

		SetCurBlock(lowerId);
		ILVar upperBound = CreateLitVar(m_SwitchVars.top().type, group.to);
		ILVar upperCmp = CreateDstVar(boolType, false);
		AddElem(new ILPrimBinary{ OperatorKind::Le, upperCmp, m_SwitchVars.top(), upperBound });
		SetTerminal(new ILIf{ upperCmp, trueId, falseId });

		SetCurBlock(trueId);
		ProcessSwitchGroup(node, group.subGroups[0]);

		SetCurBlock(falseId);
	}

	void ILGen::ProcessLitMatch(ITrSwitch& node, ITrSwitchGroup& group)
	{
		u32 curId = m_pCurBlock->label;
		u32 falseId = AddNewBlock();
		u32 defId = falseId;

		StdPairVector<ILVar, u32> cases;
		for (ITrSwitchGroup& subGroup : group.subGroups)
		{
			u32 id = AddNewBlock();
			ILVar lit = CreateLitVar(m_SwitchVars.top().type, subGroup.valOrFrom);

			if (subGroup.isDefCase)
			{
				SetTerminal(new ILUnreachable{}, defId);
				SetCurBlock(id);
				defId = id;
			}
			else
			{
				cases.emplace_back(lit, id);
			}
			

			ProcessSwitchGroup(node, subGroup);
		}
		
		SetTerminal(new ILSwitch{ m_SwitchVars.top(), cases, defId }, curId);

		SetCurBlock(falseId);
	}

	void ILGen::ProcessEnumMatch(ITrSwitch& node, ITrSwitchGroup& group)
	{
		u32 curId = m_pCurBlock->label;
		u32 falseId = AddNewBlock();

		SetCurBlock(curId);

		StdPairVector<ILVar, u32> cases;
		for (ITrSwitchGroup& subGroup : group.subGroups)
		{
			u32 id = AddCaseToBody(subGroup.cases[0]);
			ILVar lit = CreateDstVar(m_SwitchVars.top().type, false);
			AddElem(new ILValEnumInit{ lit, subGroup.member });
			cases.emplace_back(lit, id);
		}

		SetTerminal(new ILSwitch{ m_SwitchVars.top(), cases, falseId });

		SetCurBlock(falseId);
	}

	void ILGen::ProcessTuple(ITrSwitch& node, ITrSwitchGroup& group)
	{
		ILVar var = m_SwitchVars.top();
		m_SwitchVars.push(var);
		for (ITrSwitchGroup& subGroup : group.subGroups)
		{
			ProcessSwitchGroup(node, subGroup);
		}
		PopSwitchVarToDepth(group.depth);
	}

	void ILGen::ProcessTupleIndex(ITrSwitch& node, ITrSwitchGroup& group)
	{
		TypeHandle type = m_SwitchVars.top().type;
		if (type.Kind() == TypeKind::Ref)
			type = type.AsRef().subType;
		if (type.Kind() == TypeKind::Iden)
			type = type.AsIden().sym.lock()->type;
		
		TupleType& tupType = type.AsTuple();
		TypeHandle subType = tupType.subTypes[group.idx];
		
		ILVar elem = CreateDstVar(subType, false);

		m_pCurBlock->elems.emplace_back(new ILTupleAccess{ elem, m_SwitchVars.top(), u16(group.idx) });
		m_SwitchVars.push(elem);

		for (ITrSwitchGroup& subGroup : group.subGroups)
		{
			ProcessSwitchGroup(node, subGroup);
		}
		PopSwitchVarToDepth(group.depth);
	}

	void ILGen::ProcessAggr(ITrSwitch& node, ITrSwitchGroup& group)
	{
		ILVar var = m_SwitchVars.top();
		m_SwitchVars.push(var);
		for (ITrSwitchGroup& subGroup : group.subGroups)
		{
			ProcessSwitchGroup(node, subGroup);
		}
		PopSwitchVarToDepth(group.depth);
	}

	void ILGen::ProcessAggrMember(ITrSwitch& node, ITrSwitchGroup& group)
	{
		TypeHandle type = m_SwitchVars.top().type;
		if (type.Kind() == TypeKind::Ref)
			type = type.AsRef().subType;
		
		SymbolSPtr aggrSym = type.AsIden().sym.lock();
		TypeHandle subType = aggrSym->children->FindChild(nullptr, Iden::Create(group.member))->type;

		ILVar elem = CreateDstVar(subType, false);

		AddElem(new ILMemberAccess{ elem, m_SwitchVars.top(), group.member });
		m_SwitchVars.push(elem);

		for (ITrSwitchGroup& subGroup : group.subGroups)
		{
			ProcessSwitchGroup(node, subGroup);
		}
		PopSwitchVarToDepth(group.depth);
	}

	void ILGen::ProcessSlice(ITrSwitch& node, ITrSwitchGroup& group)
	{
		ILVar var = m_SwitchVars.top();
		m_SwitchVars.push(var);
		for (ITrSwitchGroup& subGroup : group.subGroups)
		{
			ProcessSwitchGroup(node, subGroup);
		}
		PopSwitchVarToDepth(group.depth);
	}

	void ILGen::ProcessSliceIndex(ITrSwitch& node, ITrSwitchGroup& group)
	{
		TypeHandle type = m_SwitchVars.top().type;
		if (type.Kind() == TypeKind::Ref)
			type = type.AsRef().subType;
		
		TypeHandle subType;
		if (type.Kind() == TypeKind::Array)
			subType = type.AsArray().subType;
		else
			subType = type.AsSlice().subType;

		if (group.indexFromBack)
		{
			// TODO: index from back
		}
		else
		{
			ILVar elem = CreateDstVar(subType, false);
			ILVar idx = CreateLitVar(m_pCtx->typeReg.Builtin(TypeMod::None, BuiltinTypeKind::USize), group.idx);
			AddElem(new ILIndex{ elem, m_SwitchVars.top(), idx });
			m_SwitchVars.push(elem);
		}

		for (ITrSwitchGroup& subGroup : group.subGroups)
		{
			ProcessSwitchGroup(node, subGroup);
		}
		PopSwitchVarToDepth(group.depth);
	}

	void ILGen::ValueBindSwitchVar(const StdString& bindName, const StdVector<usize>& caseIds)
	{
		if (bindName.empty())
			return;

		for (usize caseId : caseIds)
		{
			auto it = m_CaseBindings.find(caseId);
			if (it == m_CaseBindings.end())
				it = m_CaseBindings.try_emplace(caseId, StdPairVector<StdString, ILVar>{}).first;

			it->second.emplace_back(bindName, m_SwitchVars.top());
		}
	}

	void ILGen::AssignValueBinds(usize caseId)
	{
		auto it = m_CaseBindings.find(caseId);
		if (it == m_CaseBindings.end())
			return;

		for (StdPair<StdString, ILVar>& pair : it->second)
		{
			IdenSPtr iden = Iden::Create(pair.first);
			LocalVarDataSPtr varData{ new LocalVarData{ iden, TypeInfo{ pair.second.type }, false} };
			m_FuncCtx->localVars.AddLocalVarDeclSPtr(m_ScopeNames, varData);
			ILVar dst{ ILVarKind::Copy, m_CurVarId++, pair.second.type };
			varData->ilVar = dst;
			MapVar(iden, dst);
			AddElem(new ILAssign{ dst, pair.second });
		}
	}

	void ILGen::PopSwitchVarToDepth(usize depth)
	{
		for (usize i = m_SwitchVars.size(); i > depth; --i)
			m_SwitchVars.pop();
	}

	u32 ILGen::AddCaseToBody(usize caseId)
	{
		auto it = std::find_if(m_CaseToBodyBlocks.begin(), m_CaseToBodyBlocks.end(), [caseId](const StdPair<usize, StdVector<u32>>& pair)
		{
			return pair.first == caseId;
		});
		if (it == m_CaseToBodyBlocks.end())
		{
			m_CaseToBodyBlocks.emplace_back(caseId, StdVector<u32>{});
			it = m_CaseToBodyBlocks.end() - 1;
		}

		u32 curBlock = m_pCurBlock->label;
		u32 newBlock = AddNewBlock();
		SetCurBlock(curBlock);

		it->second.push_back(newBlock);
		return newBlock;
	}

	u32 ILGen::AddCaseToTerm()
	{
		u32 curId = m_pCurBlock->label;
		u32 termId = AddNewBlock();
		m_CaseToTermBlocks.push_back(termId);
		SetCurBlock(curId);
		return termId;
	}
}
