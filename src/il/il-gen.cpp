#include "il-gen.hpp"

#include "il.hpp"
#include "itr/itr.hpp"
#include "common/context.hpp"
#include "common/utils.hpp"
#include "module/module.hpp"
#include "module/function.hpp"
#include <algorithm>

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
	
	ILGen::ILGen()
		: ITrSemanticPass("il gen")
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

		m_pILMod = &g_Ctx.activeModule->ilMod;

		Foreach(ITrVisitorDefKind::Any, [&, this](ITrFunc& node)
		{
			if (node.funcKind == ITrFuncKind::EmptyMethod)
				return;

			Reset();

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
			m_Def.reset(new ILFuncDef{ qualName, std::move(generics) });
			m_Def->sym = node.sym;
			m_Def->genMapping = node.genMapping;
			m_FuncScope = node.qualName;
			m_FuncCtx = node.ctx;
			m_FuncCtx->localVars.ResetActiveVars();
			m_ErrType = node.errorType ? node.errorType->handle : TypeHandle{};

			// params
			for (ITrParamSPtr param : node.params)
			{
				u32 id = m_CurVarId++;
				ILVar var{ ILVarKind::Copy, id, param->type->handle };

				m_Def->params.push_back(var);
				m_pILMod->types.insert(var.type.Type());

				LocalVarDataSPtr varData = m_FuncCtx->localVars.ActivateNextVar(m_ScopeNames, param->iden);
				varData->ilVar = var;
			}

			if (node.retType)
			{
				m_Def->retType = node.retType->handle;
				m_pILMod->types.insert(m_Def->retType.Type());
			}

			m_FuncCtx->localVars.Foreach([this](LocalVarDataSPtr varData)
			{
				if (varData->isParam)
					return;
				
				u32 id = m_CurVarId++;
				ILVar var{ ILVarKind::Copy, id, varData->type };
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

					if (atAttrib->iden == "compintrin")
					{
						StdString intrinName = static_cast<ITrLiteral&>(*atAttrib->args[0]->expr).lit.iden;
						intrinName.erase(intrinName.begin());
						intrinName.erase(intrinName.end() - 1);
						ImplementCompilerIntrin(intrinName);
					}
				}
			}

			if (!m_pCurBlock->terminal)
				SetTerminal(new ILReturn{});

			g_Ctx.activeModule->ilMod.funcs.push_back(m_Def);
		});

		Foreach(ITrVisitorDefKind::Any, [&, this](ITrErrHandler& node)
		{
			Reset();
			m_CurVarId = 1;

			QualNameSPtr qualName = node.sym.lock()->qualName;
			m_Def.reset(new ILFuncDef{ qualName, {} });
			m_Def->sym = node.sym;
			m_Def->genMapping = node.genMapping;
			m_FuncScope = node.qualName;

			// TODO: ScopeName
			//m_ScopeNames.push_back(node.)

			LocalVarDataSPtr errValData = m_FuncCtx->localVars.ActivateNextVar(m_ScopeNames, node.errIden);
			
			ILVar param{ ILVarKind::Copy, 0, node.errType->handle };
			errValData->ilVar = param;
			m_Def->params.emplace_back(param);
			m_CurVarId = 1;

			m_FuncCtx = node.ctx;
			m_Def->retType = m_ErrType = node.retType;

			m_FuncCtx->localVars.Foreach([this](LocalVarDataSPtr varData)
			{
				if (varData->isParam)
					return;

				u32 id = m_CurVarId++;
				ILVar var{ ILVarKind::Copy, id, varData->type };
				varData->ilVar = var;
				m_Def->localVars.push_back(var);

				m_pILMod->types.insert(var.type.Type());
			});

			AddNewBlock();
			Walk(node);

			g_Ctx.activeModule->ilMod.funcs.push_back(m_Def);
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
		
		SetCurBlock(loopId);
		Walk(node);
		
		m_LoopEndLabels.pop();
		m_LoopBeginLabels.pop();

		SetTerminal(new ILGoto{ endId });
		SetCurBlock(endId);
	}

	void ILGen::Visit(ITrForRange& node)
	{
		m_ScopeNames.push_back(node.scopeName);
		
		u32 curId = m_pCurBlock->label;
		u32 loopStartId = AddNewBlock();
		u32 loopId  = AddNewBlock();
		u32 endId = AddNewBlock();

		SetCurBlock(curId);

		SymbolSPtr typeSym = g_Ctx.activeModule->symTable.Find(node.range->handle);
		QualNameSPtr toItQualName = QualName::Create({ "core", "iter", "ToIterator" });
		SymbolSPtr itSym = typeSym->children->FindChild(toItQualName, "Iter");
		TypeDisambiguationSPtr toItisambig = TypeDisambiguation::Create(itSym->type, toItQualName);
		QualNameSPtr toItFuncName = QualName::Create(toItisambig);
		toItFuncName = toItFuncName->Append("ToIterator");

		ITrVisitor::Visit(node.range);
		ILVar range = PopTmpVar();
		ILVar itVar = CreateDstVar(itSym->type);
		AddElem(new ILStaticCall{ itVar, toItFuncName, {} });
		
		SetTerminal(new ILGoto{ loopStartId });
		SetCurBlock(loopStartId);

		QualNameSPtr itQualName = QualName::Create({ "core", "iter", "Iterator" });
		TypeDisambiguationSPtr nextDisambig = TypeDisambiguation::Create(itVar.type, itQualName);
		QualNameSPtr nextFuncName = QualName::Create(nextDisambig)->Append("Next");
		
		ILVar nextVar = CreateDstVar(itSym->type);
		AddElem(new ILStaticCall{ nextVar, nextFuncName, {} });

		ILVar nullLit{ ILLitType::Null };
		ILVar cmpRes = CreateDstVar(g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::Bool));
		AddElem(new ILPrimBinary{ OperatorKind::Eq, cmpRes, nextVar, nullLit });
		
		SetTerminal(new ILIf{ cmpRes, loopId, endId });
		SetCurBlock(loopId);

		if (node.idens.size() == 1)
		{
			LocalVarDataSPtr localVar = m_FuncCtx->localVars.ActivateNextVar(m_ScopeNames, node.idens[0]);

			AddElem(new ILAssign{ localVar->ilVar, nextVar });
		}
		else
		{
			for (usize i = 0; i < node.idens.size(); ++i)
			{
				LocalVarDataSPtr localVar = m_FuncCtx->localVars.ActivateNextVar(m_ScopeNames, node.idens[i]);

				ILVar elem = CreateDstVar(localVar->type);
				AddElem(new ILTupleAccess{ elem, nextVar, u16(i) });
				AddElem(new ILAssign{ localVar->ilVar, nextVar });
			}
		}

		Visit(*node.body);
		SetTerminal(new ILGoto{ loopId });
		
		m_ScopeNames.pop_back();

		SetCurBlock(endId);
	}

	void ILGen::Visit(ITrSwitch& node)
	{
		ITrVisitor::Visit(node.expr);
		ILVar cond = PopTmpVar();
		m_SwitchVars.push(cond);
		m_ScopeNames.push_back(node.scopeName);

		SaveRestore caseBindings{ m_CaseBindings };
		SaveRestore caseToBody{ m_CaseToBodyBlocks };
		SaveRestore caseToTerm{ m_CaseToTermBlocks };

		ProcessSwitchGroup(node, node.baseGroup);

		for (StdPair<usize, StdVector<u32>> pair : m_CaseToBodyBlocks)
		{
			ITrSwitchCase& case_ = node.cases[pair.first];
			u32 caseId = AddNewBlock();

			m_ScopeNames.push_back(case_.block->scopeName);
			AssignValueBinds(pair.first);
			for (ITrStmtSPtr stmt : case_.block->stmts)
			{
				ITrVisitor::Visit(stmt);
			}
			m_CaseToTermBlocks.push_back(m_pCurBlock->label);
			
			for (u32 blockId : pair.second)
			{
				SetTerminal(new ILGoto{ caseId }, blockId);
			}

			m_ScopeNames.pop_back();
		}

		u32 endBlock = AddNewBlock();
		for (u32 blockId : m_CaseToTermBlocks)
		{
			if (!m_Def->blocks[blockId].terminal)
				SetTerminal(new ILGoto{ endBlock }, blockId);
		}

		SetCurBlock(endBlock);
		m_ScopeNames.pop_back();
	}

	void ILGen::Visit(ITrLabel& node)
	{
		AddNewBlock();
		m_LabelMapping.try_emplace(node.label, m_pCurBlock->label);
	}

	void ILGen::Visit(ITrBreak& node)
	{
		ILTerminalSPtr term;
		if (!node.label.empty())
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
		if (!node.label.empty())
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
		Walk(node);

		ILVar errVar = PopTmpVar();
		ILVar resVar = CreateDstVar(m_ErrType);

		AddElem(new ILAdtEnumInit{ resVar, "Error", { errVar } });
		SetTerminal(new ILReturn{ resVar });
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
		for (const StdString& iden : node.idens)
		{
			LocalVarDataSPtr varData = m_FuncCtx->localVars.GetLocalVarData(m_ScopeNames, iden);
			ILVar dst = varData->ilVar;

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

		if (src.type.HasFuzzyCompare() || dst.type.HasFuzzyCompare())
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
		ILVar dst = CreateDstVar(node.handle);

		if (src1.type.HasFuzzyCompare() || src0.type.HasFuzzyCompare())
			AddElem(new ILCompIntrin{ ILVar{}, ILCompIntrinKind::FuzzyTypeComp, {}, {src0.type, src1.type} });

		AddElem(new ILTernary{ dst, cond, src0, src1 });
		m_TmpVars.push(dst);
	}

	void ILGen::Visit(ITrBinary& node)
	{
		Walk(node);
		ILVar right = PopTmpVar();
		ILVar left = PopTmpVar();
		ILVar dst = CreateDstVar(node.handle);

		ILElemSPtr elem;
		if (node.operator_.isBuiltin)
			AddElem(new ILPrimBinary{ node.op, dst, left, right });
		else
			AddElem(new ILStaticCall{ dst, node.operator_.sym->qualName, { right, left } });

		m_TmpVars.push(dst);
	}

	void ILGen::Visit(ITrUnary& node)
	{
		Walk(node);
		ILVar var = PopTmpVar();
		ILVar dst = CreateDstVar(node.handle);

		ILElemSPtr elem;
		if (node.operator_.isBuiltin)
			AddElem(new ILPrimUnary{ node.op, dst, var });
		else
			AddElem(new ILStaticCall{ dst, node.operator_.sym->qualName, { var } });

		m_TmpVars.push(dst);
	}

	void ILGen::Visit(ITrQualNameExpr& node)
	{
		// First look for a local var
		LocalVarDataSPtr local = m_FuncCtx->localVars.GetLocalVarData(m_ScopeNames, node.qualName->LastIden());
		if (local)
		{
			m_TmpVars.push(local->ilVar);
			m_pILMod->types.insert(local->type.Type());
		}
		else
		{
			ILVar dst = CreateDstVar(node.handle);
			if (node.sym->kind == SymbolKind::ValEnumMember)
			{
				AddElem(new ILValEnumInit{ dst, node.qualName->LastIden() });
			}
			else if (node.sym->kind == SymbolKind::AdtEnumMember)
			{
				AddElem(new ILAdtEnumInit{ dst, node.qualName->LastIden(), {} });
			}
			else if (node.sym->kind == SymbolKind::GenVal)
			{
				AddElem(new ILGenVal{ dst, node.qualName->LastIden() });
			}
			else
			{
				// TODO: Global var
			}
			m_TmpVars.push(dst);
		}
	}

	void ILGen::Visit(ITrIndexSlice& node)
	{
		Walk(node);

		ILVar idx = PopTmpVar();
		ILVar src = PopTmpVar();

		ILVar dst = CreateDstVar(node.handle);

		if (node.operator_.isBuiltin)
		{
			AddElem(new ILIndex{ dst, src, idx });
		}
		else
		{
			AddElem(new ILStaticCall{ node.operator_.sym->qualName, { idx, src } });
		}
		
		m_TmpVars.push(dst);
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

			SymbolSPtr methodSym = node.sym;
			SymbolSPtr callerTypeSym = methodSym->parent.lock();

			if (callerTypeSym->kind == SymbolKind::StrongInterface ||
				callerTypeSym->kind == SymbolKind::WeakInterface)
			{
				const StdString& name = node.sym->qualName->LastIden();
				if (node.handle.IsValid())
				{
					ILVar dst = CreateDstVar(node.handle);
					AddElem(new ILDynamicCall{ dst, caller, name, args });
					m_TmpVars.push(dst);
				}
				else
				{
					AddElem(new ILDynamicCall{ caller, name, args });
				}
			}
			else
			{
				args.insert(args.begin(), caller);
				if (node.handle.IsValid())
				{
					ILVar dst = CreateDstVar(node.handle);
					AddElem(new ILStaticCall{ dst, methodSym->qualName, args });
					m_TmpVars.push(dst);
				}
				else
				{
					AddElem(new ILStaticCall{ methodSym->qualName, args });
				}
				
			}
		}
		else
		{
			if (node.sym)
			{
				TypeHandle retType = node.handle;
				if (retType.IsValid())
				{
					ILVar dst = CreateDstVar(node.handle);
					AddElem(new ILStaticCall{ dst, node.sym->qualName, args });
					m_TmpVars.push(dst);
				}
				else
				{
					AddElem(new ILStaticCall{ node.sym->qualName, args });
				}
			}
			else
			{
				ILVar func = PopTmpVar();

				TypeHandle retType = node.handle;
				if (retType.IsValid())
				{
					ILVar dst = CreateDstVar(node.handle);
					AddElem(new ILIndirectCall{ dst, func, args });
					m_TmpVars.push(dst);
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

		m_pILMod->names.insert(node.iden);
		
		ILVar src = PopTmpVar();
		ILVar dst = CreateDstVar(node.handle);

		AddElem(new ILMemberAccess{ dst, src, node.iden });
		m_TmpVars.push(dst);
	}

	void ILGen::Visit(ITrTupleAccess& node)
	{
		Walk(node);
		ILVar src = PopTmpVar();
		ILVar dst = CreateDstVar(node.handle);

		AddElem(new ILTupleAccess{ dst, src, node.index });
		m_TmpVars.push(dst);
	}

	void ILGen::Visit(ITrLiteral& node)
	{
		ILVar var = CreateLitVar(node.lit, node.handle);
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
				namedArgs = !node.args[i]->iden.empty();

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
					defVar = CreateDstVar(node.handle);

					// TODO: Gen Default
				}

				for (usize i = 0; i < node.sym->orderedVarChildren.size(); ++i)
				{
					if (args[i].kind == ILVarKind::Lit ||
						args[i].type.IsValid())
						continue;
					
					SymbolSPtr child = node.sym->orderedVarChildren[i].lock();
					ILVar tmpDst = CreateDstVar(child->type);
					AddElem(new ILMemberAccess{ tmpDst, defVar, child->qualName->LastIden() });

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
					defVar = CreateDstVar(node.handle);

					// TODO: Gen Default
				}
				
				for (usize i = args.size(); i < node.sym->orderedVarChildren.size(); ++i)
				{
					SymbolSPtr child = node.sym->orderedVarChildren[i].lock();
					ILVar tmpDst = CreateDstVar(child->type);
					AddElem(new ILMemberAccess{ tmpDst, defVar, child->qualName->LastIden() });

					args.push_back(tmpDst);
				}
			}
		}

		ILVar dst = CreateDstVar(node.handle);
		
		AddElem(new ILStructInit{ dst, args });
		m_TmpVars.push(dst);
	}

	void ILGen::Visit(ITrUnionInit& node)
	{
		Walk(node);

		ILVar arg = PopTmpVar();
		ILVar dst = CreateDstVar(node.handle);
		AddElem(new ILUnionInit { dst, node.arg->iden, arg });
		m_TmpVars.push(dst);
	}

	void ILGen::Visit(ITrAdtTupleEnumInit& node)
	{
		StdVector<ILVar> args;
		for (ITrArgSPtr arg : node.args)
		{
			ITrVisitor::Visit(arg->expr);
			args.push_back(PopTmpVar());
		}
		
		ILVar dst = CreateDstVar(node.handle);
		AddElem(new ILAdtEnumInit{ dst, node.sym->qualName->LastIden(), args });
		m_TmpVars.push(dst);
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
				namedArgs = !node.args[i]->iden.empty();

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
				const StdString& name = node.args[i]->iden;
				usize idx = 0;
				while (children[i]->qualName->LastIden() != name)
					++idx;
				args[idx] = unorderedArgs[i];
			}

			if (unorderedArgs.size() < structSym->orderedVarChildren.size())
			{
				if (!node.defExpr)
				{
					defVar = CreateDstVar(node.handle);

					// TODO: Gen Default
				}

				for (usize i = 0; i < structSym->orderedVarChildren.size(); ++i)
				{
					if (args[i].kind == ILVarKind::Lit ||
						args[i].type.IsValid())
						continue;

					SymbolSPtr child = structSym->orderedVarChildren[i].lock();
					ILVar tmpDst = CreateDstVar(child->type);
					AddElem(new ILMemberAccess{ tmpDst, defVar, child->qualName->LastIden() });

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
					defVar = CreateDstVar(node.handle);

					// TODO: Gen Default
				}

				for (usize i = args.size(); i < structSym->orderedVarChildren.size(); ++i)
				{
					SymbolSPtr child = structSym->orderedVarChildren[i].lock();
					ILVar tmpDst = CreateDstVar(child->type);
					AddElem(new ILMemberAccess{ tmpDst, defVar, child->qualName->LastIden() });

					args.push_back(tmpDst);
				}
			}
		}

		ILVar tmpStruct = CreateDstVar(structSym->type);
		AddElem(new ILStructInit{ tmpStruct, args });
		tmpStruct = PopTmpVar();

		ILVar dst = CreateDstVar(node.handle);
		AddElem(new ILAdtEnumInit{ dst, node.sym->qualName->LastIden(), { tmpStruct } });
		m_TmpVars.push(dst);
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

		ILVar dst = CreateDstVar(node.handle);
		
		AddElem(new ILTupInit{ dst, args });
		m_TmpVars.push(dst);
	}

	void ILGen::Visit(ITrArrayInit& node)
	{
		Walk(node);
		
		StdVector<ILVar> args;
		for (usize i = 0; i < node.exprs.size(); ++i)
			args.push_back(PopTmpVar());
		std::reverse(args.begin(), args.end());

		ILVar dst = CreateDstVar(node.handle);
		
		AddElem(new ILArrInit{ dst, args });
		m_TmpVars.push(dst);
	}

	void ILGen::Visit(ITrCast& node)
	{
		Walk(node);

		ILVar src = PopTmpVar();
		ILVar dst = CreateDstVar(node.handle);

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
			AddElem(new ILStaticCall{ dst, node.operator_.sym->qualName, { src } });
		}

		m_TmpVars.push(dst);
	}

	void ILGen::Visit(ITrBlockExpr& node)
	{
		// TODO
	}

	void ILGen::Visit(ITrTry& node)
	{
		Walk(node);

		ILVar retVar = PopTmpVar();
		TypeHandle errType = node.expr->handle.AsIden().qualName->Generics().back().type;
		ILVar errVar = CreateDstVar(errType);
		AddElem(new ILValEnumInit{ errVar, "Error" });

		u32 throwLabel = AddNewBlock();
		u32 continueLabel = AddNewBlock();
		SetTerminal(new ILSwitch{ retVar, { {errVar, throwLabel} }, continueLabel });

		if (node.errHandlerName)
		{
			ILVar handlerErr = CreateDstVar(errType);
			AddElem(new ILMemberAccess{ handlerErr, errVar, "Result" }, throwLabel);

			ILVar handlerRes = CreateDstVar(m_ErrType);
			AddElem(new ILStaticCall{ handlerRes, node.errHandlerName, { handlerErr } }, throwLabel);

			retVar = handlerRes;
		}

		switch (node.kind)
		{
		case ITrTryKind::Propagating:
		{
			SetTerminal(new ILReturn{ retVar });
		}
		case ITrTryKind::Nullable:
		{
			retVar = CreateDstVar(node.handle);
			AddElem(new ILAssign{ retVar, ILVar{ ILLitType::Null } });
			SetTerminal(new ILGoto{ continueLabel });
		}
		case ITrTryKind::Panic:
		{
			// TODO: Panic
			SetTerminal(new ILReturn{});
		}
		default: ;
		}
		
		m_TmpVars.push(retVar);
	}

	void ILGen::Visit(ITrAttribs& node)
	{
		// Don't do anything
	}

	void ILGen::Reset()
	{
		m_CurLabel = 0;
		m_CurDeferLabel = 0;
		m_CurVarId = 0;

		while (!m_TmpVars.empty())
		{
			m_TmpVars.pop();
		}

		while (!m_SwitchVars.empty())
		{
			m_SwitchVars.pop();
		}
		m_CurSwitchPatternDepth = 0;

		m_CaseToBodyBlocks.clear();
		m_CaseToTermBlocks.clear();
		m_CaseBindings.clear();
		
		m_LabelMapping.clear();
		m_ITrBlockMapping.clear();
		
		while (!m_LabelStack.empty())
		{
			m_LabelStack.pop();
		}
		while (!m_LoopBeginLabels.empty())
		{
			m_LoopBeginLabels.pop();
		}
		while (!m_LoopEndLabels.empty())
		{
			m_LoopEndLabels.pop();
		}

		m_FallThrough = false;
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

	ILVar ILGen::CreateDstVar(TypeHandle type)
	{
		ILVar dst{ ILVarKind::Copy, m_CurVarId++, type };
		m_Def->tmpVars.push_back(dst);
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
			qualname = qualname->Append(scopeName);
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
				ILVar dst = CreateDstVar(g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::USize));
				AddElem(new ILCompIntrin{ dst, ILCompIntrinKind::SizeOf, {}, { m_FuncScope->Generics()[0].type } });
				SetTerminal(new ILReturn{ dst });
			});
			implMapping.try_emplace("sizeofval", [this]()
			{
				ILVar dst = CreateDstVar(g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::USize));
				AddElem(new ILCompIntrin{ dst, ILCompIntrinKind::SizeOf, {}, { m_FuncScope->Generics()[0].type } });
				SetTerminal(new ILReturn{ dst });
			});
			implMapping.try_emplace("alignof", [this]()
			{
				ILVar dst = CreateDstVar(g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::U16));
				AddElem(new ILCompIntrin{ dst, ILCompIntrinKind::AlignOf, {}, { m_FuncScope->Generics()[0].type } });
				SetTerminal(new ILReturn{ dst });
			});
			implMapping.try_emplace("alignofval", [this]()
			{
				ILVar dst = CreateDstVar(g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::U16));
				ILVar src = m_Def->params[0];
				AddElem(new ILCompIntrin{ dst, ILCompIntrinKind::AlignOfVal, { src }, {} });
				SetTerminal(new ILReturn{ dst });
			});
			implMapping.try_emplace("log2alignof", [this]()
			{
				ILVar dst = CreateDstVar(g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::U16));
				AddElem(new ILCompIntrin{ dst, ILCompIntrinKind::Log2AlignOf, {}, { m_FuncScope->Generics()[0].type } });
				SetTerminal(new ILReturn{ dst });
			});
			implMapping.try_emplace("log2alignofval", [this]()
			{
				ILVar dst = CreateDstVar(g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::U16));
				ILVar src = m_Def->params[0];
				AddElem(new ILCompIntrin{ dst, ILCompIntrinKind::Log2AlignOfVal, { src }, {} });
				SetTerminal(new ILReturn{ dst });
			});
		}

		auto it = implMapping.find(intrinName);
		if (it != implMapping.end())
			it->second();
	}

	ILVar ILGen::CreateLitVar(Token& lit, TypeHandle type)
	{
		ILLitType litType = ILLitType::F64;
		switch (type.AsBuiltin().builtin)
		{
		case BuiltinTypeKind::Bool:  litType = ILLitType::True; break;
		case BuiltinTypeKind::Char:  litType = ILLitType::Char; break;
		case BuiltinTypeKind::I8:    litType = ILLitType::I8;   break;
		case BuiltinTypeKind::I16:   litType = ILLitType::I16;  break;
		case BuiltinTypeKind::I32:   litType = ILLitType::I32;  break;
		case BuiltinTypeKind::I64:   litType = ILLitType::I64;  break;
		case BuiltinTypeKind::I128:  litType = ILLitType::I128; break;
		case BuiltinTypeKind::ISize: litType = ILLitType::I64;  break;
		case BuiltinTypeKind::U8:    litType = ILLitType::U8;   break;
		case BuiltinTypeKind::U16:   litType = ILLitType::U16;  break;
		case BuiltinTypeKind::U32:   litType = ILLitType::U32;  break;
		case BuiltinTypeKind::U64:   litType = ILLitType::U64;  break;
		case BuiltinTypeKind::U128:  litType = ILLitType::U128; break;
		case BuiltinTypeKind::USize: litType = ILLitType::U64;  break;
		//case BuiltinTypeKind::F16:   litType = ILLitType::F16;  break; <- TODO
		case BuiltinTypeKind::F32:   litType = ILLitType::F32;  break;
		case BuiltinTypeKind::F64:   litType = ILLitType::F64;  break;
		//case BuiltinTypeKind::F128:  litType = ILLitType::F128; break; <- TODO
		default: ;
		}
		
		switch (lit.type)
		{
		case TokenType::False:
			return { ILLitType::False };
		case TokenType::Null:
			return { ILLitType::Null };
		case TokenType::True:
			return { ILLitType::True };
		case TokenType::CharLit:
		{
			// TODO: UTF-8
			return { ILLitType::Char, lit.uval };
		}
		case TokenType::F16Lit:
			// TODO
			return { litType, 0.0 };
		case TokenType::F32Lit:
			return { litType, lit.fval };
		case TokenType::F64Lit:
			return { litType, lit.fval };
		case TokenType::F128Lit:
			// TODO
			return { litType, 0.0 };
		case TokenType::I8Lit:
			return { litType, lit.sval };
		case TokenType::I16Lit:
			return { litType, lit.sval };
		case TokenType::I32Lit:
			return { litType, lit.sval };
		case TokenType::I64Lit:
			return { litType, lit.sval };
		case TokenType::I128Lit:
			// TODO
			return { ILLitType::I64, 0ll };
		case TokenType::StringLit:
		{
			const StdString& text = lit.iden;
			StdVector<u8> data{ text.begin() + 1, text.end() - 1 }; // trim begin and end "
			data.push_back(0);
			return { ILLitType::String, data };
		}
		case TokenType::U8Lit:
			return { litType, lit.uval };
		case TokenType::U16Lit:
			return { litType, lit.uval };
		case TokenType::U32Lit:
			return { litType, lit.uval };
		case TokenType::U64Lit:
			return { litType, lit.uval };
		case TokenType::U128Lit:
			// TODO
			return { litType, 0ull };
		default:
			assert(false);
			return { ILLitType::False };
		}
	}

	template <typename T>
	Noctis::ILVar Noctis::ILGen::CreateLitVar(TypeHandle type, const T& val)
	{
		if (type.Kind() != TypeKind::Builtin)
			return ILVar{ ILLitType::Null };

		switch (type.AsBuiltin().builtin)
		{
		case BuiltinTypeKind::Bool: return ILVar{ val ? ILLitType::True : ILLitType::False };
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
		default: return ILVar{ ILLitType::Null };
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

		TypeHandle boolType = g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::Bool);

		SetCurBlock(curId);
		ILVar lowerBound = CreateLitVar(m_SwitchVars.top().type, group.valOrFrom);
		ILVar lowerCmp = CreateDstVar(boolType);
		AddElem(new ILPrimBinary{ OperatorKind::Ge, lowerCmp, m_SwitchVars.top(), lowerBound});
		SetTerminal(new ILIf{ lowerCmp, lowerId, falseId });

		SetCurBlock(lowerId);
		ILVar upperBound = CreateLitVar(m_SwitchVars.top().type, group.to);
		ILVar upperCmp = CreateDstVar(boolType);
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

		TypeHandle enumType = m_SwitchVars.top().type;
		if (enumType.Kind() == TypeKind::Ref)
			enumType = enumType.AsRef().subType;
		
		StdPairVector<ILVar, u32> cases;
		for (ITrSwitchGroup& subGroup : group.subGroups)
		{
			if (subGroup.kind == ITrSwitchGroupKind::Leaf)
			{
				// Def case
				if (subGroup.member.empty())
				{
					SetCurBlock(falseId);
				}
				else
				{
					ILVar lit = CreateDstVar(enumType);
					AddElem(new ILValEnumInit{ lit, subGroup.member });

					u32 id = AddNewBlock();
					cases.emplace_back(lit, id);
				}

				ProcessSwitchGroup(node, subGroup);
			}
			else
			{
				ILVar lit = CreateDstVar(enumType);
				AddElem(new ILValEnumInit{ lit, subGroup.member });
				
				u32 id = AddNewBlock();
				cases.emplace_back(lit, id);

				SymbolSPtr sym = enumType.AsIden().sym.lock();
				SymbolSPtr child = sym->children->FindChild(nullptr, subGroup.member);

				LocalVarDataSPtr localVar = m_FuncCtx->localVars.ActivateNextVar(m_ScopeNames, subGroup.imm);
				// TODO: ADT enum element access
				AddElem(new ILAssign{ localVar->ilVar, m_SwitchVars.top() });
				
				m_SwitchVars.push(localVar->ilVar);
				
				ProcessSwitchGroup(node, subGroup);
				PopSwitchVarToDepth(group.depth);
			}
			SetCurBlock(curId);
		}

		SetTerminal(new ILSwitch{ m_SwitchVars.top(), cases, falseId }, curId);

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
		
		ILVar elem = CreateDstVar(subType);

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
		TypeHandle subType = aggrSym->children->FindChild(nullptr, group.member)->type;

		ILVar elem = CreateDstVar(subType);

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
			ILVar elem = CreateDstVar(subType);
			ILVar idx = CreateLitVar(g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::USize), group.idx);
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
				it = m_CaseBindings.try_emplace(caseId, StdVector<SwitchValBindInfo>{}).first;
			
			it->second.emplace_back(bindName, m_SwitchVars.top(), m_pCurBlock->label);
		}
	}

	void ILGen::AssignValueBinds(usize caseId)
	{
		auto it = m_CaseBindings.find(caseId);
		if (it == m_CaseBindings.end())
			return;

		for (SwitchValBindInfo& bindInfo : it->second)
		{
			LocalVarDataSPtr var = m_FuncCtx->localVars.ActivateNextVar(m_ScopeNames, bindInfo.bindName);
			AddElem(new ILAssign{ var->ilVar, bindInfo.var }, bindInfo.blockId);
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
