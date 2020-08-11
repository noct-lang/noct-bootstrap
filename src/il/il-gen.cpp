#include "il-gen.hpp"

#include "il.hpp"
#include "itr/itr.hpp"
#include "common/context.hpp"
#include "module/module.hpp"
#include "module/function.hpp"

namespace Noctis
{
	ILGen::ILGen(Context* pCtx)
		: ITrSemanticPass("il gen", pCtx)
		, m_pILMod(nullptr)
		, m_pCurBlock(nullptr)
		, m_CurLabel(0)
		, m_CurDeferLabel(0)
		, m_CurVarId(0)
		, m_FallThrough(false)
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
			
			StdString mangleName = node.sym.lock()->mangledName;
			m_Def.reset(new ILFuncDef{ mangleName });
			m_pILMod->names.insert(mangleName);
			m_FuncScope = node.qualName;

			// params
			for (ITrParamSPtr param : node.params)
			{
				u32 id = m_CurVarId++;
				ILVar var{ ILVarKind::Copy, id, param->type->handle };


				auto it = m_VarMapping.try_emplace(param->iden, StdVector<ILVar>{}).first;
				it->second.push_back(var);

				m_Def->params.push_back(var);

				m_pILMod->types.insert(var.type->type);
			}

			if (node.retType)
			{
				m_Def->retType = node.retType->handle;
				m_pILMod->types.insert(m_Def->retType->type);
			}
			
			m_FuncCtx = node.ctx;

			m_FuncCtx->localVars.Foreach([this](LocalVarDataSPtr varData)
			{
				if (varData->isParam)
					return;
				
				u32 id = m_CurVarId++;
				ILVar var{ ILVarKind::Copy, id, varData->type };
				varData->ilVar = var;
				m_Def->localVars.push_back(var);

				m_pILMod->types.insert(var.type->type);
			});

			AddNewBlock();
			Walk(node);

			if (!m_pCurBlock->terminal)
				m_pCurBlock->terminal.reset(new ILReturn{});

			m_pCtx->activeModule->ilMod.funcs.push_back(m_Def);
		});
	}

	void ILGen::Visit(ITrBlock& node)
	{
		u32 oldId = m_pCurBlock->label;
		u32 newId = AddNewBlock();

		SetCurBlock(oldId);
		m_pCurBlock->terminal.reset(new ILGoto{ newId });
		SetCurBlock(newId);
		
		m_ScopeNames.push_back(node.scopeName);
		Walk(node);
		m_ScopeNames.pop_back();
	}

	void ILGen::Visit(ITrIf& node)
	{
		ITrVisitor::Visit(node.cond);
		ILVar cond = PopTmpVar();

		u32 oldBlock = m_pCurBlock->label;
		u32 tBlock = AddNewBlock();
		u32 fBlock = node.fBlock ? AddNewBlock() : 0;
		u32 endBlock = AddNewBlock();
		if (!node.fBlock)
			fBlock = endBlock;

		SetCurBlock(oldBlock);
		ILTerminalSPtr term{ new ILIf{ cond, tBlock, fBlock } };
		m_pCurBlock->terminal = term;

		SetCurBlock(tBlock);
		Visit(*node.tBlock);

		if (node.fBlock)
		{ 
			SetCurBlock(fBlock);
			Visit(*node.fBlock);
		}
		
		SetCurBlock(endBlock);
	}

	void ILGen::Visit(ITrLoop& node)
	{
		u32 startLabel = m_pCurBlock->label;
		u32 loopLabel = AddNewBlock();
		u32 endLabel = AddNewBlock();

		SetCurBlock(startLabel);
		m_pCurBlock->terminal.reset(new ILGoto{ loopLabel });

		m_LoopBeginLabels.push(loopLabel);
		m_LoopEndLabels.push(endLabel);
		m_ScopeNames.push_back(node.scopeName);
		SetCurBlock(loopLabel);
		
		Walk(node);
		
		m_ScopeNames.pop_back();
		m_LoopEndLabels.pop();
		m_LoopBeginLabels.pop();

		m_pCurBlock->terminal.reset(new ILGoto{ endLabel });
		SetCurBlock(endLabel);
	}

	void ILGen::Visit(ITrSwitch& node)
	{
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
			m_pCurBlock->terminal.reset(new ILGoto{ it->second });
		}
		else
		{
			u32 label = m_LoopBeginLabels.top();
			m_pCurBlock->terminal.reset(new ILGoto{ label });
		}
		AddNewBlock();
	}

	void ILGen::Visit(ITrContinue& node)
	{
		ILElemSPtr elem;
		if (node.label)
		{
			auto it = m_LabelMapping.find(node.label);
			m_pCurBlock->terminal.reset(new ILGoto{ it->second });
		}
		else
		{
			u32 label = m_LoopBeginLabels.top();
			m_pCurBlock->terminal.reset(new ILGoto{ label });
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
		m_pCurBlock->terminal.reset(new ILGoto{ it->second });
		AddNewBlock();
	}

	void ILGen::Visit(ITrReturn& node)
	{
		Walk(node);

		ILElemSPtr elem;
		if (node.expr)
		{
			ILVar var = PopTmpVar();
			m_pCurBlock->terminal.reset(new ILReturn{ var });
		}
		else
		{
			m_pCurBlock->terminal.reset(new ILReturn{});
		}
	}

	void ILGen::Visit(ITrThrow& node)
	{
	}

	void ILGen::Visit(ITrDefer& node)
	{
		u32 curLabel = m_pCurBlock->label;

		u32 deferLabel = AddNewBlock();

		// TODO

		// Reset
		SetCurBlock(curLabel);
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
			{
				ILVar src = PopTmpVar();
				m_pCurBlock->elems.push_back(ILElemSPtr{ new ILAssign{ dst, src } });
			}
		}

		
	}

	void ILGen::Visit(ITrAssign& node)
	{
		Walk(node);
		ILVar src = PopTmpVar();
		ILVar dst = PopTmpVar();

		ILElemSPtr elem;
		if (node.op == OperatorKind::Eq)
		{
			elem.reset(new ILAssign{ dst, src });
		}
		else if (node.operator_.isBuiltin)
		{
			elem.reset(new ILPrimAssign{ node.op, dst, src });
		}
		else
		{
			// TODO
		}

		m_TmpVars.push(dst);
		m_pCurBlock->elems.push_back(elem);
	}

	void ILGen::Visit(ITrTernary& node)
	{
		Walk(node);
		ILVar src1 = PopTmpVar();
		ILVar src0 = PopTmpVar();
		ILVar cond = PopTmpVar();
		ILVar dst = CreateDstVar(node.typeHandle);

		ILElemSPtr elem{ new ILTernary{ dst, cond, src0, src1 } };
		m_TmpVars.push(dst);
		m_pCurBlock->elems.push_back(elem);
	}

	void ILGen::Visit(ITrBinary& node)
	{
		Walk(node);
		ILVar right = PopTmpVar();
		ILVar left = PopTmpVar();
		ILVar dst = CreateDstVar(node.typeHandle);

		ILElemSPtr elem;
		if (node.operator_.isBuiltin)
		{
			elem.reset(new ILPrimBinary{ node.op, dst, left, right });
		}
		else
		{
			elem.reset(new ILFuncCall{ dst, node.operator_.sym->mangledName, { right, left } });
		}

		m_pCurBlock->elems.push_back(elem);
	}

	void ILGen::Visit(ITrUnary& node)
	{
		Walk(node);
		ILVar var = PopTmpVar();
		ILVar dst = CreateDstVar(node.typeHandle);

		ILElemSPtr elem;
		if (node.operator_.isBuiltin)
		{
			elem.reset(new ILPrimUnary{ node.op, dst, var });
		}
		else
		{
			elem.reset(new ILFuncCall{ dst, node.operator_.sym->mangledName, { var } });
		}

		m_pCurBlock->elems.push_back(elem);
	}

	void ILGen::Visit(ITrQualNameExpr& node)
	{
		// First look for a local var
		LocalVarDataSPtr local = m_FuncCtx->localVars.GetLocalVarData(m_ScopeNames, node.qualName->Iden());
		if (local)
		{
			ILVar var;
			auto it = m_VarMapping.find(local->iden);
			if (it == m_VarMapping.end())
			{
				var = ILVar{ ILVarKind::Copy, std::numeric_limits<u32>::max(), local->type };
				m_pILMod->types.insert(local->type->type);
			}
			else
			{
				var = it->second.back();
				m_pILMod->types.insert(var.type->type);
			}

			m_TmpVars.push(var);
		}
		else
		{
			SymbolSPtr sym = m_pCtx->activeModule->symTable.Find(GetCurScope(), node.qualName);

			if (sym->kind == SymbolKind::ValEnumMember)
			{
				ILVar dst = CreateDstVar(node.typeHandle);
				ILElemSPtr elem{ new ILValEnumInit{ dst, node.qualName->Iden()->Name() } };
				m_pCurBlock->elems.push_back(elem);
			}
			else if (sym->kind == SymbolKind::AdtEnumMember)
			{
				ILVar dst = CreateDstVar(node.typeHandle);
				ILElemSPtr elem{ new ILAdtEnumInit{ dst, node.qualName->Iden()->Name(), {} } };
				m_pCurBlock->elems.push_back(elem);
			}
			else
			{
				// TODO: Global var
			}
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

		ILElemSPtr elem;
		if (node.isMethod)
		{
			ILVar caller = PopTmpVar();

			const StdString& name = node.sym->qualName->Iden()->ToFuncSymName();

			if (node.typeHandle)
			{
				ILVar dst = CreateDstVar(node.typeHandle);
				elem.reset(new ILMethodCall{ dst, caller, name, args });
			}
			else
			{
				elem.reset(new ILMethodCall{ caller, name, args });
			}
		}
		else
		{
			if (node.sym)
			{
				const StdString& name = node.sym->mangledName;

				TypeHandle retType = node.typeHandle->AsFunc().retType;
				if (retType)
				{
					ILVar dst = CreateDstVar(node.typeHandle);
					elem.reset(new ILFuncCall{ dst, name, args });
				}
				else
				{
					elem.reset(new ILFuncCall{ name, args });
				}
			}
			else
			{
				ILVar func = PopTmpVar();

				TypeHandle retType = node.typeHandle->AsFunc().retType;
				if (retType)
				{
					ILVar dst = CreateDstVar(node.typeHandle);
					elem.reset(new ILIndirectCall{ dst, func, args });
				}
				else
				{
					elem.reset(new ILIndirectCall{ func, args });
				}
			}
		}
		m_pCurBlock->elems.push_back(elem);
		
	}

	void ILGen::Visit(ITrMemberAccess& node)
	{
		Walk(node);

		m_pILMod->names.insert(node.iden->Name());
		
		ILVar src = PopTmpVar();
		ILVar dst = CreateDstVar(node.typeHandle);

		ILElemSPtr elem{ new ILMemberAccess{ dst, src, node.iden->Name() } };
		m_pCurBlock->elems.push_back(elem);
	}

	void ILGen::Visit(ITrTupleAccess& node)
	{
		Walk(node);
		ILVar src = PopTmpVar();
		ILVar dst = CreateDstVar(node.typeHandle);

		ILElemSPtr elem{ new ILTupleAccess{ dst, src, node.index } };
		m_pCurBlock->elems.push_back(elem);
	}

	void ILGen::Visit(ITrLiteral& node)
	{
		switch (node.lit.Type())
		{
		case TokenType::False:
		{
			m_TmpVars.push({ false });
			break;
		}
		case TokenType::Null:
		{
			m_TmpVars.push({ ILLitType::Null, {} });
			break;
		}
		case TokenType::True:
		{
			m_TmpVars.push({ true });
			break;
		}
		case TokenType::CharLit:
		{
			// TODO: UTF-8
			
			u8 val = u8(node.lit.Unsigned());
			StdVector<u8> data{ val };
			m_TmpVars.push({ ILLitType::Char, data });
			break;
		}
		case TokenType::F16Lit:
		{
			// TODO
			break;
		}
		case TokenType::F32Lit:
		{
			f32 val = f32(node.lit.Fp());
			StdVector<u8> data{ reinterpret_cast<u8*>(&val), reinterpret_cast<u8*>(&val) + sizeof(f32) };
			m_TmpVars.push({ ILLitType::F32, data });
			break;
		}
		case TokenType::F64Lit:
		{
			f64 val = f64(node.lit.Fp());
			StdVector<u8> data{ reinterpret_cast<u8*>(&val), reinterpret_cast<u8*>(&val) + sizeof(f32) };
			m_TmpVars.push({ ILLitType::F64, data });
			break;
		}
		case TokenType::F128Lit:
		{
			// TODO
			break;
		}
		case TokenType::I8Lit:
		{
			u8 val = u8(node.lit.Signed());
			StdVector<u8> data{ val };
			m_TmpVars.push({ ILLitType::I8, data });
			break;
		}
		case TokenType::I16Lit:
		{
			i16 val = i16(node.lit.Signed());
			StdVector<u8> data{ reinterpret_cast<u8*>(&val), reinterpret_cast<u8*>(&val) + sizeof(i16) };
			m_TmpVars.push({ ILLitType::I16, data });
			break;
		}
		case TokenType::I32Lit:
		{
			i32 val = i32(node.lit.Signed());
			StdVector<u8> data{ reinterpret_cast<u8*>(&val), reinterpret_cast<u8*>(&val) + sizeof(i32) };
			m_TmpVars.push({ ILLitType::I32, data });
			break;
		}
		case TokenType::I64Lit:
		{
			i64 val = i64(node.lit.Signed());
			StdVector<u8> data{ reinterpret_cast<u8*>(&val), reinterpret_cast<u8*>(&val) + sizeof(i64) };
			m_TmpVars.push({ ILLitType::I64, data });
			break;
		}
		case TokenType::I128Lit:
		{
			// TODO
			break;
		}
		case TokenType::StringLit:
		{
			const StdString& text = node.lit.Text();
			StdVector<u8> data{ text.begin() + 1, text.end() - 1 }; // trim begin and end "
			data.push_back(0);
			m_TmpVars.push({ ILLitType::String, data });
			break;
		} 
		case TokenType::U8Lit:
		{
			u8 val = u8(node.lit.Unsigned());
			StdVector<u8> data{ val };
			m_TmpVars.push({ ILLitType::U8, data });
			break;
		}
		case TokenType::U16Lit:
		{
			u16 val = u16(node.lit.Unsigned());
			StdVector<u8> data{ reinterpret_cast<u8*>(&val), reinterpret_cast<u8*>(&val) + sizeof(u16) };
			m_TmpVars.push({ ILLitType::U16, data });
			break;
		}
		case TokenType::U32Lit:
		{
			u32 val = u32(node.lit.Unsigned());
			StdVector<u8> data{ reinterpret_cast<u8*>(&val), reinterpret_cast<u8*>(&val) + sizeof(u32) };
			m_TmpVars.push({ ILLitType::U32, data });
			break;
		}
		case TokenType::U64Lit:
		{
			u64 val = u64(node.lit.Unsigned());
			StdVector<u8> data{ reinterpret_cast<u8*>(&val), reinterpret_cast<u8*>(&val) + sizeof(u64) };
			m_TmpVars.push({ ILLitType::U64, data });
			break;
		}
		case TokenType::U128Lit:
		{
			// TODO
			break;
		}
		default: ;
		}
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
					defVar = CreateDstVar(node.typeHandle);

					// TODO: Gen Default
				}

				for (usize i = 0; i < node.sym->orderedVarChildren.size(); ++i)
				{
					if (args[i].kind == ILVarKind::Lit ||
						args[i].type)
						continue;
					
					SymbolSPtr child = node.sym->orderedVarChildren[i].lock();
					ILVar tmpDst = CreateDstVar(child->type);
					ILElemSPtr tmpElem{ new ILMemberAccess{ tmpDst, defVar, child->qualName->Iden()->Name() } };
					m_pCurBlock->elems.push_back(tmpElem);

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
					defVar = CreateDstVar(node.typeHandle);

					// TODO: Gen Default
				}
				
				for (usize i = args.size(); i < node.sym->orderedVarChildren.size(); ++i)
				{
					SymbolSPtr child = node.sym->orderedVarChildren[i].lock();
					ILVar tmpDst = CreateDstVar(child->type);
					ILElemSPtr tmpElem{ new ILMemberAccess{ tmpDst, defVar, child->qualName->Iden()->Name() } };
					m_pCurBlock->elems.push_back(tmpElem);

					args.push_back(tmpDst);
				}
			}
		}

		ILVar dst = CreateDstVar(node.typeHandle);
		
		ILElemSPtr elem{ new ILStructInit{ dst, args } };
		m_pCurBlock->elems.push_back(elem);
	}

	void ILGen::Visit(ITrUnionInit& node)
	{
		Walk(node);

		ILVar arg = PopTmpVar();
		ILVar dst = CreateDstVar(node.typeHandle);
		ILElemSPtr elem{ new ILUnionInit { dst, arg } };
		m_pCurBlock->elems.push_back(elem);
	}

	void ILGen::Visit(ITrAdtTupleEnumInit& node)
	{
		StdVector<ILVar> args;
		for (ITrArgSPtr arg : node.args)
		{
			ITrVisitor::Visit(arg->expr);
			args.push_back(PopTmpVar());
		}

		IdenType& idenType = node.sym->type->AsIden();
		SymbolSPtr structSym = idenType.sym.lock();
		
		ILVar dst = CreateDstVar(node.typeHandle);
		ILElemSPtr elem{ new ILAdtEnumInit{ dst, node.sym->qualName->Iden()->Name(), args } };
		m_pCurBlock->elems.push_back(elem);
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

		IdenType& idenType = node.sym->type->AsIden();
		SymbolSPtr structSym = idenType.sym.lock();

		StdVector<ILVar> args;
		if (namedArgs)
		{
			args.resize(structSym->orderedVarChildren.size());

			for (usize i = 0; i < unorderedArgs.size(); ++i)
			{
				u32 idx = node.argOrder[i];
				args[idx] = unorderedArgs[i];
			}

			if (unorderedArgs.size() < structSym->orderedVarChildren.size())
			{
				if (!node.defExpr)
				{
					defVar = CreateDstVar(node.typeHandle);

					// TODO: Gen Default
				}

				for (usize i = 0; i < structSym->orderedVarChildren.size(); ++i)
				{
					if (args[i].kind == ILVarKind::Lit ||
						args[i].type)
						continue;

					SymbolSPtr child = structSym->orderedVarChildren[i].lock();
					ILVar tmpDst = CreateDstVar(child->type);
					ILElemSPtr tmpElem{ new ILMemberAccess{ tmpDst, defVar, child->qualName->Iden()->Name() } };
					m_pCurBlock->elems.push_back(tmpElem);

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
					defVar = CreateDstVar(node.typeHandle);

					// TODO: Gen Default
				}

				for (usize i = args.size(); i < structSym->orderedVarChildren.size(); ++i)
				{
					SymbolSPtr child = structSym->orderedVarChildren[i].lock();
					ILVar tmpDst = CreateDstVar(child->type);
					ILElemSPtr tmpElem{ new ILMemberAccess{ tmpDst, defVar, child->qualName->Iden()->Name() } };
					m_pCurBlock->elems.push_back(tmpElem);

					args.push_back(tmpDst);
				}
			}
		}

		ILVar tmpStruct = CreateDstVar(structSym->type);
		ILElemSPtr structElem{ new ILStructInit{ tmpStruct, args } };
		m_pCurBlock->elems.push_back(structElem);
		tmpStruct = PopTmpVar();

		ILVar dst = CreateDstVar(node.typeHandle);
		ILElemSPtr elem{ new ILAdtEnumInit{ dst, node.sym->qualName->Iden()->Name(), { tmpStruct } } };
		m_pCurBlock->elems.push_back(elem);
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

		ILVar dst = CreateDstVar(node.typeHandle);
		
		ILElemSPtr elem{ new ILTupInit{ dst, args } };
		m_pCurBlock->elems.push_back(elem);
	}

	void ILGen::Visit(ITrArrayInit& node)
	{
		Walk(node);
		
		StdVector<ILVar> args;
		for (usize i = 0; i < node.exprs.size(); ++i)
			args.push_back(PopTmpVar());
		std::reverse(args.begin(), args.end());

		ILVar dst = CreateDstVar(node.typeHandle);
		
		ILElemSPtr elem{ new ILArrInit{ dst, args } };
		m_pCurBlock->elems.push_back(elem);
	}

	void ILGen::Visit(ITrCast& node)
	{
		Walk(node);

		ILVar src = PopTmpVar();
		ILVar dst = CreateDstVar(node.typeHandle);

		ILElemSPtr elem;
		if (node.castKind == ITrCastKind::Transmute)
		{
			elem.reset(new ILTransmute{ dst, src });
		}
		else if (node.operator_.isBuiltin)
		{
			elem.reset(new ILPrimCast{ dst, src });
		}
		else
		{
			elem.reset(new ILFuncCall{ dst, node.operator_.sym->mangledName, { src } });
		}
		m_pCurBlock->elems.push_back(elem);
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

	ILVar ILGen::CreateDstVar(TypeHandle type)
	{
		ILVar dst{ ILVarKind::Copy, m_CurVarId++, type };
		m_Def->tmpVars.push_back(dst);
		ILVar movDst = dst;
		movDst.kind = ILVarKind::Move;
		m_TmpVars.push(movDst);
		
		m_pILMod->types.insert(dst.type->type);
		
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
}
