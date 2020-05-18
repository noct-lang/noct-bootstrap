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
		, m_CurVarId(0)
		, m_CurLabelId(0)
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

			m_CurVarId = 0;
			m_CurLabelId = 0;
			
			StdString mangleName = node.sym.lock()->mangledName;
			m_Def.reset(new ILFuncDef{ mangleName });
			m_pILMod->names.insert(mangleName);

			// params
			for (ITrParamSPtr param : node.params)
			{
				u32 id = m_CurVarId++;
				ILVar var{ ILVarKind::Copy, id, param->type->handle };


				auto it = m_VarMapping.try_emplace(param->iden, StdStack<ILVar>{}).first;
				it->second.push(var);

				m_Def->params.push_back(var);

				TypeSPtr type = m_pCtx->typeReg.GetType(var.type);
				m_pILMod->types.insert(type);
			}

			if (node.retType)
			{
				m_Def->retType = node.retType->handle;
				TypeSPtr type = m_pCtx->typeReg.GetType(m_Def->retType);
				m_pILMod->types.insert(type);
			}

			m_FuncCtx = node.ctx;
			Walk(node);

			m_pCtx->activeModule->ilMod.funcs.push_back(m_Def);
		});
	}

	void ILGen::Visit(ITrBlock& node)
	{
		ILElemSPtr elem{ new ILBlock{ 0 } };

		m_Elems.push(elem);
		Walk(node);
		m_Elems.pop();
		AddToParent(elem);
	}

	void ILGen::Visit(ITrIf& node)
	{
		
		//ILElemSPtr elem{ new ILIf{  } };
		
	}

	void ILGen::Visit(ITrLoop& node)
	{
		u32 loopBeginLabel = m_CurLabelId++;
		u32 loopEndLabel = m_CurLabelId++;

		m_LoopBeginLabels.push(loopBeginLabel);
		m_LoopEndLabels.push(loopEndLabel);

		ILElemSPtr elem{ new ILLoop{ loopBeginLabel, loopEndLabel } };
		m_Elems.push(elem);
		Walk(node);
		m_Elems.pop();
		
	}

	void ILGen::Visit(ITrSwitch& node)
	{
	}

	void ILGen::Visit(ITrLabel& node)
	{
		u32 label = m_CurLabelId++;
		m_LabelMapping.try_emplace(node.label, label);
		ILElemSPtr elem{ new ILLabel{ label } };
		AddToParent(elem);
	}

	void ILGen::Visit(ITrBreak& node)
	{
		ILElemSPtr elem;
		if (node.label)
		{
			auto it = m_LabelMapping.find(node.label);
			elem.reset(new ILGoto{ it->second });
		}
		else
		{
			u32 label = m_LoopBeginLabels.top();
			m_LoopBeginLabels.pop();
			elem.reset(new ILGoto{ label });
		}
		AddToParent(elem);
	}

	void ILGen::Visit(ITrContinue& node)
	{
		ILElemSPtr elem;
		if (node.label)
		{
			auto it = m_LabelMapping.find(node.label);
			elem.reset(new ILGoto{ it->second });
		}
		else
		{
			u32 label = m_LoopEndLabels.top();
			m_LoopEndLabels.pop();
			elem.reset(new ILGoto{ label });
		}
		AddToParent(elem);
	}

	void ILGen::Visit(ITrFallthrough& node)
	{
		m_FallThrough = true;
	}

	void ILGen::Visit(ITrGoto& node)
	{
	}

	void ILGen::Visit(ITrReturn& node)
	{
		Walk(node);

		ILElemSPtr elem;
		if (node.expr)
		{
			ILVar var = PopTmpVar();
			elem.reset(new ILReturn{ var });
		}
		else
		{
			elem.reset(new ILReturn{});
		}
		AddToParent(elem);
	}

	void ILGen::Visit(ITrThrow& node)
	{
	}

	void ILGen::Visit(ITrAssign& node)
	{
		Walk(node);
		ILVar src = PopTmpVar();
		ILVar dst = PopTmpVar();

		ILElemSPtr elem{ new ILAssign{ dst, src } };
		m_TmpVars.push(dst);
		AddToParent(elem);		
	}

	void ILGen::Visit(ITrTernary& node)
	{
		Walk(node);
		ILVar src1 = PopTmpVar();
		ILVar src0 = PopTmpVar();
		ILVar cond = PopTmpVar();
		ILVar dst = PopTmpVar();

		ILElemSPtr elem{ new ILTernary{ dst, cond, src0, src1 } };
		m_TmpVars.push(dst);
		AddToParent(elem);
	}

	void ILGen::Visit(ITrBinary& node)
	{
		Walk(node);
		ILVar right = PopTmpVar();
		ILVar left = PopTmpVar();

		u32 id = m_CurVarId++;
		ILVar dst{ ILVarKind::Copy, id, node.typeHandle };
		m_Def->tmpVars.push_back(dst);
		ILVar movDst = dst;
		movDst.kind = ILVarKind::Move;
		m_TmpVars.push(movDst);

		ILElemSPtr elem;
		if (node.isBuiltinOp)
		{
			elem.reset(new ILPrimBinary{ node.op, dst, left, right });
			TypeSPtr type = m_pCtx->typeReg.GetType(dst.type);
			m_pILMod->types.insert(type);
		}
		//else
		{
			
		}

		AddToParent(elem);
	}

	void ILGen::Visit(ITrUnary& node)
	{
		Walk(node);
		ILVar var = PopTmpVar();
		
		u32 id = m_CurVarId;
		ILVar dst{ ILVarKind::Copy, id, node.typeHandle };
		m_Def->tmpVars.push_back(dst);
		ILVar movDst = dst;
		movDst.kind = ILVarKind::Move;
		m_TmpVars.push(movDst);

		ILElemSPtr elem;
		//if (m_pCtx->typeReg.IsType(var.type, TypeKind::Builtin))
		{
			elem.reset(new ILPrimUnary{ node.op, dst, var });
			TypeSPtr type = m_pCtx->typeReg.GetType(dst.type);
			m_pILMod->types.insert(type);
		}
		//else
		{
			
		}

		AddToParent(elem);
	}

	void ILGen::Visit(ITrQualNameExpr& node)
	{
		// First look for a local var
		LocalVarDataSPtr local = m_FuncCtx->localVars.GetLocalVarData({}, node.qualName->Iden());
		if (local)
		{
			ILVar var;
			auto it = m_VarMapping.find(local->iden);
			if (it == m_VarMapping.end())
			{
				var = ILVar{ ILVarKind::Copy, std::numeric_limits<u32>::max(), local->type };
				TypeSPtr type = m_pCtx->typeReg.GetType(local->type);
				m_pILMod->types.insert(type);
			}
			else
			{
				var = it->second.top();
				TypeSPtr type = m_pCtx->typeReg.GetType(var.type);
				m_pILMod->types.insert(type);
			}

			m_TmpVars.push(var);
			return;
		}

		// TODO
		m_TmpVars.push(ILVar{ ILVarKind::Copy, 0, TypeHandle(-1) });
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
			StdVector<u8> data{ text.begin(), text.end() };
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

	void ILGen::AddToParent(ILElemSPtr elem)
	{
		if (m_Elems.empty())
		{
			m_Def->elems.push_back(elem);
			return;
		}
		
		ILElemSPtr parent = m_Elems.top();
		switch (parent->kind)
		{
		case ILKind::Block:
		{
			ILBlock& block = *static_cast<ILBlock*>(elem.get());
			block.elems.push_back(elem);
			break;
		}
		case ILKind::If:
		{
			ILIf& if_ = *static_cast<ILIf*>(elem.get());
			if_.elems.push_back(elem);
			break;
		}
		case ILKind::Else:
		{
			ILIfElse& ifelse = *static_cast<ILIfElse*>(elem.get());
			ifelse.elems.push_back(elem);
			break;
		}
		case ILKind::Loop:
		{
			ILLoop& loop = *static_cast<ILLoop*>(elem.get());
			loop.elems.push_back(elem);
			break;
		}
		default:;
		}
	}

	ILVar ILGen::PopTmpVar()
	{
		ILVar tmp = m_TmpVars.top();
		m_TmpVars.pop();
		return tmp;
	}
}
