#include "il-printer.hpp"

#include "common/logger.hpp"
#include "common/context.hpp"
#include "il/il.hpp"


namespace Noctis
{
	ILPrinter::ILPrinter()
		: m_Indent(0)
	{
	}

	void ILPrinter::Print(ILModule& mod)
	{
		for (ILFuncDefSPtr funcDef : mod.funcs)
		{
			Visit(*funcDef);
		}
		g_Logger.Flush();
	}

	void ILPrinter::Visit(ILFuncDef& node)
	{
		m_pFuncDef = &node;
		g_Logger.Log("func");

		if (!node.generics.empty())
		{
			g_Logger.Log("<");

			for (usize i = 0; i < node.generics.size(); ++i)
			{
				if (i != 0)
					g_Logger.Log(",");
				
				ILGeneric& gen = node.generics[i];
				g_Logger.Log(gen.iden);
				if (gen.type.IsValid())
				{
					TypeHandle logType = gen.type;
					for (StdPair<const StdString, TypeHandle>& pair : m_pFuncDef->genMapping)
					{
						logType = g_TypeReg.ReplaceSubType(logType, pair.second, g_TypeReg.Iden(TypeMod::None, QualName::Create(pair.first)));
					}
					StdString typeName = g_TypeReg.ToString(logType);
					g_Logger.Log(":%s", typeName.c_str());
				}
			}
			
			g_Logger.Log(">");
		}

		g_Logger.Log(" %s (", node.qualName->ToString().c_str());
		
		u32 paramCount = u32(node.params.size());
		for (u32 i = 0; i < paramCount; ++i)
		{
			if (i != 0)
				g_Logger.Log(", ");
			LogVar(node.params[i]);
		}

		if (node.retType.IsValid())
		{
			TypeHandle logType = node.retType;
			for (StdPair<const StdString, TypeHandle>& pair : m_pFuncDef->genMapping)
			{
				logType = g_TypeReg.ReplaceSubType(logType, pair.second, g_TypeReg.Iden(TypeMod::None, QualName::Create(pair.first)));
			}
			StdString typeName = g_TypeReg.ToString(logType);
			g_Logger.Log(") -> %s {\n", typeName.c_str());
		}
		else
		{
			g_Logger.Log(") -> () {\n");
		}

		for (ILVar& var : node.localVars)
		{
			PrintIndent();
			g_Logger.Log("local ");
			LogVar(var);
			g_Logger.Log("\n");
		}

		
		for (ILBlock& block : node.blocks)
		{
			Visit(block);
		}
		g_Logger.Log("}\n");
	}

	void ILPrinter::Visit(ILBlock& node)
	{
		PrintIndent();
		g_Logger.Log("bb%u:\n", node.label);
		PrintIndent();
		g_Logger.Log("{\n", node.label);

		++m_Indent;
		ILVisitor::Visit(node);
		--m_Indent;
		
		PrintIndent();
		g_Logger.Log("}\n");
	}

	void ILPrinter::Visit(ILIf& node)
	{
		PrintIndent();
		g_Logger.Log("if ");
		LogVar(node.cond);
		g_Logger.Log(" ? %u : %u\n", node.trueLabel, node.falseLabel);
		
		ILVisitor::Visit(node);
	}


	void ILPrinter::Visit(ILSwitch& node)
	{
		PrintIndent();
		g_Logger.Log("switch ");
		LogVar(node.cond);
		g_Logger.Log(" ->\n");
		PrintIndent();
		g_Logger.Log("    [\n");

		for (StdPair<ILVar, u32> case_ : node.cases)
		{
			PrintIndent();
			g_Logger.Log("    ");
			LogVar(case_.first);
			g_Logger.Log(" => %u,\n", case_.second);
		}

		PrintIndent();
		g_Logger.Log("    _ => %u\n", node.defCase);
		PrintIndent();
		g_Logger.Log("    ]\n");
	}

	void ILPrinter::Visit(ILGoto& node)
	{
		PrintIndent();
		g_Logger.Log("goto %u\n", node.label);
	}

	void ILPrinter::Visit(ILReturn& node)
	{
		PrintIndent();
		g_Logger.Log("return");
		if (node.kind == ILKind::ReturnVal)
		{
			g_Logger.Log(" ");
			LogVar(node.var);
		}
		g_Logger.Log("\n");
	}

	void ILPrinter::Visit(ILUnreachable& node)
	{
		PrintIndent();
		g_Logger.Log("unreachable\n");
	}

	void ILPrinter::Visit(ILAssign& node)
	{
		PrintIndent();
		LogVar(node.dst);
		g_Logger.Log(" = ");
		LogVar(node.src);
		g_Logger.Log("\n");
	}

	void ILPrinter::Visit(ILPrimAssign& node)
	{
		PrintIndent();
		LogVar(node.dst);
		g_Logger.Log(" %s ", GetOpName(node.op).data());
		LogVar(node.src);
		g_Logger.Log("\n");
	}

	void ILPrinter::Visit(ILPrimBinary& node)
	{
		PrintIndent();
		LogVar(node.dst);
		g_Logger.Log(" = ");
		LogVar(node.src0);
		g_Logger.Log(" %s ", GetOpName(node.op).data());
		LogVar(node.src1);
		g_Logger.Log("\n");
	}

	void ILPrinter::Visit(ILPrimUnary& node)
	{
		PrintIndent();
		LogVar(node.dst);
		OperatorKind op = node.op;
		switch (node.op)
		{
		case OperatorKind::PreInc:
		{
			g_Logger.Log(" = ++");
			LogVar(node.src);
			break;
		}
		case OperatorKind::PreDec:
		{
			g_Logger.Log(" = --");
			LogVar(node.src);
			break;
		}
		case OperatorKind::NullPanic:
		{
			g_Logger.Log(" = ");
			LogVar(node.src);
			g_Logger.Log("!!");
			break;
		}
		default:
		{
			g_Logger.Log(" = %s", GetOpName(node.op).data());
			LogVar(node.src);
			break;
		}
		}
		
		g_Logger.Log("\n");
	}

	void ILPrinter::Visit(ILPrimCast& node)
	{
		PrintIndent();
		LogVar(node.dst);
		g_Logger.Log(" = ");
		LogVar(node.src);
		StdString typeName = g_TypeReg.ToString(node.dst.type);
		g_Logger.Log(" as %s", typeName.c_str());
		g_Logger.Log("\n");
	}

	void ILPrinter::Visit(ILTernary& node)
	{
		PrintIndent();
		LogVar(node.dst);
		g_Logger.Log(" = ");
		LogVar(node.cond);
		g_Logger.Log(" ? ");
		LogVar(node.src0);
		g_Logger.Log(" : ");
		LogVar(node.src1);
		g_Logger.Log("\n");
	}

	void ILPrinter::Visit(ILTransmute& node)
	{
		PrintIndent();
		LogVar(node.dst);
		g_Logger.Log(" = ");
		LogVar(node.src);
		g_Logger.Log(" transmute %%s\n", g_TypeReg.ToString(node.dst.type));
	}

	void ILPrinter::Visit(ILIndex& node)
	{
		PrintIndent();
		LogVar(node.dst);
		g_Logger.Log(" = (");
		LogVar(node.src);
		g_Logger.Log(")[");
		LogVar(node.idx);
		g_Logger.Log("]\n");
	}

	void ILPrinter::Visit(ILGenVal& node)
	{
		PrintIndent();
		LogVar(node.dst);
		g_Logger.Log(" = gen_val %s\n", node.genName.c_str());
	}

	void ILPrinter::Visit(ILCompIntrin& node)
	{
		StdString intrinName = GetCompIntrinName(node.intrin);

		PrintIndent();
		if (node.dst.type.IsValid())
		{
			LogVar(node.dst);
			g_Logger.Log(" = ");
		}

		g_Logger.Log("compintrin %s(", intrinName.c_str());
		for (usize i = 0; i < node.vars.size(); ++i)
		{
			if (i != 0)
				g_Logger.Log(", ");
			LogVar(node.vars[i]);
		}
		if (!node.vars.empty() && !node.types.empty())
			g_Logger.Log(", ");
		for (usize i = 0; i < node.types.size(); ++i)
		{
			if (i != 0)
				g_Logger.Log(", ");
			g_Logger.Log(g_TypeReg.ToString(node.types[i]));
		}
		g_Logger.Log(")\n");
	}

	void ILPrinter::Visit(ILStaticCall& node)
	{
		PrintIndent();
		if (node.kind == ILKind::StaticCallRet)
		{
			LogVar(node.dst);
			g_Logger.Log(" = ");
		}

		g_Logger.Log("call %s(", node.func->ToString().c_str());

		usize argCnt = node.args.size();
		for (usize i = 0; i < argCnt; ++i)
		{
			if (i != 0)
				g_Logger.Log(", ");
			LogVar(node.args[i]);
		}

		g_Logger.Log(")\n");
	}

	void ILPrinter::Visit(ILDynamicCall& node)
	{
		PrintIndent();
		if (node.kind == ILKind::DynamicCallRet)
		{
			LogVar(node.dst);
			g_Logger.Log(" = ");
		}

		g_Logger.Log("call (");
		LogVar(node.caller);
		g_Logger.Log(").%s(", node.func.c_str());

		usize argCnt = node.args.size();
		for (usize i = 0; i < argCnt; ++i)
		{
			if (i != 0)
				g_Logger.Log(", ");
			LogVar(node.args[i]);
		}

		g_Logger.Log(")\n");
	}

	void ILPrinter::Visit(ILIndirectCall& node)
	{
		PrintIndent();
		if (node.kind == ILKind::IndirectCallRet)
		{
			LogVar(node.dst);
			g_Logger.Log(" = ");
		}

		g_Logger.Log("call ");
		LogVar(node.func);
		g_Logger.Log(" (");

		usize argCnt = node.args.size();
		for (usize i = 0; i < argCnt; ++i)
		{
			if (i != 0)
				g_Logger.Log(", ");
			LogVar(node.args[i]);
		}

		g_Logger.Log(")\n");
	}

	void ILPrinter::Visit(ILMemberAccess& node)
	{
		PrintIndent();
		LogVar(node.dst);
		g_Logger.Log(" = (");
		LogVar(node.src);
		g_Logger.Log(").%s\n", node.name.c_str());
	}

	void ILPrinter::Visit(ILTupleAccess& node)
	{
		PrintIndent();
		LogVar(node.dst);
		g_Logger.Log(" = (");
		LogVar(node.src);
		g_Logger.Log(").%u\n", node.index);
	}

	void ILPrinter::Visit(ILStructInit& node)
	{
		PrintIndent();
		LogVar(node.dst);
		StdString typeName = g_TypeReg.ToString(node.dst.type);
		g_Logger.Log(" = struct %s {", typeName.c_str());
		usize argCnt = node.args.size();
		for (usize i = 0; i < argCnt; ++i)
		{
			if (i != 0)
				g_Logger.Log(", ");
			LogVar(node.args[i]);
		}
		g_Logger.Log("}\n");
	}

	void ILPrinter::Visit(ILUnionInit& node)
	{
		PrintIndent();
		LogVar(node.dst);
		StdString typeName = g_TypeReg.ToString(node.dst.type);
		g_Logger.Log(" = union %s { %s: ", typeName.c_str(), node.member.c_str());
		LogVar(node.arg);
		g_Logger.Log("}\n");
	}

	void ILPrinter::Visit(ILValEnumInit& node)
	{
		PrintIndent();
		LogVar(node.dst);
		StdString typeName = g_TypeReg.ToString(node.dst.type);
		g_Logger.Log(" = val_enum %s\n", node.member.c_str());
	}

	void ILPrinter::Visit(ILAdtEnumInit& node)
	{
		PrintIndent();
		LogVar(node.dst);
		StdString typeName = g_TypeReg.ToString(node.dst.type);
		g_Logger.Log(" = adt_enum %s {", node.member.c_str());
		usize argCnt = node.args.size();
		for (usize i = 0; i < argCnt; ++i)
		{
			if (i != 0)
				g_Logger.Log(", ");
			LogVar(node.args[i]);
		}
		g_Logger.Log("}\n");
	}

	void ILPrinter::Visit(ILTupInit& node)
	{
		PrintIndent();
		LogVar(node.dst);
		StdString typeName = g_TypeReg.ToString(node.dst.type);
		g_Logger.Log(" = (");
		usize argCnt = node.args.size();
		for (usize i = 0; i < argCnt; ++i)
		{
			if (i != 0)
				g_Logger.Log(", ");
			LogVar(node.args[i]);
		}
		g_Logger.Log(")\n");
	}

	void ILPrinter::Visit(ILArrInit& node)
	{
		PrintIndent();
		LogVar(node.dst);
		StdString typeName = g_TypeReg.ToString(node.dst.type);
		g_Logger.Log(" = [");
		usize argCnt = node.args.size();
		for (usize i = 0; i < argCnt; ++i)
		{
			if (i != 0)
				g_Logger.Log(", ");
			LogVar(node.args[i]);
		}
		g_Logger.Log("]\n");
	}

	void ILPrinter::PrintIndent()
	{
		g_Logger.Log("    ");
		for (u8 i = 0; i < m_Indent; ++i)
		{
			g_Logger.Log("    ");
		}
	}

	void ILPrinter::LogVar(ILVar& var)
	{
		TypeHandle logType = var.type;
		for (StdPair<const StdString, TypeHandle>& pair : m_pFuncDef->genMapping)
		{
			logType = g_TypeReg.ReplaceSubType(logType, pair.second, g_TypeReg.Iden(TypeMod::None, QualName::Create(pair.first)));
		}
		StdString typeName = g_TypeReg.ToString(logType);
		
		switch (var.kind)
		{
		case ILVarKind::Copy:
		{
			g_Logger.Log("%%%u:%s", var.id, typeName.c_str());
			break;
		}
		case ILVarKind::Ref:
		{
			g_Logger.Log("ref %%%u:%s", var.id, typeName.c_str());
			break;
		}
		case ILVarKind::Move:
		{
			g_Logger.Log("move %%%u:%s", var.id, typeName.c_str());
			break;
		}
		case ILVarKind::Lit:
		{
			switch (var.litType)
			{
			case ILLitType::False:
			{
				g_Logger.Log("false:bool");
				break;
			}
			case ILLitType::True:
			{
				g_Logger.Log("true:bool");
				break;
			}
			case ILLitType::I8:
			{
				i8 val = *reinterpret_cast<i8*>(var.litData.data());
				g_Logger.Log("%i:i8", val);
				break;
			}
			case ILLitType::I16:
			{
				i16 val = *reinterpret_cast<i16*>(var.litData.data());
				g_Logger.Log("%i:i16", val);
				break;
			}
			case ILLitType::I32:
			{
				i32 val = *reinterpret_cast<i32*>(var.litData.data());
				g_Logger.Log("%i:i32", val);
				break;
			}
			case ILLitType::I64:
			{
				i64 val = *reinterpret_cast<i64*>(var.litData.data());
				g_Logger.Log("%i:i64", val);
				break;
			}
			case ILLitType::I128: break;
			case ILLitType::U8:
			{
				u8 val = *reinterpret_cast<u8*>(var.litData.data());
				g_Logger.Log("%u:u8", val);
				break;
			}
			case ILLitType::U16:
			{
				u16 val = *reinterpret_cast<u16*>(var.litData.data());
				g_Logger.Log("%u:u16", val);
				break;
			}
			case ILLitType::U32:
			{
				u32 val = *reinterpret_cast<u32*>(var.litData.data());
				g_Logger.Log("%u:u32", val);
				break;
			}
			case ILLitType::U64:
			{
				u64 val = *reinterpret_cast<u64*>(var.litData.data());
				g_Logger.Log("%u:u64", val);
				break;
			}
			case ILLitType::U128: break;
			case ILLitType::F32:
			{
				f32 val = *reinterpret_cast<f32*>(var.litData.data());
				g_Logger.Log("%f:f32", val);
				break;
			}
			case ILLitType::F64:
			{
				f64 val = *reinterpret_cast<f64*>(var.litData.data());
				g_Logger.Log("%f:f64", val);
				break;
			}
			case ILLitType::Char:
			{
				i8 val = *reinterpret_cast<i8*>(var.litData.data());
				g_Logger.Log("'%c':char", val);
				break;
			}
			case ILLitType::String:
			{
				i8 val = *reinterpret_cast<i8*>(var.litData.data());
				g_Logger.Log("%s:[]const char", var.litData.data());
				break;
			}
			case ILLitType::Null:
			{
				g_Logger.Log("null:null");
				break;
			}
			default: ;
			}
			
			break;
		}
		default: ;
		}
	}
}
