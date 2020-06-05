#include "il-encode.hpp"

#include "module/module.hpp"
#include "common/context.hpp"
#include "common/errorsystem.hpp"
#include "common/name-mangling.hpp"

namespace Noctis
{
	ILEncode::ILEncode(Context* pCtx)
		: ILVisitor(pCtx)
		, m_pCurEncodingFunc(nullptr)
		, m_TypeWidth(1)
	{
	}

	StdVector<u8> ILEncode::Encode(ILModule& mod)
	{
		StdVector<u8> encoded;
		// Write type table
		encoded.push_back(0xFE);
		u32 typeCount = u32(mod.types.size());
		encoded.insert(encoded.end(), reinterpret_cast<u8*>(&typeCount), reinterpret_cast<u8*>(&typeCount) + sizeof(u32));
		
		u32 typeId = 0;
		for (TypeSPtr type : mod.types)
		{
			m_Types.try_emplace(type, typeId++);
			StdString typeStr = NameMangling::Mangle(m_pCtx, type);
			encoded.insert(encoded.end(), typeStr.begin(), typeStr.end());
			encoded.push_back(0);
		}
		u64 typeTableSize = u64(encoded.size());

		if (mod.types.size() <= 0xFF)
			m_TypeWidth = 1;
		else if (mod.types.size() <= 0xFFFF)
			m_TypeWidth = 2;
		else
			m_TypeWidth = 4;

		
		// Write name table
		encoded.push_back(0xFD);
		u32 nameCount = u32(mod.names.size());
		encoded.insert(encoded.end(), reinterpret_cast<u8*>(&nameCount), reinterpret_cast<u8*>(&nameCount) + sizeof(u32));

		u32 nameId = 0;
		for (const StdString& name : mod.names)
		{
			m_Names.try_emplace(name, nameId++);
			encoded.insert(encoded.end(), name.begin(), name.end());
			encoded.push_back(0);
		}
		u64 nameTableSize = u64(encoded.size()) - typeTableSize;

		// Encode and write functions
		for (ILFuncDefSPtr def : mod.funcs)
		{
			Visit(*def);
		}
		
		for (StdVector<u8>& encodedFunc : m_EncodedFuncs)
		{
			encoded.insert(encoded.end(), encodedFunc.begin(), encodedFunc.end());
		}
		
		// Write and insert header
		ILHeader header;
		header.magic[0] = char(0xFF);
		header.magic[1] = 'N';
		header.magic[2] = 'I';
		header.magic[3] = 'L';
		header.version = 0;
		memset(header.reserved, 0, sizeof(header.reserved));
		header.fileSize = u64(encoded.size() + sizeof(ILHeader));
		header.typeTableSize = typeTableSize;
		header.nameTableSize = nameTableSize;
		encoded.insert(encoded.begin(), reinterpret_cast<u8*>(&header), reinterpret_cast<u8*>(&header) + sizeof(ILHeader));

		return encoded;
	}

	void ILEncode::Visit(ILFuncDef& node)
	{
		m_EncodedFuncs.emplace_back(StdVector<u8>{});
		m_pCurEncodingFunc = &m_EncodedFuncs.back();

		// def id
		u32 idAndMangle = 0xF0; // LE, so id at begin
		u32 nameId = GetNameId(node.mangleName);
		idAndMangle |= (nameId & 0x00FF'FFFF) << 8;
		
		WriteData(idAndMangle);
		WriteData(u32(node.blocks.size()));

		WriteData(u16(node.params.size()));
		for (ILVar& var : node.params)
		{
			EncodeType(var.type);
		}
		
		WriteData(u16(node.localVars.size()));
		for (ILVar& var : node.localVars)
		{
			EncodeType(var.type);
		}

		for (ILBlock& block : node.blocks)
		{
			Visit(block);
		}

		// Insert func size
		u32 size = u32(m_pCurEncodingFunc->size()) + 4;
		m_pCurEncodingFunc->insert(m_pCurEncodingFunc->begin() + 8, reinterpret_cast<u8*>(&size), reinterpret_cast<u8*>(&size) + sizeof(u32));

		m_pCurEncodingFunc = nullptr;
	}

	void ILEncode::Visit(ILBlock& node)
	{
		u32 idAndLabel = u8(node.kind);
		idAndLabel |= (node.label & 0x00FF'FFFF) << 8;
		WriteData(idAndLabel);
		WriteData(u32(node.elems.size()));

		for (ILElemSPtr elem : node.elems)
		{
			ILVisitor::Visit(*elem);
		}
		ILVisitor::Visit(*node.terminal);
	}

	void ILEncode::Visit(ILIf& node)
	{
		u32 trueLabelAndId = u32(node.kind);
		trueLabelAndId |= (node.trueLabel & 0x00FF'FFFF) << 8;
		WriteData(trueLabelAndId);
		WriteData(node.falseLabel);
		EncodeVar(node.cond);
	}

	void ILEncode::Visit(ILSwitch& node)
	{
	}

	void ILEncode::Visit(ILGoto& node)
	{
		u32 idAndLabel = u32(node.kind);
		idAndLabel |= (node.label & 0x00FF'FFFF) << 8;
		WriteData(idAndLabel);
	}

	void ILEncode::Visit(ILReturn& node)
	{
		WriteData(u8(node.kind));
		if (node.kind == ILKind::ReturnVal)
			EncodeVar(node.var);
	}

	void ILEncode::Visit(ILAssign& node)
	{
		WriteData(u8(node.kind));
		EncodeVarAndType(node.dst);
		EncodeVar(node.src);
	}

	void ILEncode::Visit(ILPrimAssign& node)
	{
		WriteData(u8(node.kind));
		WriteData(u8(node.op));
		EncodeVarAndType(node.dst);
		EncodeVar(node.src);
	}

	void ILEncode::Visit(ILPrimBinary& node)
	{
		WriteData(u8(node.kind));
		WriteData(u8(node.op));
		EncodeVarAndType(node.dst);
		EncodeVar(node.src0);
		EncodeVar(node.src1);
	}

	void ILEncode::Visit(ILPrimUnary& node)
	{
		WriteData(u8(node.kind));
		WriteData(u8(node.op));
		EncodeVarAndType(node.dst);
		EncodeVar(node.src);
	}

	void ILEncode::Visit(ILPrimCast& node)
	{
		WriteData(u8(node.kind));
		EncodeVarAndType(node.dst);
		EncodeVar(node.src);
	}

	void ILEncode::Visit(ILTernary& node)
	{
		WriteData(u8(node.kind));
		EncodeVarAndType(node.dst);
		EncodeVar(node.cond);
		EncodeVar(node.src0);
		EncodeVar(node.src1);
	}

	void ILEncode::Visit(ILTransmute& node)
	{
		WriteData(u8(node.kind));
		EncodeVarAndType(node.dst);
		EncodeVar(node.src);
	}

	void ILEncode::Visit(ILFuncCall& node)
	{
		WriteData(u8(node.kind));
		u32 nameId = GetNameId(node.func);
		WriteData(nameId);
		WriteData(u8(node.args.size()));

		if (node.kind == ILKind::FuncCallRet)
			EncodeVarAndType(node.dst);

		for (ILVar& arg : node.args)
		{
			EncodeVar(arg);
		}
	}

	void ILEncode::Visit(ILMethodCall& node)
	{
		WriteData(u8(node.kind));
		u32 nameId = GetNameId(node.func);
		WriteData(nameId);
		WriteData(u8(node.args.size()));

		if (node.kind == ILKind::FuncCallRet)
			EncodeVarAndType(node.dst);

		EncodeVar(node.caller);

		for (ILVar& arg : node.args)
		{
			EncodeVar(arg);
		}
	}

	void ILEncode::Visit(ILIndirectCall& node)
	{
		WriteData(u8(node.kind));
		WriteData(u8(node.args.size()));

		if (node.kind == ILKind::IndirectCallRet)
			EncodeVarAndType(node.dst);

		EncodeVar(node.func);

		for (ILVar& arg : node.args)
		{
			EncodeVar(arg);
		}
	}

	void ILEncode::Visit(ILMemberAccess& node)
	{
		WriteData(u8(node.kind));
		u32 nameId = GetNameId(node.name);
		WriteData(nameId);
		EncodeVarAndType(node.dst);
		EncodeVar(node.src);
	}

	void ILEncode::Visit(ILTupleAccess& node)
	{
		WriteData(u8(node.kind));
		WriteData(node.index);
		EncodeVarAndType(node.dst);
		EncodeVar(node.src);
	}

	void ILEncode::Visit(ILStructInit& node)
	{
		WriteData(u8(node.kind));
		WriteData(u8(node.args.size()));
		EncodeVarAndType(node.dst);
		for (ILVar& arg : node.args)
		{
			EncodeVar(arg);
		}
	}

	void ILEncode::Visit(ILUnionInit& node)
	{
		WriteData(u8(node.kind));
		WriteData(u8(node.args.size()));
		EncodeVarAndType(node.dst);
		for (ILVar& arg : node.args)
		{
			EncodeVar(arg);
		}
	}

	void ILEncode::Visit(ILValEnumInit& node)
	{
		WriteData(u8(node.kind));
		u32 nameId = GetNameId(node.member);
		WriteData(nameId);
		EncodeVarAndType(node.dst);
	}

	void ILEncode::Visit(ILAdtEnumInit& node)
	{
		WriteData(u8(node.kind));
		u32 nameId = GetNameId(node.member);
		WriteData(nameId);
		WriteData(u8(node.args.size()));
		EncodeVarAndType(node.dst);
		for (ILVar& arg : node.args)
		{
			EncodeVar(arg);
		}
	}

	void ILEncode::Visit(ILTupInit& node)
	{
		WriteData(u8(node.kind));
		WriteData(u8(node.args.size()));
		EncodeVarAndType(node.dst);
		for (ILVar& arg : node.args)
		{
			EncodeVar(arg);
		}
	}

	void ILEncode::Visit(ILArrInit& node)
	{
		WriteData(u8(node.kind));
		WriteData(u8(node.args.size()));
		EncodeVarAndType(node.dst);
		for (ILVar& arg : node.args)
		{
			EncodeVar(arg);
		}
	}

	void ILEncode::EncodeVarAndType(ILVar& var)
	{
		EncodeVar(var);
		EncodeType(var.type);
	}

	void ILEncode::EncodeVar(ILVar& var)
	{
		if (var.kind == ILVarKind::Lit)
		{
			u8 header = ((u8(ILVarKind::Lit) & 0x3) << 6) | ((u8(var.litType) & 0x1F) << 1) | (u8(var.boolBit) & 0x1);
			WriteData(header);
			m_pCurEncodingFunc->insert(m_pCurEncodingFunc->end(), var.litData.begin(), var.litData.end());
		}
		else
		{
			// 1 byte val has 5 bytes to store id
			bool moreThan1Byte = var.id > 0x1F;
			
			u8 val = u8(var.kind) << 6;
			val |= u32(moreThan1Byte) << 5;
			val |= var.id & 0x1F;
			WriteData(val);

			if (!moreThan1Byte)
				return;
			
			// 2 byte val has 12 bytes to store id
			bool moreThan2Bytes = var.id > 0x0FFF;

			val = u32(moreThan2Bytes) << 7;
			val |= (var.id >> 5) & 0x7F;
			WriteData(val);

			if (!moreThan2Bytes)
				return;

			// 3 byte val has 19 bytes to store id
			bool moreThan3Bytes = var.id > 0x7FFFF;
			val = u32(moreThan2Bytes) << 7;
			val |= (var.id >> 12) & 0x7F;
			WriteData(val);

			if (!moreThan3Bytes)
				return;


			// 4 byte val has 27 bytes to store id (max size)
			val = (var.id >> 19) & 0xFF;
			WriteData(val);
		}
	}

	void ILEncode::EncodeType(TypeHandle handle)
	{
		TypeSPtr type = m_pCtx->typeReg.GetType(handle);
		auto it = m_Types.find(type);
		switch (m_TypeWidth)
		{
		case 1:
		{
			if (it != m_Types.end())
				WriteData(u8(it->second));
			else
				WriteData(u8(0xFF));
			break;
		}
		case 2:
		{
			if (it != m_Types.end())
				WriteData(u16(it->second));
			else
				WriteData(u16(0xFFFF));
			break;
		}
		case 4:
		{
			if (it != m_Types.end())
				WriteData(u32(it->second));
			else
				WriteData(u32(0xFFF'FFFF));
			break;
		}
		default:;
		}
	}

	u32 ILEncode::GetNameId(const StdString& name)
	{
		auto it = m_Names.find(name);
		if (it == m_Names.end())
			return 0xFFFF'FFFF;
		return it->second;
	}

	ILDecode::ILDecode(Context* pCtx)
		: m_pCtx(pCtx)
		, m_pMod(nullptr)
		, m_pByteCode(nullptr)
		, m_BCPos(0)
		, m_CurVarId(0)
		, m_TypeWidth(0)
		, m_VarWidth(0)
	{
	}

	ILModule ILDecode::Decode(const StdVector<u8>& bytecode)
	{
		ILModule mod;
		m_pMod = &mod;

		const ILHeader& header = *reinterpret_cast<const ILHeader*>(bytecode.data());
		m_BCPos = sizeof(ILHeader);
		m_pByteCode = &bytecode;


		// Check magic
		if (header.magic[0] != char(0xFF) ||
			header.magic[1] != 'N' ||
			header.magic[2] != 'I' ||
			header.magic[3] != 'L')
		{
			g_ErrorSystem.Error("Trying to decode IL bytecode with invalid signature!\n");
			return mod;
		}

		// Check version
		if (header.version != 0)
		{
			g_ErrorSystem.Error("Trying to decode IL bytecode with invalid version!\n");
			return mod;
		}

		// Check reserved
		if (header.reserved[0] != 0 ||
			header.reserved[1] != 0 ||
			header.reserved[2] != 0)
		{
			g_ErrorSystem.Error("IL bytecode has reserved bytes set!\n");
		}

		if (header.typeTableSize > 0)
		{
			if (bytecode[m_BCPos] != 0xFE)
			{
				g_ErrorSystem.Error("IL bytecode type table is missing!\n");
				return mod;
			}

			DecodeTypeTable(header.typeTableSize);

			if (m_Types.size() <= 0xFF)
				m_TypeWidth = 1;
			else if (m_Types.size() <= 0xFFFF)
				m_TypeWidth = 2;
			else
				m_TypeWidth = 4;
		}


		if (header.nameTableSize > 0)
		{
			if (bytecode[m_BCPos] != 0xFD)
			{
				g_ErrorSystem.Error("IL bytecode name table is missing!\n");
				return mod;
			}

			DecodeNameTable(header.nameTableSize);
		}

		while (m_BCPos < bytecode.size())
		{
			DecodeFunc();
		}


		return mod;
	}

	void ILDecode::DecodeTypeTable(u64 size)
	{
		const StdVector<u8>& bytecode = *m_pByteCode;
		u64 typeEnd = m_BCPos + size;

		// Skip id
		++m_BCPos;

		u32 count = *reinterpret_cast<const u32*>(&bytecode[m_BCPos]);
		m_BCPos += sizeof(u32);

		for (u32 i = 0; i < count; ++i)
		{
			StdString typeMangle = ExtractNullTermString(bytecode, m_BCPos);
			TypeHandle handle = NameMangling::DemangleType(m_pCtx, typeMangle);
			m_Types.push_back(handle);
			m_pMod->types.insert(m_pCtx->typeReg.GetType(handle));
		}
	}

	void ILDecode::DecodeNameTable(u64 size)
	{
		const StdVector<u8>& bytecode = *m_pByteCode;
		u64 typeEnd = m_BCPos + size;

		// Skip id
		++m_BCPos;

		u32 count = *reinterpret_cast<const u32*>(&bytecode[m_BCPos]);
		m_BCPos += sizeof(u32);

		for (u32 i = 0; i < count; ++i)
		{
			StdString name = ExtractNullTermString(bytecode, m_BCPos);
			m_Names.push_back(name);
			m_pMod->names.insert(name);
		}
	}

	void ILDecode::DecodeFunc()
	{
		const StdVector<u8>& bytecode = *m_pByteCode;

		m_CurVarId = 0;
		
		// skip ID
		u32 idAndMangle = ReadData<u32>();

		u32 mangleId = idAndMangle >> 8;
		StdString mangle = m_Names[mangleId];

		ILFuncDefSPtr def{ new ILFuncDef{ mangle } };

		u32 blockCount = ReadData<u32>();
		u32 funcSize = ReadData<u32>();
		u16 paramCount = ReadData<u16>();

		
		for (u16 i = 0; i < paramCount; ++i)
		{
			u32 id = m_CurVarId++;
			TypeHandle type = DecodeType();
			def->params.push_back(ILVar{ ILVarKind::Copy, id, type });
			m_IdTypeMapping.push_back(type);
		}

		u16 localCount = ReadData<u16>();

		for (u16 i = 0; i < localCount; ++i)
		{
			u32 id = m_CurVarId++;
			TypeHandle type = DecodeType();
			def->localVars.push_back(ILVar{ ILVarKind::Copy, id, type });
			m_IdTypeMapping.push_back(type);
		}

		for (u32 i = 0; i < blockCount; ++i)
		{
			ILBlock block = DecodeBlock();
			def->blocks.push_back(std::move(block));
		}
		
		m_pMod->funcs.push_back(def);
	}

	ILBlock ILDecode::DecodeBlock()
	{
		u32 idAndLabel = ReadData<u32>();

		// TODO: Check block def id

		u32 label = (idAndLabel & 0xFFFF'FF00) >> 8;
		ILBlock block{ label };

		u32 elemCount = ReadData<u32>();
		for (u32 i = 0; i < elemCount; ++i)
		{
			block.elems.push_back(DecodeElem());
		}
		ILElemSPtr term = DecodeElem();
		block.terminal = *reinterpret_cast<ILTerminalSPtr*>(&term);
		return block;
	}

	ILElemSPtr ILDecode::DecodeElem()
	{
		ILKind kind = ILKind(ReadData<u8>());
		switch (kind)
		{
		case ILKind::If: return DecodeIf();
		case ILKind::Switch: return DecodeSwitch();
		case ILKind::Goto: return DecodeGoto();
		case ILKind::ReturnNoVal: return DecodeReturn(false);
		case ILKind::ReturnVal: return DecodeReturn(true);
		case ILKind::Assign: return DecodeAssign();
		case ILKind::PrimAssign: return DecodePrimAssign();
		case ILKind::PrimBinary: return DecodePrimBinary();
		case ILKind::PrimUnary: return DecodePrimUnary();
		case ILKind::PrimCast: return DecodePrimCast();
		case ILKind::Ternary: return DecodeTernary();
		case ILKind::Transmute: return DecodeTransmute();
		case ILKind::CompIntrin: return DecodeCompIntrin();
		case ILKind::FuncCallNoRet: return DecodeFuncCall(false);
		case ILKind::FuncCallRet: return DecodeFuncCall(true);
		case ILKind::MethodCallNoRet: return DecodeMethodCall(false);
		case ILKind::MethodCallRet: return DecodeMethodCall(true);
		case ILKind::MemberAccess: return DecodeMemberAccess();
		case ILKind::TupleAccess: return DecodeTupleAccess();
		case ILKind::StructInit: return DecodeStructInit();
		case ILKind::ValEnumInit: return DecodeValEnumInit();
		case ILKind::AdtEnumInit: return DecodeAdtEnumInit();
		case ILKind::TupInit: return DecodeTupInit();
		case ILKind::ArrInit: return DecodeArrInit();
		default: return nullptr;
		}
	}

	ILElemSPtr ILDecode::DecodeIf()
	{
		u32 trueLabel = ReadData<u32>(3);
		u32 falseLabel = ReadData<u32>(3);
		(void)ReadData<u8>();
		ILVar cond = DecodeVar(false);
		return ILElemSPtr{ new ILIf{ cond, trueLabel, falseLabel } };
	}

	ILElemSPtr ILDecode::DecodeSwitch()
	{
		// TODO
		return nullptr;
	}

	ILElemSPtr ILDecode::DecodeGoto()
	{
		u32 label = ReadData<u32>(3);
		return ILElemSPtr{ new ILGoto{ label } };
	}

	ILElemSPtr ILDecode::DecodeReturn(bool hasVal)
	{
		if (!hasVal)
			return ILElemSPtr{ new ILReturn{} };

		ILVar var = DecodeVar(false);
		return ILElemSPtr{ new ILReturn{ var } };
	}

	ILElemSPtr ILDecode::DecodeAssign()
	{
		ILVar dst = DecodeVar(true);
		ILVar src = DecodeVar(false);
		return ILElemSPtr{ new ILAssign{ dst, src } };
	}

	ILElemSPtr ILDecode::DecodePrimAssign()
	{
		OperatorKind op = OperatorKind(ReadData<u8>());
		ILVar dst = DecodeVar(true);
		ILVar src = DecodeVar(false);
		return ILElemSPtr{ new ILPrimAssign{ op, dst, src } };
	}

	ILElemSPtr ILDecode::DecodePrimBinary()
	{
		OperatorKind op = OperatorKind(ReadData<u8>());
		ILVar dst = DecodeVar(true);
		ILVar src0 = DecodeVar(false);
		ILVar src1 = DecodeVar(false);
		return ILElemSPtr{ new ILPrimBinary{ op, dst, src0, src1 } };
	}

	ILElemSPtr ILDecode::DecodePrimUnary()
	{
		OperatorKind op = OperatorKind(ReadData<u8>());
		ILVar dst = DecodeVar(true);
		ILVar src = DecodeVar(false);
		return ILElemSPtr{ new ILPrimUnary{ op, dst, src } };
	}

	ILElemSPtr ILDecode::DecodePrimCast()
	{
		ILVar dst = DecodeVar(true);
		ILVar src = DecodeVar(false);
		return ILElemSPtr{ new ILPrimCast{ dst, src } };
	}

	ILElemSPtr ILDecode::DecodeTernary()
	{
		ILVar dst = DecodeVar(true);
		ILVar cond = DecodeVar(false);
		ILVar src0 = DecodeVar(false);
		ILVar src1 = DecodeVar(false);
		return ILElemSPtr{ new ILTernary{ dst, cond, src0, src1 } };
	}

	ILElemSPtr ILDecode::DecodeTransmute()
	{
		ILVar dst = DecodeVar(true);
		ILVar src = DecodeVar(false);
		return ILElemSPtr{ new ILTransmute{ dst, src } };
	}

	ILElemSPtr ILDecode::DecodeCompIntrin()
	{
		// TODO
		return nullptr;
	}

	ILElemSPtr ILDecode::DecodeFuncCall(bool hasRet)
	{
		u32 nameId = ReadData<u32>();
		u8 numArgs = ReadData<u8>();

		ILVar dst;
		if (hasRet)
			dst = DecodeVar(true);

		StdVector<ILVar> args;
		for (u8 i = 0; i < numArgs; ++i)
		{
			args.push_back(DecodeVar(false));
		}

		StdString name = m_Names[nameId];
		if (hasRet)
			return ILElemSPtr{ new ILFuncCall{ dst, name, args } };
		return ILElemSPtr{ new ILFuncCall{ name, args } };
	}

	ILElemSPtr ILDecode::DecodeMethodCall(bool hasRet)
	{
		u32 nameId = ReadData<u32>();
		u8 numArgs = ReadData<u8>();

		ILVar dst;
		if (hasRet)
			dst = DecodeVar(true);

		ILVar caller = DecodeVar(false);
		StdVector<ILVar> args;
		for (u8 i = 0; i < numArgs; ++i)
		{
			args.push_back(DecodeVar(false));
		}
		
		StdString name = m_Names[nameId];
		if (hasRet)
			return ILElemSPtr{ new ILMethodCall{ dst, caller, name, args } };
		return ILElemSPtr{ new ILMethodCall{ caller, name, args } };
	}

	ILElemSPtr ILDecode::DecodeIndirectCall(bool hasRet)
	{
		u8 numArgs = ReadData<u8>();

		ILVar dst;
		if (hasRet)
			dst = DecodeVar(true);

		ILVar func = DecodeVar(false);
		StdVector<ILVar> args;
		for (u8 i = 0; i < numArgs; ++i)
		{
			args.push_back(DecodeVar(false));
		}

		if (hasRet)
			return ILElemSPtr{ new ILIndirectCall{ dst, func, args } };
		return ILElemSPtr{ new ILIndirectCall{ func, args } };
	}

	ILElemSPtr ILDecode::DecodeMemberAccess()
	{
		u32 nameId = ReadData<u32>();
		ILVar dst = DecodeVar(true);
		ILVar src = DecodeVar(false);
		StdString name = m_Names[nameId];
		return ILElemSPtr{ new ILMemberAccess{ dst, src, name } };
	}

	ILElemSPtr ILDecode::DecodeTupleAccess()
	{
		u16 index = ReadData<u16>();
		ILVar dst = DecodeVar(true);
		ILVar src = DecodeVar(false);
		return ILElemSPtr{ new ILTupleAccess{ dst, src, index } };
	}

	ILElemSPtr ILDecode::DecodeStructInit()
	{
		u8 argCount = ReadData<u8>();
		ILVar dst = DecodeVar(true);
		StdVector<ILVar> args;
		for (u8 i = 0; i < argCount; ++i)
		{
			args.push_back(DecodeVar(false));
		}
		return ILElemSPtr{ new ILStructInit{ dst, args } };
	}

	ILElemSPtr ILDecode::DecodeUnionInit()
	{
		u8 argCount = ReadData<u8>();
		ILVar dst = DecodeVar(true);
		StdVector<ILVar> args;
		for (u8 i = 0; i < argCount; ++i)
		{
			args.push_back(DecodeVar(false));
		}
		return ILElemSPtr{ new ILUnionInit{ dst, args } };
	}

	ILElemSPtr ILDecode::DecodeValEnumInit()
	{
		u32 nameId = ReadData<u32>();
		ILVar dst = DecodeVar(true);
		StdString name = m_Names[nameId];
		return ILElemSPtr{ new ILValEnumInit{ dst, name } };
	}

	ILElemSPtr ILDecode::DecodeAdtEnumInit()
	{
		u32 nameId = ReadData<u32>();
		u8 argCount = ReadData<u8>();
		ILVar dst = DecodeVar(true);
		StdVector<ILVar> args;
		for (u8 i = 0; i < argCount; ++i)
		{
			args.push_back(DecodeVar(false));
		}
		
		StdString name = m_Names[nameId];
		return ILElemSPtr{ new ILAdtEnumInit{ dst, name, args } };
	}

	ILElemSPtr ILDecode::DecodeTupInit()
	{
		u8 argCount = ReadData<u8>();
		ILVar dst = DecodeVar(true);
		StdVector<ILVar> args;
		for (u8 i = 0; i < argCount; ++i)
		{
			args.push_back(DecodeVar(false));
		}

		return ILElemSPtr{ new ILTupInit{ dst, args } };
	}

	ILElemSPtr ILDecode::DecodeArrInit()
	{
		u8 argCount = ReadData<u8>();
		ILVar dst = DecodeVar(true);
		StdVector<ILVar> args;
		for (u8 i = 0; i < argCount; ++i)
		{
			args.push_back(DecodeVar(false));
		}

		return ILElemSPtr{ new ILArrInit{ dst, args } };
	}

	TypeHandle ILDecode::DecodeType()
	{
		switch (m_TypeWidth)
		{
		case 1:
		{
			u8 idx = ReadData<u8>();
			return m_Types[idx];
		}
		case 3:
		{
			u16 idx = ReadData<u16>();
			return m_Types[idx];
		}
		case 4:
		{
			u32 idx = ReadData<u32>();
			return m_Types[idx];
		}
		default: return TypeHandle(-1);
		}
	}

	ILVar ILDecode::DecodeVar(bool typeStored)
	{
		u8 byte = ReadData<u8>();

		ILVarKind varKind = ILVarKind(byte >> 6);

		if (varKind == ILVarKind::Lit)
		{
			ILLitType litType = ILLitType((byte >> 1) & 0x1F);
			StdVector<u8> litData;
			switch (litType)
			{
			case ILLitType::Bool:
			{
				bool val = byte & 0x01;
				return ILVar{ val };
			}
			case ILLitType::I8:
			case ILLitType::U8:
			{
				u8 val = ReadData<u8>();
				litData.push_back(u8(val));
				break;
			}
			case ILLitType::I16:
			case ILLitType::U16:
			{
				u16 val = ReadData<u16>();
				u8* addr = reinterpret_cast<u8*>(&val);
				litData.insert(litData.end(), addr, addr + sizeof(u16));
				break;
			}
			case ILLitType::I32:
			case ILLitType::U32:
			case ILLitType::F32:
			{
				u32 val = ReadData<u32>();
				u8* addr = reinterpret_cast<u8*>(&val);
				litData.insert(litData.end(), addr, addr + sizeof(u32));
				break;
			}
			case ILLitType::I64:
			case ILLitType::U64:
			case ILLitType::F64:
			{
				u64 val = ReadData<u64>();
				u8* addr = reinterpret_cast<u8*>(&val);
				litData.insert(litData.end(), addr, addr + sizeof(u64));
				break;
			}
			case ILLitType::I128:
			case ILLitType::U128:
			{
				u64 low = ReadData<u64>();
				u8* addr = reinterpret_cast<u8*>(&low);
				litData.insert(litData.end(), addr, addr + sizeof(u64));

				u64 high = ReadData<u64>();
				addr = reinterpret_cast<u8*>(&high);
				litData.insert(litData.end(), addr, addr + sizeof(u64));
				break;
			}
			case ILLitType::Char:
			{
				// TODO: UTF-8 char in bootstrap ???
				char val = ReadData<char>();
				litData.push_back(u8(val));
				break;
			}
			case ILLitType::String:
			{
				StdString str = ExtractNullTermString(*m_pByteCode, m_BCPos);
				litData.insert(litData.end(), str.begin(), str.end());
				break;
			}
			default: ;
			}

			return ILVar{ litType, litData };
		}
		else
		{
			u32 id = byte & 0x1F;

			if (byte & 0x20)
			{
				byte = ReadData<u8>();
				id |= (byte & 0x7F) << 5;

				if (byte & 0x80)
				{
					byte = ReadData<u8>();
					id |= (byte & 0x7F) << 12;
					
					if (byte & 0x80)
					{
						byte = ReadData<u8>();
						id |= byte << 19;
					}
				}
			}

			TypeHandle type;
			if (typeStored)
			{
				type = DecodeType();
				if (m_IdTypeMapping.size() <= id)
					m_IdTypeMapping.resize(id + 1);
				m_IdTypeMapping[id] = type;
			}
			else
			{
				type = m_IdTypeMapping[id];
			}


			return ILVar{ varKind, id, type };
		}
	}
}
