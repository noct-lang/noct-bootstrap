#pragma once
#include "common/defs.hpp"
#include "common/type.hpp"
#include "il-visitor.hpp"

namespace Noctis
{
	struct ILVar;
	struct ILModule;

	FWDECL_STRUCT_SPTR(ILElem);

#pragma pack(push, 1)
	struct ILHeader
	{
		char magic[4];
		u8 version;
		u8 reserved[3];
		u64 fileSize;
		u64 typeTableSize;
		u64 nameTableSize;
	};
#pragma pack(pop)

	class ILEncode : public ILVisitor
	{
	public:
		ILEncode(Context* pCtx);
		
		StdVector<u8> Encode(ILModule& mod);
		
		void Visit(ILFuncDef& node) override;
		void Visit(ILBlock& node) override;
		void Visit(ILIf& node) override;
		void Visit(ILSwitch& node) override;
		void Visit(ILGoto& node) override;
		void Visit(ILReturn& node) override;
		void Visit(ILAssign& node) override;
		void Visit(ILPrimAssign& node) override;
		void Visit(ILPrimBinary& node) override;
		void Visit(ILPrimUnary& node) override;
		void Visit(ILPrimCast& node) override;
		void Visit(ILTernary& node) override;
		void Visit(ILTransmute& node) override;
		void Visit(ILIndex& node) override;
		void Visit(ILCompIntrin& node) override;
		void Visit(ILStaticCall& node) override;
		void Visit(ILDynamicCall& node) override;
		void Visit(ILIndirectCall& node) override;
		void Visit(ILMemberAccess& node) override;
		void Visit(ILTupleAccess& node) override;
		void Visit(ILStructInit& node) override;
		void Visit(ILUnionInit& node) override;
		void Visit(ILValEnumInit& node) override;
		void Visit(ILAdtEnumInit& node) override;
		void Visit(ILTupInit& node) override;
		void Visit(ILArrInit& node) override;

	private:
		template<typename T>
		void WriteData(const T& val);

		void EncodeVarAndType(ILVar& var);
		void EncodeVar(ILVar& var);
		void EncodeType(TypeHandle handle);

		u32 GetNameId(const StdString& name);

		StdUnorderedMap<TypeSPtr, u32> m_Types;
		StdUnorderedMap<StdString, u32> m_Names;
		
		StdVector<StdVector<u8>> m_EncodedFuncs;
		StdVector<u8>* m_pCurEncodingFunc;

		u32 m_TypeWidth;
	};

	template <typename T>
	void ILEncode::WriteData(const T& val)
	{
		m_pCurEncodingFunc->insert(m_pCurEncodingFunc->end(), 
								   reinterpret_cast<const u8*>(&val), 
								   reinterpret_cast<const u8*>(&val) + sizeof(T));
	}


	class ILDecode
	{
	public:
		ILDecode(Context* pCtx);

		ILModule Decode(const StdVector<u8>& bytecode);

	private:

		void DecodeTypeTable(u64 size);
		void DecodeNameTable(u64 size);
		
		void DecodeFunc();

		ILBlock DecodeBlock();
		ILElemSPtr DecodeElem();

		ILElemSPtr DecodeIf();
		ILElemSPtr DecodeSwitch();
		ILElemSPtr DecodeGoto();
		ILElemSPtr DecodeReturn(bool hasVal);
		ILElemSPtr DecodeAssign();
		ILElemSPtr DecodePrimAssign();
		ILElemSPtr DecodePrimBinary();
		ILElemSPtr DecodePrimUnary();
		ILElemSPtr DecodePrimCast();
		ILElemSPtr DecodeTernary();
		ILElemSPtr DecodeTransmute();
		ILElemSPtr DecodeIndex();
		ILElemSPtr DecodeCompIntrin();
		ILElemSPtr DecodeFuncCall(bool hasRet);
		ILElemSPtr DecodeMethodCall(bool hasRet);
		ILElemSPtr DecodeIndirectCall(bool hasRet);
		ILElemSPtr DecodeMemberAccess();
		ILElemSPtr DecodeTupleAccess();
		ILElemSPtr DecodeStructInit();
		ILElemSPtr DecodeUnionInit();
		ILElemSPtr DecodeValEnumInit();
		ILElemSPtr DecodeAdtEnumInit();
		ILElemSPtr DecodeTupInit();
		ILElemSPtr DecodeArrInit();


		TypeHandle DecodeType();
		ILVar DecodeVar(bool typeStored);

		template<typename T>
		T ReadData();

		template<typename T>
		T ReadData(usize numBytes);

		Context* m_pCtx;
		ILModule* m_pMod;

		const StdVector<u8>* m_pByteCode;
		u64 m_BCPos;

		StdVector<TypeHandle> m_Types;
		StdVector<StdString> m_Names;
		StdVector<TypeHandle> m_IdTypeMapping;

		u32 m_CurVarId;

		u8 m_TypeWidth;
		u8 m_VarWidth;
	};

	template <typename T>
	T ILDecode::ReadData()
	{
		u64 tmp = m_BCPos;
		m_BCPos += sizeof(T);
		return *reinterpret_cast<const T*>(&(*m_pByteCode)[tmp]);
	}

	template <typename T>
	T ILDecode::ReadData(usize numBytes)
	{
		u64 tmp = m_BCPos;
		m_BCPos += numBytes;
		return *reinterpret_cast<const T*>(&(*m_pByteCode)[tmp]);
	}
}
