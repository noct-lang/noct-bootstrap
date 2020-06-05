#pragma once
#include "common/defs.hpp"
#include "module/module.hpp"

namespace Noctis
{
	FWDECL_STRUCT_SPTR(Symbol);
	
	struct Context;
	struct Module;

#pragma pack(push, 1)
	struct ModuleSectionHeader
	{
		char iden[4];
		u64 size;
	};
#pragma pack(pop)

	enum class SymbolLinkKind : u8
	{
		TypeParent, // symbols with types as parent
		InterfaceImpl, // Interface implementations per symbol
		Variants,
		InterfaceExt,
		MemberOrder,
	};

	class ModuleEncode
	{
	public:
		ModuleEncode(Context* pCtx);

		StdVector<u8> Encode(Module& mod);

	private:

		u32 GetOrAddName(const StdString& name);

		void EncodeImportSection();
		
		void EncodeSymSection();
		void EncodeSLnkSection();

		template<typename T>
		void WriteData(StdVector<u8>& insertTo, const T& data);
		void WriteName(StdVector<u8>& insertTo, const StdString& name);

		const StdString& GetMangledQualName(QualNameSPtr qualName);
		const StdString& GetMangledType(TypeHandle type);

		Context* m_pCtx;
		Module* m_pMod;

		u32 m_CurNameId;
		StdVector<u8> m_NameSection;
		StdUnorderedMap<StdString, u32> m_NameMapping;

		StdVector<u8> m_ImportSection;
		StdVector<u8> m_SymSection;
		StdVector<u8> m_SLnkSection;

		StdUnorderedMap<QualNameSPtr, StdString> m_QualNameMangleCache;
		StdUnorderedMap<TypeSPtr, StdString> m_TypeMangleCache;
	};


	template <typename T>
	void ModuleEncode::WriteData(StdVector<u8>& insertTo, const T& data)
	{
		const u8* addr = reinterpret_cast<const u8*>(&data);
		insertTo.insert(insertTo.end(), addr, addr + sizeof(T));
	}


	class ModuleDecode
	{
	public:
		ModuleDecode(Context* pCtx);

		ModuleSPtr CreateModuleWithHeader(const StdString& filePath);
		
		void Decode(Module& mod);

	private:
		void Reset();

		void DecodeImport(const ModuleSectionHeader& header);
		void DecodeName(const ModuleSectionHeader& header);
		void DecodeSyms(const ModuleSectionHeader& header);
		void DecodeSLnk(const ModuleSectionHeader& header);
		void DecodeILBC(const ModuleSectionHeader& header);

		void ParentSymbols();

		template<typename T>
		const T& ReadData();
		const StdString& ReadName();

		QualNameSPtr GetQualNameFromMangle(const StdString& mangle);
		TypeHandle GetTypeFromMangle(const StdString& mangle);

		Context* m_pCtx;
		Module* m_pMod;

		const StdVector<u8>* m_pData;
		u64 m_DataPos;

		StdVector<StdString> m_Names;

		StdUnorderedMap<QualNameSPtr, SymbolSPtr> m_Syms;
		StdUnorderedMap<QualNameSPtr, StdUnorderedMap<QualNameSPtr, SymbolSPtr>> m_ImplSyms;

		StdUnorderedMap<StdString, QualNameSPtr> m_QualNameMangleCache;
		StdUnorderedMap<StdString, TypeHandle> m_TypeMangleCache;
	};

	template <typename T>
	const T& ModuleDecode::ReadData()
	{
		u64 tmp = m_DataPos;
		m_DataPos += sizeof(T);
		return *reinterpret_cast<const T*>(&(*m_pData)[tmp]);
	}
}
