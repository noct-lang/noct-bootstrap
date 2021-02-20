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
		char iden[4] = { '=', '=', '=', '=' };
		u64 size = 0;
	};
#pragma pack(pop)

	struct ModuleSection
	{
		ModuleSectionHeader header;
		StdVector<u8> data;

		void WriteTo(StdVector<u8>& buffer);
	};

	enum class SymbolLinkKind : u8
	{
		InterfaceImpl, // Interface implementations per symbol
		TypeImpl,
		MemberOrder,
	};

	class ModuleEncode
	{
	public:
		ModuleEncode();

		StdVector<u8> Encode(Module& mod);

	private:

		u32 GetOrAddName(const StdString& name);

		u32 EncodeImports(StdVector<u8>& encoded);

		void EncodeNameSection();
		void EncodeSymSection();
		void EncodeInstSection();
		void EncodeSLnkSection();
		void EncodeILSection();

		template<typename T>
		void WriteData(StdVector<u8>& insertTo, const T& data);
		void WriteName(StdVector<u8>& insertTo, const StdString& name);
		void WriteSymbolId(StdVector<u8>& insertTo, SymbolSPtr sym);
		void WriteId(StdVector<u8>& insertTo, u32 id);
		void WriteType(StdVector<u8>& insertTo, TypeHandle type);
		void WriteQualName(StdVector<u8>& insertTo, QualNameSPtr qualName);

		const StdString& GetMangledQualName(QualNameSPtr qualName);
		const StdString& GetMangledType(TypeHandle type);

		Module* m_pMod;

		StdVector<ModuleSection> m_Sections;
		
		StdVector<u8> m_NameSection;

		BoundsInfo* m_pBoundsInfo;

		StdUnorderedMap<SymbolSPtr, u32> m_SymbolIdMapping;
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
		ModuleDecode();

		ModuleSPtr CreateModuleWithHeader(const StdString& filePath);
		
		void Decode(Module& mod);

	private:
		void Reset();

		void UpdateImports();
		void DecodeName(const ModuleSectionHeader& header);
		void DecodeSyms(const ModuleSectionHeader& header);
		void DecodeInst(const ModuleSectionHeader& header);
		void DecodeSLnk(const ModuleSectionHeader& header);
		void DecodeILBC(const ModuleSectionHeader& header);

		void ParentSyms();

		SymbolSPtr GetSymbol(QualNameSPtr qualName);

		template<typename T>
		const T& ReadData();
		u32 ReadId();

		QualNameSPtr ReadQualName();
		TypeHandle ReadType();

		QualNameSPtr GetQualNameFromId(u32 id);
		TypeHandle GetTypeFromId(u32 id);

		Module* m_pMod;

		const StdVector<u8>* m_pData;
		u64 m_DataPos;

		StdVector<SymbolSPtr> m_SymIdMapping;
		StdUnorderedMap<QualNameSPtr, SymbolSPtr> m_Syms;
		StdUnorderedMap<QualNameSPtr, StdUnorderedMap<QualNameSPtr, SymbolSPtr>> m_ImplSyms;

		StdUnorderedMap<QualNameSPtr, SymbolInstSPtr> m_Insts;

		BoundsInfo* m_pBoundsInfo;
	};

	template <typename T>
	const T& ModuleDecode::ReadData()
	{
		u64 tmp = m_DataPos;
		m_DataPos += sizeof(T);
		return *reinterpret_cast<const T*>(&(*m_pData)[tmp]);
	}
}
