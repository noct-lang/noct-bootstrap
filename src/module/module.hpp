#pragma once
#include "common/defs.hpp"
#include "macro.hpp"
#include "ast/ast.hpp"
#include "graph.hpp"
#include "il/il.hpp"
#include "itr/itr.hpp"
#include "symbol.hpp"

namespace Noctis
{

#pragma pack(push, 1)
	struct ModuleHeaderFields
	{
		char magic[4];
		u8 version;
		u8 reserved[3];
		u64 size;
		u32 idenSize;
		u32 importsSize;
	};
#pragma pack(pop)

	struct ModuleHeader
	{
		ModuleHeaderFields fields;
		StdVector<QualNameSPtr> imports;
		u8 numSections;
	};

	struct ModuleEncodeInfo
	{
		StdVector<StdString> names;
		StdUnorderedMap<StdString, u32> nameIdMapping;
		
		StdUnorderedMap<StdString, StdPair<QualNameSPtr, BoundsInfo>> toQualNameMapping;
		StdUnorderedMap<QualNameSPtr, StdString> fromQualNameMapping;
		
		StdUnorderedMap<StdString, TypeHandle> toTypeMapping;
		StdUnorderedMap<TypeHandle, StdString> fromTypeMapping;

		ModuleEncodeInfo();

		const StdString& GetNameFromId(u32 id);
		u32 GetOrAddName(const StdString& name);
		u32 GetNameId(const StdString& name);

		QualNameSPtr GetQualNameFromId(Context* pCtx, u32 id, BoundsInfo* pBoundsInfo);
		u32 GetOrAddQualName(Context* pCtx, QualNameSPtr qualName, BoundsInfo* pBoundsInfo);
		u32 GetQualNameId(QualNameSPtr qualName);

		TypeHandle GetTypeFromId(Context* pCtx, u32 id);
		u32 GetOrAddType(Context* pCtx, TypeHandle handle);
	};
	
	FWDECL_STRUCT_SPTR(Module);
	struct Module
	{
		Module(QualNameSPtr qualName, Context* pCtx);

		StdVector<AstTree> trees;
		MacroContext macroCtx;
		ITrModule itrModule;

		ModuleSymbolTable symTable;
		StdUnorderedSet<SymbolSPtr> comptimeSymbols;

		DependencyGraph dependencyGraph;

		StdUnorderedMap<QualNameSPtr, ModuleSPtr> imports;

		OperatorTable opTable;

		ILModule ilMod;
		
		QualNameSPtr qualName;

		ModuleHeader header;
		StdString filePath;
		ModuleEncodeInfo encodeInfo;
		
		bool isDecoded : 1;
		bool isImported : 1;
	};
	
}
