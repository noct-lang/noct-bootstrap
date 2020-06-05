#pragma once
#include "common/defs.hpp"
#include "macro.hpp"
#include "ast/ast.hpp"
#include "il/il.hpp"
#include "itr/itr.hpp"
#include "symbol.hpp"

namespace Noctis
{

#pragma pack(push, 1)
	struct ModuleHeader
	{
		char magic[4];
		u8 version;
		u8 reserved[3];
		u64 size;
		u32 idenSize;

		u8 numSections;
	};
#pragma pack(pop)

	FWDECL_STRUCT_SPTR(Module);
	
	struct Module
	{
		Module(QualNameSPtr qualName, Context* pCtx);

		StdVector<AstTree> trees;
		MacroContext macroCtx;
		ITrModule itrModule;

		ModuleSymbolTable symTable;
		StdUnorderedSet<SymbolSPtr> comptimeSymbols;

		StdUnorderedMap<QualNameSPtr, ModuleSPtr> imports;

		OperatorTable opTable;

		ILModule ilMod;
		
		QualNameSPtr qualName;

		ModuleHeader header;
		StdString filePath;
		bool isDecoded : 1;
		bool isImported : 1;
	};
	
}
