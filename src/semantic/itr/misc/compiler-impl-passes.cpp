#include "compiler-impl-passes.hpp"



#include "common/context.hpp"
#include "common/name-mangling.hpp"
#include "itr/itr.hpp"
#include "module/module.hpp"
#include "module/symbol.hpp"

namespace Noctis
{
	CompilerImplPass::CompilerImplPass(Context* pCtx)
		: ITrSemanticPass("compiler implementation pass", pCtx)
	{
	}

	void CompilerImplPass::Process(ITrModule& mod)
	{
		SetModule(mod);

		ModuleSymbolTable& symTable = m_pCtx->activeModule->symTable;

		QualNameSPtr structMarkerQualName = QualName::Create({ "core", "marker", "Struct" });
		SymbolSPtr structMarkerSym = symTable.Find(nullptr, structMarkerQualName);

		Foreach(ITrVisitorDefKind::Any, [&](ITrStruct& node)
		{
			SymbolSPtr sym = node.sym.lock();

			if (structMarkerSym)
				sym->markers.push_back(structMarkerSym);
		});

		QualNameSPtr unionMarkerQualName = QualName::Create({ "core", "marker", "Union" });
		SymbolSPtr unionMarkerSym = symTable.Find(nullptr, unionMarkerQualName);

		Foreach(ITrVisitorDefKind::Any, [&](ITrUnion& node)
		{
			SymbolSPtr sym = node.sym.lock();

			if (unionMarkerQualName)
				sym->markers.push_back(unionMarkerSym);
		});

		QualNameSPtr valEnumMarkerQualName = QualName::Create({ "core", "marker", "ValEnum" });
		SymbolSPtr valEnumMarkerSym = symTable.Find(nullptr, valEnumMarkerQualName);

		Foreach(ITrVisitorDefKind::Any, [&](ITrValEnum& node)
		{
			SymbolSPtr sym = node.sym.lock();

			if (valEnumMarkerSym)
				sym->markers.push_back(valEnumMarkerSym);
		});

		QualNameSPtr adtEnumMarkerQualName = QualName::Create({ "core", "marker", "AdtEnum" });
		SymbolSPtr adtEnumMarkerSym = symTable.Find(nullptr, adtEnumMarkerQualName);

		Foreach(ITrVisitorDefKind::Any, [&](ITrAdtEnum& node)
		{
			SymbolSPtr sym = node.sym.lock();

			if (adtEnumMarkerSym)
				sym->markers.push_back(adtEnumMarkerSym);
		});

		QualNameSPtr strongInterfaceMarkerQualName = QualName::Create({ "core", "marker", "StrongInterface" });
		SymbolSPtr strongInterfaceMarkerSym = symTable.Find(nullptr, strongInterfaceMarkerQualName);

		Foreach(ITrVisitorDefKind::Any, [&](ITrStrongInterface& node)
		{
			SymbolSPtr sym = node.sym.lock();

			if (strongInterfaceMarkerSym)
				sym->markers.push_back(strongInterfaceMarkerSym);
		});

		QualNameSPtr weakInterfaceMarkerQualName = QualName::Create({ "core", "marker", "WeakInterface" });
		SymbolSPtr  weakInterfaceMarkerSym = symTable.Find(nullptr, weakInterfaceMarkerQualName);

		Foreach(ITrVisitorDefKind::Any, [&](ITrWeakInterface& node)
		{
			SymbolSPtr sym = node.sym.lock();

			if (weakInterfaceMarkerSym)
				sym->markers.push_back(weakInterfaceMarkerSym);
		});

		QualNameSPtr markerInterfaceMarkerQualName = QualName::Create({ "core", "marker", "MarkerInterface" });
		SymbolSPtr markerInterfaceMarkerSym = symTable.Find(nullptr, markerInterfaceMarkerQualName);

		Foreach(ITrVisitorDefKind::Any, [&](ITrMarkerInterface& node)
		{
			SymbolSPtr sym = node.sym.lock();

			if (markerInterfaceMarkerSym)
				sym->markers.push_back(markerInterfaceMarkerSym);
		});

		QualNameSPtr typealiasMarkerQualName = QualName::Create({ "core", "marker", "MarkerInterface" });
		SymbolSPtr typealiasMarkerSym = symTable.Find(nullptr, typealiasMarkerQualName);

		Foreach(ITrVisitorDefKind::Any, [&](ITrTypealias& node)
		{
			SymbolSPtr sym = node.sym.lock();

			if (typealiasMarkerSym)
				sym->markers.push_back(typealiasMarkerSym);
		});

		QualNameSPtr typedefMarkerQualName = QualName::Create({ "core", "marker", "MarkerInterface" });
		SymbolSPtr typedefInterfaceMarkerSym = symTable.Find(nullptr, typedefMarkerQualName);

		Foreach(ITrVisitorDefKind::Any, [&](ITrTypedef& node)
		{
			SymbolSPtr sym = node.sym.lock();

			if (typedefInterfaceMarkerSym)
				sym->markers.push_back(typedefInterfaceMarkerSym);
		});
	}

	ImplEliminationPass::ImplEliminationPass(Context* pCtx)
		: ITrSemanticPass("impl elimination pass", pCtx)
	{
	}

	void ImplEliminationPass::Process(ITrModule& mod)
	{
		SetModule(mod);

		StdUnorderedSet<SymbolSPtr> processedSyms;
		Foreach(ITrVisitorDefKind::Any, [&, this](ITrImpl& node)
		{	
			SymbolSPtr sym = node.sym.lock();

			auto it = processedSyms.find(sym);
			if (it != processedSyms.end())
				return;
			processedSyms.emplace(sym);
			
			TypeHandle type = node.type->handle;
			ModuleSymbolTable& symTable = m_pCtx->activeModule->symTable;

			QualNameSPtr parentQualName;
			if (type.Kind() == TypeKind::Iden)
			{
				parentQualName = type.AsIden().qualName;
			}
			else
			{
				StdString typeName = NameMangling::Mangle(m_pCtx, type);
				parentQualName = QualName::Create(typeName);
			}

			usize implDepth = node.qualName->Depth();
			sym->children->Foreach([&](SymbolSPtr child, QualNameSPtr)
			{
				if (child->qualName->Depth() <= implDepth)
					return;

				QualNameSPtr subName = child->qualName->GetSubName(node.qualName);
				if (!subName)
					return;
				
				QualNameSPtr qualName = parentQualName->Append(subName);
				child->qualName = qualName;
			});

			SymbolSPtr newParent;
			if (type.Kind() == TypeKind::Iden)
			{	
				newParent = symTable.Find(parentQualName);
			}
			else
			{
				newParent = symTable.Find(type);
				if (!newParent)
				{
					StdString typeName = NameMangling::Mangle(m_pCtx, type);
					newParent = CreateSymbol(m_pCtx, SymbolKind::Type, parentQualName);
					newParent->type = type;
					
					symTable.Add(newParent);
				}
			}
			newParent->children->Merge(sym->children);
			
		});
	}
}
