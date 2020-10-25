#include "misc-passes.hpp"


#include "common/name-mangling.hpp"
#include "common/qualname.hpp"
#include "itr/itr.hpp"
#include "module/symbol.hpp"

namespace Noctis
{
	NameManglePass::NameManglePass(Context* pCtx)
		: ITrSemanticPass("name mangle pass", pCtx)
	{
	}

	void NameManglePass::Process(ITrModule& mod)
	{
		SetModule(mod);

		Foreach(ITrVisitorDefKind::Any, [this](ITrFunc& node)
		{
			SymbolSPtr sym = node.sym.lock();
			sym->mangledName = NameMangling::Mangle(m_pCtx, sym);
		});
	}

	MarkingPass::MarkingPass(Context* pCtx)
		: ITrSemanticPass("marking pass", pCtx)
		, m_CompileTimeOnly(false)
		, m_DependsOnValueGenerics(false)
	{
	}

	void MarkingPass::Process(ITrModule& mod)
	{
		SetModule(mod);
		
		Foreach(ITrVisitorDefKind::Module, [&](ITrFunc& node)
		{
			SymbolSPtr sym = node.sym.lock();
			Mark(node, node.attribs ? node.attribs->attribs : Attribute::None);
			ITrBodySPtr body = mod.GetBody(node);
			ITrVisitor::Visit(body);
		});

		Foreach(ITrVisitorDefKind::Any, [&](ITrImpl& node)
		{
			SymbolSPtr sym = node.sym.lock();
			Mark(node, node.attribs ? node.attribs->attribs : Attribute::None);
			ITrBodySPtr body = mod.GetBody(node);
			ITrVisitor::Visit(body);
		});
	}

	void MarkingPass::Visit(ITrStruct& node)
	{
		SymbolSPtr sym = node.sym.lock();
		Mark(node, node.attribs ? node.attribs->attribs : Attribute::None);
		ITrBodySPtr body = m_pMod->GetBody(node);
		ITrVisitor::Visit(body);
	}

	void MarkingPass::Visit(ITrUnion& node)
	{
		SymbolSPtr sym = node.sym.lock();
		Mark(node, node.attribs ? node.attribs->attribs : Attribute::None);
		ITrBodySPtr body = m_pMod->GetBody(node);
		ITrVisitor::Visit(body);
	}

	void MarkingPass::Visit(ITrValEnum& node)
	{
		SymbolSPtr sym = node.sym.lock();
		Mark(node, node.attribs ? node.attribs->attribs : Attribute::None);
		ITrBodySPtr body = m_pMod->GetBody(node);
		ITrVisitor::Visit(body);
	}

	void MarkingPass::Visit(ITrAdtEnum& node)
	{
		SymbolSPtr sym = node.sym.lock();
		Mark(node, node.attribs ? node.attribs->attribs : Attribute::None);
		ITrBodySPtr body = m_pMod->GetBody(node);
		ITrVisitor::Visit(body);
	}

	void MarkingPass::Visit(ITrVar& node)
	{
		SymbolSPtr sym = node.sym.lock();
		Mark(node, node.attribs ? node.attribs->attribs : Attribute::None);
		ITrBodySPtr body = m_pMod->GetBody(node);
		ITrVisitor::Visit(body);
	}

	void MarkingPass::Visit(ITrFunc& node)
	{
		SymbolSPtr sym = node.sym.lock();
		Mark(node, node.attribs ? node.attribs->attribs : Attribute::None);
		ITrBodySPtr body = m_pMod->GetBody(node);
		ITrVisitor::Visit(body);
	}

	void MarkingPass::Mark(ITrDef& def, Attribute attribs)
	{
		SymbolSPtr sym = def.sym.lock();
		
		sym->comptimeOnly = m_CompileTimeOnly;
		if (!m_CompileTimeOnly && ENUM_IS_SET(attribs, Attribute::Comptime))
			sym->comptimeOnly = m_CompileTimeOnly = true;

		sym->dependsOnValueGenerics = m_DependsOnValueGenerics;
		if (!m_DependsOnValueGenerics)
		{
			if (sym->qualName)
			{
				IdenSPtr iden = sym->qualName->LastIden();
				for (IdenGeneric& idenGen : iden->Generics())
				{
					if (!idenGen.isType)
					{
						sym->dependsOnValueGenerics = m_DependsOnValueGenerics = true;
						break;
					}
				}
			}
			else if (def.genDecl)
			{
				for (ITrGenParamSPtr param : def.genDecl->params)
				{
					if (!param->isType)
					{
						sym->dependsOnValueGenerics = m_DependsOnValueGenerics = true;
						break;
					}
				}
			}
		}
	}
}
