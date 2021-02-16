#pragma once
#include "semantic/semantic-pass.hpp"
#include "common/type.hpp"
#include "semantic/ast/misc/iden-scope-pass.hpp"
#include "tokens/span.hpp"

namespace Noctis
{
	FWDECL_CLASS_SPTR(Iden);
	FWDECL_CLASS_SPTR(QualName);
	FWDECL_STRUCT_SPTR(ITrGenDecl);
	FWDECL_STRUCT_SPTR(Symbol);
	FWDECL_STRUCT_SPTR(SymbolInst);
	
	FWDECL_STRUCT_SPTR(ITrImpl);

	class TypeCollectionCommon : public ITrSemanticPass
	{
	public:
		TypeCollectionCommon(const char* name, Context* pCtx);

		void Visit(ITrStruct& node) override;
		void Visit(ITrUnion& node) override;
		void Visit(ITrValEnum& node) override;
		void Visit(ITrValEnumMember& node) override;
		void Visit(ITrAdtEnum& node) override;
		void Visit(ITrAdtEnumMember& node) override;
		void Visit(ITrTypealias& node) override;
		void Visit(ITrTypedef& node) override;
		void Visit(ITrFunc& node) override;
		
		void Visit(ITrVar& node) override;

	protected:

		virtual bool HandleImpls(SymbolSPtr sym);

		StdStack<SymbolSPtr> m_Syms;

		TypeHandle m_ImplType;

		bool m_InImpl;
		bool m_InInterface;
		QualNameSPtr m_TypeQualName;
		QualNameSPtr m_ImplQualName;

		ITrDefSPtr m_Impl;
	};
	
	class TypeCollection : public TypeCollectionCommon
	{
	public:
		TypeCollection(Context* pCtx);

		void Process(ITrModule& mod) override;
	};

	class ImplCollection : public TypeCollectionCommon
	{
	public:
		ImplCollection(Context* pCtx);

		void Process(ITrModule& mod) override;

	private:
		bool HandleImpls(SymbolSPtr sym) override;
		
		void CollectInterfaces(SymbolSPtr sym, QualNameSPtr nodeQualName, const StdPairVector<QualNameSPtr, SpanId>& implInterfaces);
		void CollectInterfaces(SymbolSPtr sym, QualNameSPtr nodeQualName, const StdPair<QualNameSPtr, SpanId>& implInterface);
		void CollectNeededChildren(SymbolInstSPtr ifaceInst);
		void AddMissingChildrenWithDefImpl();

		StdVector<SymbolInstSPtr> m_Interfaces;
		StdUnorderedMap<IdenSPtr, StdPair<SymbolSPtr, SymbolInstSPtr>> m_NeededChildren;
		SymbolSPtr m_ImplSymbol;
	};

	class GenericTypeCollection : public ITrSemanticPass
	{
	public:
		GenericTypeCollection(Context* pCtx);

		void Process(ITrModule& mod) override;

	private:
		void HandleGenerics(QualNameSPtr qualName, ITrGenDeclSPtr decl, ITrDef& def);
		SymbolSPtr m_Sym;
	};
}
