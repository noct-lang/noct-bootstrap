#pragma once
#include "semantic/semantic-pass.hpp"

namespace Noctis
{
	FWDECL_CLASS_SPTR(Iden);
	FWDECL_CLASS_SPTR(QualName);
	FWDECL_STRUCT_SPTR(ITrGenDecl);
	FWDECL_STRUCT_SPTR(Symbol);
	
	class TypeCollection : public ITrSemanticPass
	{
	public:
		TypeCollection(Context* pCtx);

		void Process(ITrModule& mod) override;

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
		

		void HandleGenerics(QualNameSPtr baseQualName, ITrGenDeclSPtr decl);
		bool HandleImpls(SymbolSPtr sym);

	private:
		StdStack<SymbolSPtr> m_Syms;
		StdVector<SymbolSPtr> m_Interfaces;
		
		bool m_ProcessImplSym;

		bool m_InImpl;
		QualNameSPtr m_TypeQualName;
		QualNameSPtr m_ImplQualName;
	};

}
