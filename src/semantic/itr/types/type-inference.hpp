#pragma once
#include "common/type.hpp"
#include "module/symbol.hpp"
#include "semantic/semantic-pass.hpp"

namespace Noctis
{
	FWDECL_CLASS_SPTR(QualName);
	FWDECL_STRUCT_SPTR(FuncContext);
	FWDECL_STRUCT_SPTR(ITrGenDecl);

	// Function type inference prepass
	class TypeInference : public ITrSemanticPass
	{
	public:
		TypeInference(Context* pCtx);
		void SetPrepass();

		void Process(ITrModule& mod) override;

		
		void Visit(ITrBlock& node) override;

		void Visit(ITrLocalVar& node) override;
		void Visit(ITrAssign& node) override;
		void Visit(ITrTernary& node) override;
		void Visit(ITrBinary& node) override;
		void Visit(ITrUnary& node) override;
		void Visit(ITrQualNameExpr& node) override;
		void Visit(ITrIndexSlice& node) override;
		void Visit(ITrExprSPtr& ptr, ITrAmbiguousCall node) override;
		void Visit(ITrAdtTupleEnumInit& node) override;
		void Visit(ITrMemberAccess& node) override;
		void Visit(ITrTupleAccess& node) override;
		void Visit(ITrLiteral& node) override;
		void Visit(ITrExprSPtr& ptr, ITrAmbiguousAggrInit node) override;
		void Visit(ITrStructInit& node) override;
		void Visit(ITrTupleInit& node) override;
		void Visit(ITrArrayInit& node) override;
		void Visit(ITrCast& node) override;
		void Visit(ITrBlockExpr& node) override;
		void Visit(ITrUnsafeExpr& node) override;
		void Visit(ITrComma& node) override;
		void Visit(ITrClosure& node) override;
		void Visit(ITrMove& node) override;
		void Visit(ITrIs& node) override;
		void Visit(ITrTry& node) override;
		void Visit(ITrSpecKw& node) override;
		void Visit(ITrCompRun& node) override;

		void Visit(ITrType& node) override;

	private:
		void HandleGenerics(ITrGenDeclSPtr decl, IdenSPtr iden);
		
		QualNameSPtr GetCurScope();
		
		QualNameSPtr m_Scope;
		StdVector<StdString> m_ScopeNames;
		FuncContextSPtr m_FuncCtx;
		QualNameSPtr m_InterfaceQualname;
		bool m_Prepass;

		TypeHandle m_SelfType;

		SymbolSPtr m_Sym;
	};
	
}
