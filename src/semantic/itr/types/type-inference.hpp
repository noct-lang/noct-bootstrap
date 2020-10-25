#pragma once
#include "common/type.hpp"
#include "module/function.hpp"
#include "module/symbol.hpp"
#include "semantic/semantic-pass.hpp"

namespace Noctis
{
	FWDECL_CLASS_SPTR(QualName);
	FWDECL_STRUCT_SPTR(FuncContext);
	FWDECL_STRUCT_SPTR(ITrGenDecl);
	FWDECL_STRUCT_SPTR(ITrImpl);
	FWDECL_STRUCT_SPTR(ITrGenTypeBound);

	

	// Function type inference prepass
	class TypeInference : public ITrSemanticPass
	{
	public:
		TypeInference(Context* pCtx, bool prepass);

		void Process(ITrModule& mod) override;
		
		void Visit(ITrBlock& node) override;
		void Visit(ITrReturn& node) override;

		void Visit(ITrLocalVar& node) override;
		void Visit(ITrAssign& node) override;
		void Visit(ITrTernary& node) override;
		void Visit(ITrBinary& node) override;
		void Visit(ITrUnary& node) override;
		void Visit(ITrQualNameExpr& node) override;
		void Visit(ITrIndexSlice& node) override;
		void Visit(ITrExprSPtr& ptr, ITrAmbiguousCall node) override;
		void Visit(ITrFuncCall& node) override;
		void Visit(ITrAdtTupleEnumInit& node) override;
		void Visit(ITrMemberAccess& node) override;
		void Visit(ITrTupleAccess& node) override;
		void Visit(ITrLiteral& node) override;
		void Visit(ITrExprSPtr& ptr, ITrAmbiguousAggrInit node) override;
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

		// TODO: Patterns

	private:
		void HandleGenerics(ITrDef& def, IdenSPtr iden);
		void HandleAssocTypes(ITrGenBoundType& boundType, GenTypeInfo& genInfo);

		GenTypeInfo GetSubTypeInfo(const TypeInfo& srcInfo, TypeHandle interfaceType, const StdString& subTypeName);
		void NarrowGenBound(TypeInfo& typeInfo);
		
		QualNameSPtr GetCurScope();

		void Expect(TypeHandle handle);
		void ExpectNone();
		
		QualNameSPtr m_Scope;
		StdVector<StdString> m_ScopeNames;
		FuncContextSPtr m_FuncCtx;
		QualNameSPtr m_InterfaceQualname;
		StdVector<QualNameSPtr> m_SubInterfaceQualNames;

		TypeHandle m_SelfType;

		SymbolSPtr m_Sym;

		ITrDefSPtr m_Impl;

		TypeHandle m_ReturnHandle;
		TypeHandle m_ExpectedHandle;

		StdUnorderedMap<IdenSPtr, TypeHandle> m_GenMapping;

		bool m_Prepass;

		// Debug info
		StdString m_DebugMethodName;
	};
}
