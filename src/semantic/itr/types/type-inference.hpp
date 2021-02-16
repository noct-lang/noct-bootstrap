#pragma once
#include "common/type.hpp"
#include "module/function.hpp"
#include "module/symbol.hpp"
#include "semantic/semantic-pass.hpp"

namespace Noctis
{
	struct ITrSwitchCase;
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

		void Visit(ITrErrHandler& node) override;
		
		void Visit(ITrBlock& node) override;
		void Visit(ITrForRange& node) override;
		void Visit(ITrSwitch& node) override;
		void Visit(ITrReturn& node) override;
		void Visit(ITrLocalVar& node) override;
		void Visit(ITrThrow& node) override;

		void Visit(ITrExprSPtr& node) override;
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

		void Visit(ITrAdtAggrEnumPattern& node) override;
		void Visit(ITrAdtTupleEnumPattern& node) override;
		void Visit(ITrAggrPattern& node) override;
		void Visit(ITrLiteralPattern& node) override;
		void Visit(ITrPatternSPtr& ptr, ITrAmbiguousAggrPattern& node) override;
		void Visit(ITrSlicePattern& node) override;
		void Visit(ITrTuplePattern& node) override;
		void Visit(ITrTypePattern& node) override;
		void Visit(ITrValueBindPattern& node) override;
		void Visit(ITrValueEnumPattern& node) override;

	private:

		TypeHandle InferType(TypeHandle handle, TypeMod mod);
		QualNameSPtr InferQualNameGenerics(QualNameSPtr origQualName);
		IdenSPtr InferIdenGenerics(IdenSPtr origIden);
		TypeDisambiguationSPtr InferDisambigGenerics(TypeDisambiguationSPtr origDisambig);
		
		void HandleGenerics(ITrDef& def, IdenSPtr iden);
		void HandleAssocTypes(TypeHandle srcType, ITrGenBoundType& boundType);

		void UpdateInstantiations(SymbolSPtr sym);
		
		QualNameSPtr GetCurScope();

		void Expect(TypeHandle handle);
		void ExpectNone();
		
		QualNameSPtr m_Scope;
		StdVector<StdString> m_ScopeNames;
		FuncContextSPtr m_FuncCtx;
		QualNameSPtr m_InterfaceQualname;
		StdVector<QualNameSPtr> m_SubInterfaceQualNames;

		TypeHandle m_SelfType;
		TypeHandle m_ErrType;

		SymbolSPtr m_Sym;

		ITrGenDeclSPtr m_GenDecl;
		ITrDefSPtr m_Impl;

		TypeHandle m_ReturnHandle;
		TypeHandle m_ErrorHandle;
		TypeHandle m_ExpectedHandle;

		StdUnorderedMap<IdenSPtr, TypeHandle> m_GenMapping;
		BoundsInfo* m_pBoundsInfo;

		bool m_Prepass;

		// Debug info
		StdString m_DebugMethodName;
	};
}
