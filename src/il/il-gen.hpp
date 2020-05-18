#pragma once
#include "il.hpp"
#include "itr/itr.hpp"
#include "semantic/semantic-pass.hpp"

namespace Noctis
{
	FWDECL_STRUCT_SPTR(ILElem);
	FWDECL_STRUCT_SPTR(FuncContext);

	class ILGen : public ITrSemanticPass
	{
	public:
		ILGen(Context* pCtx);

		void Process(ITrModule& mod) override;

		void Visit(ITrBlock& node) override;
		void Visit(ITrIf& node) override;
		void Visit(ITrLoop& node) override;
		void Visit(ITrSwitch& node) override;
		void Visit(ITrLabel& node) override;
		void Visit(ITrBreak& node) override;
		void Visit(ITrContinue& node) override;
		void Visit(ITrFallthrough& node) override;
		void Visit(ITrGoto& node) override;
		void Visit(ITrReturn& node) override;
		void Visit(ITrThrow& node) override;
		
		//void Visit(ITrDefer& node) override;
		//void Visit(ITrUnsafe& node) override;
		//void Visit(ITrErrHandler& node) override;
		//void Visit(ITrCompCond& node) override;
		
		//void Visit(ITrLocalVar& node) override;
		void Visit(ITrAssign& node) override;
		void Visit(ITrTernary& node) override;
		void Visit(ITrBinary& node) override;
		void Visit(ITrUnary& node) override;
		
		void Visit(ITrQualNameExpr& node) override;
		
		//void Visit(ITrIndexSlice& node) override;
		//void Visit(ITrExprSPtr& ptr, ITrAmbiguousCall& node) override;
		//void Visit(ITrFuncCall& node) override;
		//void Visit(ITrAdtTupleEnumInit& node) override;
		//void Visit(ITrMemberAccess& node) override;
		//void Visit(ITrTupleAccess& node) override;
		
		void Visit(ITrLiteral& node) override;
		
		//void Visit(ITrExprSPtr& ptr, ITrAmbiguousAggrInit& node) override;
		//void Visit(ITrAggrInit& node) override;
		//void Visit(ITrAdtAggrEnumInit& node) override;
		//void Visit(ITrTupleInit& node) override;
		//void Visit(ITrArrayInit& node) override;
		//void Visit(ITrCast& node) override;
		//void Visit(ITrBlockExpr& node) override;
		//void Visit(ITrUnsafeExpr& node) override;
		//void Visit(ITrComma& node) override;
		//void Visit(ITrClosure& node) override;
		//void Visit(ITrMove& node) override;
		//void Visit(ITrIs& node) override;
		//void Visit(ITrTry& node) override;
		//void Visit(ITrSpecKw& node) override;
		//void Visit(ITrCompRun& node) override;
		//void Visit(ITrType& node) override;
		//void Visit(ITrPlaceholderPattern& node) override;
		//void Visit(ITrPatternSPtr& ptr, ITrAmbiguousIdenPattern& node) override;
		//void Visit(ITrValueBindPattern& node) override;
		//void Visit(ITrLiteralPattern& node) override;
		//void Visit(ITrRangePattern& node) override;
		//void Visit(ITrTuplePattern& node) override;
		//void Visit(ITrValueEnumPattern& node) override;
		//void Visit(ITrAdtTupleEnumPattern& node) override;
		//void Visit(ITrPatternSPtr& ptr, ITrAmbiguousAggrPattern& node) override;
		//void Visit(ITrAggrPattern& node) override;
		//void Visit(ITrAdtAggrEnumPattern& node) override;
		//void Visit(ITrSlicePattern& node) override;
		//void Visit(ITrEitherPattern& node) override;
		//void Visit(ITrTypePattern& node) override;
		//void Visit(ITrAttribs& node) override;
		//void Visit(ITrAtAttrib& node) override;
		//void Visit(ITrGenDecl& node) override;
		//void Visit(ITrGenTypeParam& node) override;
		//void Visit(ITrGenValParam& node) override;
		//void Visit(ITrGenBound& node) override;
		//void Visit(ITrDefSPtr& def) override;
		//void Visit(ITrStmtSPtr& stmt) override;
		//void Visit(ITrExprSPtr& expr) override;
		//void Visit(ITrPatternSPtr& pattern) override;
		//void Visit(ITrBodySPtr& body) override;


	private:
		void AddToParent(ILElemSPtr elem);

		ILVar PopTmpVar();

		ILModule* m_pILMod;

		ILFuncDefSPtr m_Def;
		StdStack<ILElemSPtr> m_Elems;

		StdStack<ILVar> m_TmpVars;

		u32 m_CurVarId;
		u32 m_CurLabelId;

		FuncContextSPtr m_FuncCtx;

		StdUnorderedMap<IdenSPtr, StdStack<ILVar>> m_VarMapping;
		StdUnorderedMap<IdenSPtr, u32> m_LabelMapping;
		StdStack<u32> m_LabelStack;
		StdStack<u32> m_LoopBeginLabels;
		StdStack<u32> m_LoopEndLabels;

		bool m_FallThrough;
	};
	
}
