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
		void Visit(ITrForRange& node) override;
		void Visit(ITrSwitch& node) override;
		void Visit(ITrLabel& node) override;
		void Visit(ITrBreak& node) override;
		void Visit(ITrContinue& node) override;
		void Visit(ITrFallthrough& node) override;
		void Visit(ITrGoto& node) override;
		void Visit(ITrReturn& node) override;
		void Visit(ITrThrow& node) override;
		
		void Visit(ITrDefer& node) override;

		// Done by ITrBlock
		//void Visit(ITrUnsafe& node) override;
		
		//void Visit(ITrCompCond& node) override;
		
		void Visit(ITrLocalVar& node) override;
		
		void Visit(ITrAssign& node) override;
		void Visit(ITrTernary& node) override;
		void Visit(ITrBinary& node) override;
		void Visit(ITrUnary& node) override;
		
		void Visit(ITrQualNameExpr& node) override;
		
		void Visit(ITrIndexSlice& node) override;
		
		void Visit(ITrFuncCall& node) override;
		
		
		void Visit(ITrMemberAccess& node) override;
		void Visit(ITrTupleAccess& node) override;
		
		void Visit(ITrLiteral& node) override;
		
		void Visit(ITrStructInit& node) override;
		void Visit(ITrUnionInit& node) override;
		
		void Visit(ITrAdtTupleEnumInit& node) override;
		void Visit(ITrAdtAggrEnumInit& node) override;
		
		void Visit(ITrTupleInit& node) override;
		void Visit(ITrArrayInit& node) override;
		
		void Visit(ITrCast& node) override;
		
		void Visit(ITrBlockExpr& node) override;
		// Handled by ITrBlockExpr
		//void Visit(ITrUnsafeExpr& node) override;
		
		//void Visit(ITrComma& node) override;
		//void Visit(ITrClosure& node) override;
		
		//void Visit(ITrMove& node) override;
		
		//void Visit(ITrIs& node) override;
		void Visit(ITrTry& node) override;
		//void Visit(ITrSpecKw& node) override;
		//void Visit(ITrCompRun& node) override;
		
		void Visit(ITrAttribs& node) override;
		//void Visit(ITrAtAttrib& node) override;
		//void Visit(ITrGenDecl& node) override;
		//void Visit(ITrGenTypeParam& node) override;
		//void Visit(ITrGenValParam& node) override;
		//void Visit(ITrGenTypeBound& node) override;
		//void Visit(ITrDefSPtr& def) override;
		//void Visit(ITrStmtSPtr& stmt) override;
		//void Visit(ITrExprSPtr& expr) override;
		//void Visit(ITrPatternSPtr& pattern) override;
		//void Visit(ITrBodySPtr& body) override;


	private:
		u32 AddNewBlock();
		void SetCurBlock(u32 label);

		void MapVar(IdenSPtr iden, ILVar var);

		ILVar CreateDstVar(TypeHandle type);
		ILVar PopTmpVar();

		QualNameSPtr GetCurScope();

		void ImplementCompilerIntrin(const StdString& intrinName);

		ILVar CreateLitVar(Token& lit, TypeHandle type);

		template<typename T>
		ILVar CreateLitVar(TypeHandle type, const T& val);

		template<typename T>
		void AddElem(T* elem, u32 label = u32(-1));
		template<typename T>
		void SetTerminal(T* terminal, u32 label = u32(-1));

		// Switch
		void ProcessSwitchGroup(ITrSwitch& node, ITrSwitchGroup& group);
		void ProcessSwitchBase(ITrSwitch& node, ITrSwitchGroup& group);

		void ProcessSwitchLeaf(ITrSwitch& node, ITrSwitchGroup& group);
		void ProcessRange(ITrSwitch& node, ITrSwitchGroup& group);
		void ProcessLitMatch(ITrSwitch& node, ITrSwitchGroup& group);
		void ProcessEnumMatch(ITrSwitch& node, ITrSwitchGroup& group);

		void ProcessTuple(ITrSwitch& node, ITrSwitchGroup& group);
		void ProcessTupleIndex(ITrSwitch& node, ITrSwitchGroup& group);
		void ProcessAggr(ITrSwitch& node, ITrSwitchGroup& group);
		void ProcessAggrMember(ITrSwitch& node, ITrSwitchGroup& group);
		void ProcessSlice(ITrSwitch& node, ITrSwitchGroup& group);
		void ProcessSliceIndex(ITrSwitch& node, ITrSwitchGroup& group);

		void ValueBindSwitchVar(const StdString& bindName, const StdVector<usize>& caseIds);
		void AssignValueBinds(usize caseId);

		void PopSwitchVarToDepth(usize depth);
		u32 AddCaseToBody(usize caseId);
		u32 AddCaseToTerm();
		
		ILModule* m_pILMod;

		ILFuncDefSPtr m_Def;
		ILBlock* m_pCurBlock;
		
		u32 m_CurLabel;
		u32 m_CurDeferLabel;
		u32 m_CurVarId;

		StdStack<ILVar> m_TmpVars;
		
		StdStack<ILVar> m_SwitchVars;
		usize m_CurSwitchPatternDepth;

		StdPairVector<usize, StdVector<u32>> m_CaseToBodyBlocks;
		StdVector<u32> m_CaseToTermBlocks;
		StdUnorderedMap<usize, StdPairVector<StdString, ILVar>> m_CaseBindings;
		
		FuncContextSPtr m_FuncCtx;
		QualNameSPtr m_FuncScope;
		StdVector<StdString> m_ScopeNames;

		StdUnorderedMap<IdenSPtr, StdVector<ILVar>> m_VarMapping;
		StdUnorderedMap<IdenSPtr, u32> m_LabelMapping;
		StdUnorderedMap<ITrBlock*, u32> m_ITrBlockMapping;
		StdStack<u32> m_LabelStack;
		StdStack<u32> m_LoopBeginLabels;
		StdStack<u32> m_LoopEndLabels;

		bool m_FallThrough;
		TypeHandle m_ErrType;
	};
	
}
