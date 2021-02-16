#pragma once
#include "itr/itr.hpp"
#include "semantic/semantic-pass.hpp"

namespace Noctis
{
	struct ITrSwitchGroup;
	struct TypeHandle;
	enum class Attribute : u8;
	FWDECL_STRUCT_SPTR(Symbol);
	
	class NameManglePass : public ITrSemanticPass
	{
	public:
		NameManglePass(Context* pCtx);
		
		void Process(ITrModule& mod) override;
	};

	class MarkingPass : public ITrSemanticPass
	{
	public:
		MarkingPass(Context* pCtx);

		void Process(ITrModule& mod) override;

		void Visit(ITrStruct& node) override;
		void Visit(ITrUnion& node) override;
		void Visit(ITrValEnum& node) override;
		void Visit(ITrAdtEnum& node) override;
		void Visit(ITrVar& node) override;
		void Visit(ITrFunc& node) override;
		
	private:

		void Mark(ITrDef& def, Attribute attribs);

		bool m_CompileTimeOnly;
		bool m_DependsOnValueGenerics;
	};

	class SwitchProcessPass : public ITrSemanticPass
	{
	public:
		SwitchProcessPass(Context* pCtx);

		void Process(ITrModule& mod) override;

		void Visit(ITrSwitch& node) override;

	private:

		StdVector<ITrPatternSPtr> SplitEither(ITrPatternSPtr pattern);

		ITrSwitchGroup CreateGroup(ITrPatternSPtr pattern, TypeHandle& type, usize caseId, usize depth = 1);

		ITrSwitchGroup CreateLeaf(ITrPatternSPtr pattern, TypeHandle& type, usize caseId, usize depth);
		ITrSwitchGroup CreateRange(ITrPatternSPtr pattern, TypeHandle& type, usize caseId, usize depth);
		ITrSwitchGroup CreateLitMatch(ITrPatternSPtr pattern, TypeHandle& type, usize caseId, usize depth);

		ITrSwitchGroup CreateEnum(ITrPatternSPtr pattern, TypeHandle& type, usize caseId, usize depth);
		
		ITrSwitchGroup CreateEnumMatch(ITrPatternSPtr pattern, TypeHandle& type, usize caseId, usize depth);
		ITrSwitchGroup CreateTupleEnumMatch(ITrPatternSPtr pattern, TypeHandle& type, usize caseId, usize depth);
		ITrSwitchGroup CreateAggrEnumMatch(ITrPatternSPtr pattern, TypeHandle& type, usize caseId, usize depth);


		ITrSwitchGroup CreateTuple(ITrPatternSPtr pattern, TypeHandle& type, usize caseId, usize depth);
		ITrSwitchGroup CreateAggr(ITrPatternSPtr pattern, TypeHandle& type, usize caseId, usize depth);

		ITrSwitchGroup CreateSlice(ITrPatternSPtr pattern, TypeHandle& type, usize caseId, usize depth);

		ITrSwitchGroup& GetSecondToLastSubgroup(ITrSwitchGroup& group);

		void MergeGroups(ITrSwitchGroup& baseGroup, ITrSwitchGroup& subGroup);

		u64 GetLitVal(Token& tok);
	};

	class CopyCheckPass : public ITrSemanticPass
	{
	public:
		CopyCheckPass(Context* pCtx);

		void Process(ITrModule& mod) override;

		void Visit(ITrAssign& node) override;

	private:
		bool CheckCopyable(TypeHandle typeInfo, BoundsInfo& boundsInfo);

		BoundsInfo* m_pBoundsInfo;
	};

	class ErrHandlerCollectionPass : public ITrSemanticPass
	{
	public:
		ErrHandlerCollectionPass(Context* pCtx);

		void Process(ITrModule& mod) override;;

		void Visit(ITrErrHandler& node) override;

	private:
		FuncContextSPtr m_FuncCtx;
	};

	class TryCheckPass : public ITrSemanticPass
	{
	public:
		TryCheckPass(Context* pCtx);

		void Process(ITrModule& mod) override;

		void Visit(ITrTry& node) override;

	private:
		FuncContextSPtr m_FuncCtx;
		TypeHandle m_ErrorHandle;
	};
}
