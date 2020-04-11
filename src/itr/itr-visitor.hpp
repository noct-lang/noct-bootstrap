#pragma once
#include <functional>

#include "common/defs.hpp"

namespace Noctis
{
	struct ITrModule;
	FWDECL_STRUCT_SPTR(ITrDef);
	FWDECL_STRUCT_SPTR(ITrStmt);
	FWDECL_STRUCT_SPTR(ITrExpr);
	FWDECL_STRUCT_SPTR(ITrPattern);
	FWDECL_STRUCT_SPTR(ITrBody);

	struct ITrParam;
	
	struct ITrStruct;
	struct ITrUnion;
	struct ITrValEnum;
	struct ITrValEnumMember;
	struct ITrAdtEnum;
	struct ITrAdtEnumMember;
	struct ITrMarkerInterface;
	struct ITrStrongInterface;
	struct ITrWeakInterface;
	struct ITrTypealias;
	struct ITrTypedef;
	struct ITrVar;
	struct ITrFunc;
	struct ITrImpl;

	struct ITrBlock;
	struct ITrIf;
	struct ITrLoop;
	struct ITrSwitch;
	struct ITrLabel;
	struct ITrBreak;
	struct ITrContinue;
	struct ITrFallthrough;
	struct ITrGoto;
	struct ITrReturn;
	struct ITrThrow;
	struct ITrDefer;
	struct ITrUnsafe;
	struct ITrErrHandler;
	struct ITrCompCond;
	struct ITrLocalVar;

	struct ITrAssign;
	struct ITrTernary;
	struct ITrBinary;
	struct ITrUnary;
	struct ITrQualName;
	struct ITrIndexSlice;
	struct ITrAmbiguousCall;
	struct ITrFuncCall;
	struct ITrAdtTupleEnumInit;
	struct ITrMemberAccess;
	struct ITrTupleAccess;
	struct ITrLiteral;
	struct ITrAmbiguousAggrInit;
	struct ITrAggrInit;
	struct ITrAdtAggrEnumInit;
	struct ITrTupleInit;
	struct ITrArrayInit;
	struct ITrCast;
	struct ITrMove;
	struct ITrBlockExpr;
	struct ITrUnsafeExpr;
	struct ITrComma;
	struct ITrClosure;
	struct ITrIs;
	struct ITrTry;
	struct ITrSpecKw;
	struct ITrCompRun;

	struct ITrType;

	struct ITrPlaceholderPattern;
	struct ITrAmbiguousIdenPattern;
	struct ITrValueBindPattern;
	struct ITrLiteralPattern;
	struct ITrRangePattern;
	struct ITrTuplePattern;
	struct ITrValueEnumPattern;
	struct ITrAdtTupleEnumPattern;
	struct ITrAmbiguousAggrPattern;
	struct ITrAggrPattern;
	struct ITrAdtAggrEnumPattern;
	struct ITrSlicePattern;
	struct ITrEitherPattern;
	struct ITrTypePattern;

	struct ITrAttribs;
	struct ITrAtAttrib;

	struct ITrGenDecl;
	struct ITrGenTypeParam;
	struct ITrGenValParam;
	struct ITrGenBound;

	enum class ITrVisitorDefKind : u8
	{
		Module,
		Local,
		Any
	};
	
	class ITrVisitor
	{
	public:
		ITrVisitor(bool walkDefs = false);
		virtual ~ITrVisitor();

		void SetModule(ITrModule& mod) { m_pMod = &mod; }

		void Foreach(ITrVisitorDefKind kind, const std::function<void(ITrStruct&)>& func);
		void Foreach(ITrVisitorDefKind kind, const std::function<void(ITrUnion&)>& func);
		void Foreach(ITrVisitorDefKind kind, const std::function<void(ITrValEnum&)>& func);
		void Foreach(ITrVisitorDefKind kind, const std::function<void(ITrValEnumMember&)>& func);
		void Foreach(ITrVisitorDefKind kind, const std::function<void(ITrAdtEnum&)>& func);
		void Foreach(ITrVisitorDefKind kind, const std::function<void(ITrAdtEnumMember&)>& func);
		void Foreach(ITrVisitorDefKind kind, const std::function<void(ITrMarkerInterface&)>& func);
		void Foreach(ITrVisitorDefKind kind, const std::function<void(ITrWeakInterface&)>& func);
		void Foreach(ITrVisitorDefKind kind, const std::function<void(ITrStrongInterface&)>& func);
		void Foreach(ITrVisitorDefKind kind, const std::function<void(ITrTypealias&)>& func);
		void Foreach(ITrVisitorDefKind kind, const std::function<void(ITrTypedef&)>& func);
		void Foreach(ITrVisitorDefKind kind, const std::function<void(ITrVar&)>& func);
		void Foreach(ITrVisitorDefKind kind, const std::function<void(ITrFunc&)>& func);
		void Foreach(ITrVisitorDefKind kind, const std::function<void(ITrImpl&)>& func);
		
		virtual void Visit(ITrStruct& node);
		virtual void Visit(ITrUnion& node);
		virtual void Visit(ITrValEnum& node);
		virtual void Visit(ITrValEnumMember& node);
		virtual void Visit(ITrAdtEnum& node);
		virtual void Visit(ITrAdtEnumMember& node);
		virtual void Visit(ITrMarkerInterface& node);
		virtual void Visit(ITrStrongInterface& node);
		virtual void Visit(ITrWeakInterface& node);
		virtual void Visit(ITrTypealias& node);
		virtual void Visit(ITrTypedef& node);
		virtual void Visit(ITrVar& node);
		virtual void Visit(ITrFunc& node);
		virtual void Visit(ITrImpl& node);

		virtual void Visit(ITrBlock& node);
		virtual void Visit(ITrIf& node);
		virtual void Visit(ITrLoop& node);
		virtual void Visit(ITrSwitch& node);
		virtual void Visit(ITrLabel& node);
		virtual void Visit(ITrBreak& node);
		virtual void Visit(ITrContinue& node);
		virtual void Visit(ITrFallthrough& node);
		virtual void Visit(ITrGoto& node);
		virtual void Visit(ITrReturn& node);
		virtual void Visit(ITrThrow& node);
		virtual void Visit(ITrDefer& node);
		virtual void Visit(ITrUnsafe& node);
		virtual void Visit(ITrErrHandler& node);
		virtual void Visit(ITrCompCond& node);
		virtual void Visit(ITrLocalVar& node);

		virtual void Visit(ITrAssign& node);
		virtual void Visit(ITrTernary& node);
		virtual void Visit(ITrBinary& node);
		virtual void Visit(ITrUnary& node);
		virtual void Visit(ITrQualName& node);
		virtual void Visit(ITrIndexSlice& node);
		virtual void Visit(ITrExprSPtr& ptr, ITrAmbiguousCall& node);
		virtual void Visit(ITrFuncCall& node);
		virtual void Visit(ITrAdtTupleEnumInit& node);
		virtual void Visit(ITrMemberAccess& node);
		virtual void Visit(ITrTupleAccess& node);
		virtual void Visit(ITrLiteral& node);
		virtual void Visit(ITrExprSPtr& ptr, ITrAmbiguousAggrInit& node);
		virtual void Visit(ITrAggrInit& node);
		virtual void Visit(ITrAdtAggrEnumInit& node);
		virtual void Visit(ITrTupleInit& node);
		virtual void Visit(ITrArrayInit& node);
		virtual void Visit(ITrCast& node);
		virtual void Visit(ITrBlockExpr& node);
		virtual void Visit(ITrUnsafeExpr& node);
		virtual void Visit(ITrComma& node);
		virtual void Visit(ITrClosure& node);
		virtual void Visit(ITrMove& node);
		virtual void Visit(ITrIs& node);
		virtual void Visit(ITrTry& node);
		virtual void Visit(ITrSpecKw& node);
		virtual void Visit(ITrCompRun& node);

		virtual void Visit(ITrType& node);
		
		virtual void Visit(ITrPlaceholderPattern& node);
		virtual void Visit(ITrPatternSPtr& ptr, ITrAmbiguousIdenPattern& node);
		virtual void Visit(ITrValueBindPattern& node);
		virtual void Visit(ITrLiteralPattern& node);
		virtual void Visit(ITrRangePattern& node);
		virtual void Visit(ITrTuplePattern& node);
		virtual void Visit(ITrValueEnumPattern& node);
		virtual void Visit(ITrAdtTupleEnumPattern& node);
		virtual void Visit(ITrPatternSPtr& ptr, ITrAmbiguousAggrPattern& node);
		virtual void Visit(ITrAggrPattern& node);
		virtual void Visit(ITrAdtAggrEnumPattern& node);
		virtual void Visit(ITrSlicePattern& node);
		virtual void Visit(ITrEitherPattern& node);
		virtual void Visit(ITrTypePattern& node);

		virtual void Visit(ITrAttribs& node);
		virtual void Visit(ITrAtAttrib& node);

		virtual void Visit(ITrGenDecl& node);
		virtual void Visit(ITrGenTypeParam& node);
		virtual void Visit(ITrGenValParam& node);
		virtual void Visit(ITrGenBound& node);
		

		virtual void Visit(ITrDefSPtr& def);
		virtual void Visit(ITrStmtSPtr& stmt);
		virtual void Visit(ITrExprSPtr& expr);
		virtual void Visit(ITrPatternSPtr& pattern);
		virtual void Visit(ITrBodySPtr& body);
	

	protected:

		void Walk(ITrStruct& node);
		void Walk(ITrUnion& node);
		void Walk(ITrValEnum& node);
		void Walk(ITrValEnumMember& node);
		void Walk(ITrAdtEnum& node);
		void Walk(ITrAdtEnumMember& node);
		void Walk(ITrMarkerInterface& node);
		void Walk(ITrStrongInterface& node);
		void Walk(ITrWeakInterface& node);
		void Walk(ITrTypealias& node);
		void Walk(ITrTypedef& node);
		void Walk(ITrVar& node);
		void Walk(ITrFunc& node);
		void Walk(ITrImpl& node);
		
		void Walk(ITrBlock& node);
		void Walk(ITrIf& node);
		void Walk(ITrLoop& node);
		void Walk(ITrSwitch& node);
		void Walk(ITrLabel& node);
		void Walk(ITrBreak& node);
		void Walk(ITrContinue& node);
		void Walk(ITrFallthrough& node);
		void Walk(ITrGoto& node);
		void Walk(ITrReturn& node);
		void Walk(ITrThrow& node);
		void Walk(ITrDefer& node);
		void Walk(ITrUnsafe& node);
		void Walk(ITrErrHandler& node);
		void Walk(ITrCompCond& node);
		void Walk(ITrLocalVar& node);

		void Walk(ITrAssign& node);
		void Walk(ITrTernary& node);
		void Walk(ITrBinary& node);
		void Walk(ITrUnary& node);
		void Walk(ITrQualName& node);
		void Walk(ITrIndexSlice& node);
		void Walk(ITrAmbiguousCall& node);
		void Walk(ITrAdtTupleEnumInit& node);
		void Walk(ITrFuncCall& node);
		void Walk(ITrMemberAccess& node);
		void Walk(ITrTupleAccess& node);
		void Walk(ITrLiteral& node);
		void Walk(ITrAmbiguousAggrInit& node);
		void Walk(ITrAggrInit& node);
		void Walk(ITrAdtAggrEnumInit& node);
		void Walk(ITrTupleInit& node);
		void Walk(ITrArrayInit& node);
		void Walk(ITrCast& node);
		void Walk(ITrBlockExpr& node);
		void Walk(ITrUnsafeExpr& node);
		void Walk(ITrComma& node);
		void Walk(ITrClosure& node);
		void Walk(ITrMove& node);
		void Walk(ITrIs& node);
		void Walk(ITrTry& node);
		void Walk(ITrSpecKw& node);
		void Walk(ITrCompRun& node);

		void Walk(ITrType& node);

		void Walk(ITrPlaceholderPattern& node);
		void Walk(ITrAmbiguousIdenPattern& node);
		void Walk(ITrValueBindPattern& node);
		void Walk(ITrLiteralPattern& node);
		void Walk(ITrRangePattern& node);
		void Walk(ITrTuplePattern& node);
		void Walk(ITrValueEnumPattern& node);
		void Walk(ITrAdtTupleEnumPattern& node);
		void Walk(ITrAmbiguousAggrPattern& node);
		void Walk(ITrAggrPattern& node);
		void Walk(ITrAdtAggrEnumPattern& node);
		void Walk(ITrSlicePattern& node);
		void Walk(ITrEitherPattern& node);
		void Walk(ITrTypePattern& node);

		void Walk(ITrAttribs& node);
		void Walk(ITrAtAttrib& node);

		void Walk(ITrGenDecl& node);
		void Walk(ITrGenTypeParam& node);
		void Walk(ITrGenValParam& node);
		void Walk(ITrGenBound& node);

		ITrModule* m_pMod;
		bool m_VisitDefs;
	};

}
