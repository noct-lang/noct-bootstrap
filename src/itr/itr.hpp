#pragma once
#include "common/defs.hpp"
#include "common/type.hpp"
#include "common/qualname.hpp"
#include "module/attributes.hpp"
#include "module/operator.hpp"
#include "tokens/span.hpp"
#include "tokens/token.hpp"

namespace Noctis
{
	FWDECL_CLASS_SPTR(QualName);
	FWDECL_CLASS_SPTR(TypeDisambiguation);

	FWDECL_STRUCT_SPTR(ITrArg);
	FWDECL_STRUCT_SPTR(ITrParam);
	FWDECL_STRUCT_SPTR(ITrQualName);
	FWDECL_STRUCT_SPTR(ITrIden);
	FWDECL_STRUCT_SPTR(ITrTypeDisambiguation);

	FWDECL_STRUCT_WPTR(ITrDef);
	
	FWDECL_STRUCT_SPTR(ITrStmt);
	FWDECL_STRUCT_SPTR(ITrLabel);
	FWDECL_STRUCT_SPTR(ITrExpr);
	FWDECL_STRUCT_SPTR(ITrLocalVar);
	FWDECL_STRUCT_SPTR(ITrType);
	FWDECL_STRUCT_SPTR(ITrAttribs);
	FWDECL_STRUCT_SPTR(ITrAtAttrib);
	FWDECL_STRUCT_SPTR(ITrPattern);
	FWDECL_STRUCT_SPTR(ITrGenDecl);
	
	FWDECL_STRUCT_SPTR(Symbol);

	FWDECL_STRUCT_SPTR(FuncContext);

	enum class ITrDefKind
	{
		//UnitTest,
		//Benchmark,
		
		Struct,
		Union,
		ValEnum,
		ValEnumMember,
		AdtEnum,
		AdtEnumMember,
		MarkerInterface,
		WeakInterface,
		StrongInterface,
		Typealias,
		Typedef,
		Var,
		Func,
		Impl,
		ErrHandler,
		Count,
	};

	enum class ITrStmtKind
	{
		Block,
		If,
		Loop,
		ForRange,
		Switch,
		Label,
		Break,
		Continue,
		Fallthrough,
		Goto,
		Return,
		Throw,
		Expr,
		Defer,
		Unsafe,
		CompIf,
		CompCond,
		LocalDecl
	};

	enum class ITrExprKind
	{
		Assign,
		Ternary,
		Binary,
		Unary,
		QualName,
		IndexSlice,
		AmbiguousCall,
		FuncOrMethodCall,
		AdtTupleEnumInit,
		MemberAccess,
		TupleAccess,
		Literal,
		AmbiguousAggrInit,
		StructInit,
		UnionInit,
		AdtAggrEnumInit,
		TupleInit,
		ArrayInit,
		CastOrTransmute,
		Move,
		Block,
		Unsafe,
		Comma,
		Closure,
		Is,
		Try,
		SpecKw,
		CompRun,
	};

	enum class ITrPatternKind
	{
		Placeholder,
		Wildcard,
		ValueBind,
		Literal,
		Range,
		Tuple,
		ValueEnum,
		AdtTupleEnum,
		AmbiguousAggr,
		AdtAggrEnum,
		Aggr,
		Slice,
		Either,
		Type,
	};

	enum class ITrFuncKind : u8
	{
		Func,
		Method,
		EmptyMethod,
		Closure
	};

	struct ITrIden
	{
		ITrIden(const StdString& iden, const StdVector<IdenGeneric>& generics, StdPairVector<ITrTypeSPtr, ITrExprSPtr>&& assocArgs, u64 startIdx, u64 endIdx);
		
		StdString iden;
		StdVector<IdenGeneric> generics;
		StdPairVector<ITrTypeSPtr, ITrExprSPtr> assocArgs;
		u64 startIdx, endIdx;
	};

	struct ITrTypeDisambiguation
	{
		ITrTypeDisambiguation(TypeDisambiguationSPtr disambiguation, ITrTypeSPtr assocType, ITrQualNameSPtr assocQualName, u64 startIdx, u64 endIdx);
		
		ITrTypeSPtr assocType;
		ITrQualNameSPtr assocQualName;
		TypeDisambiguationSPtr disambiguation;
		u64 startIdx, endIdx;
	};

	struct ITrQualName
	{
		ITrQualName(QualNameSPtr qualName, ITrTypeDisambiguationSPtr assocDisambiguation, StdVector<ITrIdenSPtr>&& assocIdens, bool hasColonColon);

		QualNameSPtr qualName;
		ITrTypeDisambiguationSPtr assocDisambiguation;
		StdVector<ITrIdenSPtr> assocIdens;
		u64 startIdx, endIdx;
		bool hasColonColon;
	};

	struct ITrParam
	{
		ITrParam(ITrAttribsSPtr attribs, const StdString& iden, ITrTypeSPtr type, u64 startIdx, u64 endIdx);

		ITrAttribsSPtr attribs;
		StdString iden;
		ITrTypeSPtr type;
		u64 startIdx, endIdx;
	};

	struct ITrArg
	{
		ITrArg(const StdString& iden, ITrExprSPtr expr, u64 startIdx);
		
		StdString iden;
		ITrExprSPtr expr;
		u64 startIdx, endIdx;
	};


	FWDECL_STRUCT_SPTR(ITrDef);
	
	struct ITrDef
	{
		ITrDef(ITrDefKind kind, ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, bool isModDef, u64 startIdx, u64 endIdx);
		virtual ~ITrDef();

		ITrDefKind kind;
		ITrAttribsSPtr attribs;
		QualNameSPtr qualName;
		ITrGenDeclSPtr genDecl;
		u64 bodyIdx;

		SymbolWPtr sym;

		// Is the definition in the module, instead of being a sub definition in another definition
		bool isModDef : 1;

		bool isDummyDef : 1;

		// Name of the file that contains the definition (needed to retrieve correct spans)
		StdString fileName;
		ITrDefWPtr ptr;

		ITrDefSPtr impl;

		StdUnorderedMap<StdString, TypeHandle> genMapping;

		u64 startIdx, endIdx;
	};

	struct ITrStruct : ITrDef
	{
		ITrStruct(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, bool isModDef, u64 startIdx, u64 endIdx);
	};

	struct ITrUnion : ITrDef
	{
		ITrUnion(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, bool isModDef, u64 startIdx, u64 endIdx);
	};

	struct ITrValEnum : ITrDef
	{
		ITrValEnum(ITrAttribsSPtr attribs, QualNameSPtr qualName, bool isModDef, u64 startIdx, u64 endIdx);
	};

	struct ITrValEnumMember : ITrDef
	{
		ITrValEnumMember(QualNameSPtr parent, const StdString& iden, ITrExprSPtr val, u64 startIdx, u64 endIdx);

		ITrExprSPtr val;
	};
	
	struct ITrAdtEnum : ITrDef
	{
		ITrAdtEnum(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, bool isModDef, u64 startIdx, u64 endIdx);
	};

	struct ITrAdtEnumMember : ITrDef
	{
		ITrAdtEnumMember(QualNameSPtr parent, const StdString& iden, ITrTypeSPtr type, u64 startIdx, u64 endIdx);

		ITrTypeSPtr type;
	};

	struct ITrMarkerInterface : ITrDef
	{
		ITrMarkerInterface(ITrAttribsSPtr attribs, QualNameSPtr qualName, u64 startIdx, u64 endIdx);
	};

	struct ITrStrongInterface : ITrDef
	{
		ITrStrongInterface(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, StdPairVector<QualNameSPtr, SpanId>&& implInterfaces, u64 startIdx, u64 endIdx);

		StdPairVector<QualNameSPtr, SpanId> implInterfaces;
	};

	struct ITrWeakInterface : ITrDef
	{
		ITrWeakInterface(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, u64 startIdx, u64 endIdx);
	};

	struct ITrTypealias : ITrDef
	{
		ITrTypealias(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, ITrTypeSPtr type, bool isModDef, u64 startIdx, u64 endIdx);

		ITrTypeSPtr type;
	};

	struct ITrTypedef : ITrDef
	{
		ITrTypedef(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, ITrTypeSPtr type, bool isModDef, u64 startIdx, u64 endIdx);

		ITrTypeSPtr type;
	};

	struct ITrVar : ITrDef
	{
		ITrVar(ITrAttribsSPtr attribs, QualNameSPtr qualName, ITrTypeSPtr type, ITrExprSPtr init, bool isModDef, u64 startIdx, u64 endIdx);

		ITrTypeSPtr type;
		ITrExprSPtr init;
	};

	struct ITrFunc : ITrDef
	{
		ITrFunc(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, StdVector<ITrParamSPtr>&& params, ITrTypeSPtr errorType, ITrTypeSPtr retType, ITrFuncKind funcKind, bool isUnsafe, bool isModDef, u64 startIdx, u64 endIdx);

		ITrFuncKind funcKind;
		StdVector<ITrParamSPtr> params;
		ITrTypeSPtr errorType;
		ITrTypeSPtr retType;
		TypeHandle selfType;
		FuncContextSPtr ctx;
		bool isUnsafe;
	};

	struct ITrImpl : ITrDef
	{
		ITrImpl(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr scope, ITrTypeSPtr type, StdPair<QualNameSPtr, SpanId> interface, u64 startIdx, u64 endIdx);

		ITrTypeSPtr type;
		StdPair<QualNameSPtr, SpanId> interface;
	};

	struct ITrErrHandler : ITrDef
	{
		ITrErrHandler(QualNameSPtr qualName, StdString errIden, ITrTypeSPtr errType, u64 startIdx, u64 endIdx);

		StdString errIden;
		ITrTypeSPtr errType;

		TypeHandle retType;
		FuncContextSPtr ctx;
	};

	struct ITrStmt
	{
		ITrStmt(ITrStmtKind kind, u64 startIdx, u64 endIdx);
		virtual  ~ITrStmt();

		ITrStmtKind stmtKind;
		u64 startIdx, endIdx;
	};

	struct ITrBlock : ITrStmt
	{
		ITrBlock(const StdString& scopeName, StdVector<ITrStmtSPtr>&& stmts, u64 startIdx, u64 endIdx);

		StdVector<ITrStmtSPtr> stmts;
		StdString scopeName;
	};
	using ITrBlockSPtr = StdSharedPtr<ITrBlock>;

	struct ITrIf : ITrStmt
	{
		ITrIf(bool isComptime, ITrLocalVarSPtr decl, ITrExprSPtr cond, ITrBlockSPtr tBlock, ITrBlockSPtr fBlock, u64 startIdx);

		bool isComptime;
		ITrLocalVarSPtr decl;
		ITrExprSPtr cond;
		ITrBlockSPtr tBlock;
		ITrBlockSPtr fBlock;
	};

	struct ITrLoop : ITrStmt
	{
		ITrLoop(const StdString& label, ITrBlockSPtr block, u64 startIdx, u64 endIdx);

		StdString label;
		ITrBlockSPtr block;
	};

	struct ITrForRange : ITrStmt
	{
		ITrForRange(const StdString& scopeName, const StdString& label, const StdVector<StdString>& idens, ITrExprSPtr range, ITrBlockSPtr body, u64 startIdx);

		StdString label;
		StdVector<StdString> idens;
		ITrExprSPtr range;
		ITrBlockSPtr body;
		StdString scopeName;
	};

	enum class ITrSwitchGroupKind : u8
	{
		Base,
		Leaf,
		Range,
		LitMatch,
		EnumMatch,

		Tuple,
		TupleIndex,

		Aggr,
		AggrMember,

		Slice,
		SliceIndex,
	};

	struct ITrSwitchGroup
	{
		ITrSwitchGroup(ITrSwitchGroupKind kind, usize depth, usize caseId)
			: kind(kind)
			, depth(depth)
			, valOrFrom(0)
			, to(0)
			, idx(0)
			, isDefCase(false)
			, indexFromBack(false)
		{
			if (caseId != usize(-1))
				cases.push_back(caseId);
		}

		ITrSwitchGroupKind kind;
		usize depth;

		u64 valOrFrom;
		u64 to;
		StdString member;

		usize idx;

		StdString imm;

		StdVector<usize> cases;
		StdVector<ITrSwitchGroup> subGroups;

		StdString bindName;
		
		bool isDefCase : 1;
		bool indexFromBack : 1;
	};

	struct ITrSwitchCase
	{
		ITrSwitchCase();
		ITrSwitchCase(ITrPatternSPtr patterns, ITrExprSPtr expr, ITrBlockSPtr block);
		
		ITrPatternSPtr pattern;
		ITrExprSPtr expr;
		ITrBlockSPtr block;
	};
	
	struct ITrSwitch : ITrStmt
	{
		ITrSwitch(const StdString& scopeName, const StdString& label, ITrExprSPtr expr, StdVector<ITrSwitchCase>&& cases, u64 startIdx, u64 endIdx);

		StdString label;
		ITrExprSPtr expr;
		StdVector<ITrSwitchCase> cases;
		StdString scopeName;

		ITrSwitchGroup baseGroup;
	};

	struct ITrLabel : ITrStmt
	{
		ITrLabel(const StdString& label, u64 startIdx, u64 endIdx);

		StdString label;
	};

	struct ITrBreak : ITrStmt
	{
		ITrBreak(const StdString& label, u64 startIdx, u64 endIdx);

		StdString label;
	};

	struct ITrContinue : ITrStmt
	{
		ITrContinue(const StdString& label, u64 startIdx, u64 endIdx);

		StdString label;
	};

	struct ITrFallthrough : ITrStmt
	{
		ITrFallthrough(u64 startIdx, u64 endIdx);
	};

	struct ITrGoto : ITrStmt
	{
		ITrGoto(const StdString& label, u64 startIdx, u64 endIdx);

		StdString label;
	};

	struct ITrReturn : ITrStmt
	{
		ITrReturn(ITrExprSPtr expr, u64 startIdx, u64 endIdx);
		
		ITrExprSPtr expr;
	};

	struct ITrThrow : ITrStmt
	{
		ITrThrow(ITrExprSPtr expr, u64 startIdx, u64 endIdx);

		ITrExprSPtr expr;
	};

	struct ITrDefer : ITrStmt
	{
		ITrDefer(bool isErr, ITrExprSPtr block, u64 startIdx, u64 endIdx);
		
		bool isErr;
		ITrExprSPtr block;
	};

	struct ITrUnsafe : ITrStmt
	{
		ITrUnsafe(ITrBlockSPtr block, u64 startIdx);

		ITrBlockSPtr block;
	};

	struct ITrCompCond : ITrStmt
	{
		ITrCompCond(bool isDebug, const StdString& iden, OperatorKind op, u64 cmpVal, ITrBlockSPtr tBlock, ITrBlockSPtr fBlock, u64 startIdx);

		bool isDebug;
		OperatorKind op;
		StdString iden;
		u64 cmpVal;
		ITrBlockSPtr tBlock;
		ITrBlockSPtr fBlock;
	};

	struct ITrLocalVar : ITrStmt
	{
		ITrLocalVar(ITrAttribsSPtr attribs, const StdVector<StdString>& idens, ITrTypeSPtr type, ITrExprSPtr init, u64 startIdx, u64 endIdx);
		
		ITrAttribsSPtr attribs;
		StdVector<StdString> idens;
		ITrTypeSPtr type;
		ITrExprSPtr init;
	};

	struct ITrExpr : ITrStmt
	{
		ITrExpr(ITrExprKind kind, u64 startIdx, u64 endIdx);
		virtual ~ITrExpr();

		ITrExprKind exprKind;
		TypeHandle handle;
		SymbolSPtr sym;
	};
	
	struct ITrAssign : ITrExpr
	{
		ITrAssign(OperatorKind op, ITrExprSPtr lExpr, ITrExprSPtr rExpr);

		OperatorKind op;
		ITrExprSPtr lExpr;
		ITrExprSPtr rExpr;

		Operator operator_;
	};

	struct ITrTernary : ITrExpr
	{
		ITrTernary(ITrExprSPtr cond, ITrExprSPtr tExpr, ITrExprSPtr fExpr);
		
		ITrExprSPtr cond;
		ITrExprSPtr tExpr;
		ITrExprSPtr fExpr;
	};

	struct ITrBinary : ITrExpr
	{
		ITrBinary(OperatorKind op, ITrExprSPtr lExpr, ITrExprSPtr rExpr);

		OperatorKind op;
		ITrExprSPtr lExpr;
		ITrExprSPtr rExpr;
		
		Operator operator_;
	};

	struct ITrUnary : ITrExpr
	{
		ITrUnary(OperatorKind op, ITrExprSPtr expr, u64 startIdx, u64 endIdx);

		OperatorKind op;
		ITrExprSPtr expr;

		Operator operator_;
	};

	struct ITrQualNameExpr : ITrExpr
	{
		ITrQualNameExpr(ITrQualNameSPtr qualName);

		QualNameSPtr qualName;
		ITrQualNameSPtr itrQualName;
	};

	struct ITrIndexSlice : ITrExpr
	{
		ITrIndexSlice(ITrExprSPtr expr, ITrExprSPtr index, u64 endIdx);
		ITrIndexSlice(ITrExprSPtr expr, ITrExprSPtr from, ITrExprSPtr to, u64 endIdx);
		
		bool explicitSlice;
		ITrExprSPtr expr;
		ITrExprSPtr index;
		ITrExprSPtr to;

		Operator operator_;
	};

	// Can be ITrFuncCall or ITrAdtTupleEnumInit
	struct ITrAmbiguousCall : ITrExpr
	{
		ITrAmbiguousCall(ITrExprSPtr expr, StdVector<ITrArgSPtr>&& args, u64 endIdx);

		ITrExprSPtr expr;
		StdVector<ITrArgSPtr> args;
	};

	struct ITrAdtTupleEnumInit : ITrExpr
	{
		ITrAdtTupleEnumInit(ITrExprSPtr expr, StdVector<ITrArgSPtr>&& args, u64 endIdx);

		ITrExprSPtr expr;
		StdVector<ITrArgSPtr> args;
	};
	
	struct ITrFuncCall : ITrExpr
	{
		ITrFuncCall(ITrExprSPtr func, StdVector<ITrArgSPtr>&& args, u64 endIdx);
		ITrFuncCall(ITrExprSPtr caller, bool nullCoalesce, const StdString& iden, const StdVector<IdenGeneric>& generics, StdVector<ITrArgSPtr>&& args, u64 endIdx);

		bool isMethod : 1;
		bool nullCoalesce : 1;
		ITrExprSPtr callerOrFunc;
		StdString iden;
		StdVector<IdenGeneric> generics;
		StdVector<ITrArgSPtr> args;
	};

	struct ITrMemberAccess : ITrExpr
	{
		ITrMemberAccess(bool nullCoalesce, ITrExprSPtr expr, const StdString& iden, u64 endIdx);
		
		bool nullCoalesce;
		ITrExprSPtr expr;
		StdString iden;
	};

	struct ITrTupleAccess : ITrExpr
	{
		ITrTupleAccess(ITrExprSPtr expr, bool nullCoalesce, u16 index, u64 endIdx);
		
		bool nullCoalesce;
		u16 index;
		ITrExprSPtr expr;
	};

	struct ITrLiteral : ITrExpr
	{
		ITrLiteral(Token lit);

		Token lit;
	};

	// Can be ITrStructInit, ITrUnionInit or ITrAdtEnumAggrInit
	struct ITrAmbiguousAggrInit : ITrExpr
	{
		ITrAmbiguousAggrInit(ITrTypeSPtr type, StdVector<ITrArgSPtr>&& args, bool hasDefInit, ITrExprSPtr defExpr, u64 endIdx);

		ITrTypeSPtr type;
		StdVector<ITrArgSPtr> args;
		ITrExprSPtr defExpr;
		bool hasDefInit;
	};

	struct ITrStructInit : ITrExpr
	{
		ITrStructInit(ITrTypeSPtr type, StdVector<ITrArgSPtr>&& args, bool hasDefInit, ITrExprSPtr defExpr, u64 endIdx);

		ITrTypeSPtr type;
		StdVector<ITrArgSPtr> args;
		ITrExprSPtr defExpr;
		bool hasDefInit;

		StdVector<u32> argOrder;
	};

	struct ITrUnionInit : ITrExpr
	{
		ITrUnionInit(ITrTypeSPtr type, ITrArgSPtr arg, u64 endIdx);

		ITrTypeSPtr type;
		ITrArgSPtr arg;
	};

	struct ITrAdtAggrEnumInit : ITrExpr
	{
		ITrAdtAggrEnumInit(ITrTypeSPtr type, StdVector<ITrArgSPtr>&& args, u64 endIdx);

		ITrTypeSPtr type;
		StdVector<ITrArgSPtr> args;
		ITrExprSPtr defExpr;
		bool hasDefInit;
	};

	struct ITrTupleInit : ITrExpr
	{
		ITrTupleInit(StdVector<ITrExprSPtr>&& exprs, u64 startIdx, u64 endIdx);

		StdVector<ITrExprSPtr> exprs;
	};

	struct ITrArrayInit : ITrExpr
	{
		ITrArrayInit(StdVector<ITrExprSPtr>&& exprs, u64 startIdx, u64 endIdx);

		StdVector<ITrExprSPtr> exprs;
	};

	enum class ITrCastKind : u8
	{
		Cast,
		SafeCast,
		NullPanicCast,
		Transmute
	};

	struct ITrCast : ITrExpr
	{
		ITrCast(ITrCastKind castKind, ITrExprSPtr expr, ITrTypeSPtr type);
		
		ITrCastKind castKind;
		ITrExprSPtr expr;
		ITrTypeSPtr type;

		Operator operator_;
		bool castToTryCast;
	};

	struct ITrBlockExpr : ITrExpr
	{
		ITrBlockExpr(const StdString& scopeName, StdVector<ITrStmtSPtr> stmts, u64 startIdx, u64 endIdx);

		StdVector<ITrStmtSPtr> stmts;
		StdString scopeName;
	};

	struct ITrUnsafeExpr : ITrExpr
	{
		ITrUnsafeExpr(ITrExprSPtr expr, u64 startIdx);

		ITrExprSPtr expr;
	};

	struct ITrMove : ITrExpr
	{
		ITrMove(ITrExprSPtr expr, u64 startIdx);

		ITrExprSPtr expr;
	};

	struct ITrComma : ITrExpr
	{
		ITrComma(StdVector<ITrExprSPtr>&& exprs, u64 startIdx, u64 endIdx);

		StdVector<ITrExprSPtr> exprs;
	};

	struct ITrClosure : ITrExpr
	{
		ITrClosure(ITrDefSPtr def, u64 startIdx, u64 endIdx);

		ITrDefSPtr def;
	};

	struct ITrIs : ITrExpr
	{
		ITrIs(ITrExprSPtr expr, ITrTypeSPtr type);

		ITrExprSPtr expr;
		ITrTypeSPtr type;
	};
	
	enum class ITrTryKind
	{
		Propagating,
		Nullable,
		Panic
	};
	
	struct ITrTry : ITrExpr
	{
		ITrTry(ITrTryKind kind, ITrExprSPtr expr, u64 startIdx);

		ITrTryKind kind;
		ITrExprSPtr expr;

		QualNameSPtr errHandlerName;
	};

	struct ITrSpecKw : ITrExpr
	{
		ITrSpecKw(TokenType kw, u64 tokIdx);

		TokenType kw;
	};

	struct ITrCompRun : ITrExpr
	{
		ITrCompRun(ITrExprSPtr expr, u64 startIdx);

		ITrExprSPtr expr;
	};

	struct  ITrType
	{
		ITrType(ITrAttribsSPtr attribs, TypeHandle handle, StdVector<ITrTypeSPtr>&& subTypes, ITrExprSPtr expr, u64 startIdx, u64 endIdx);

		ITrAttribsSPtr attribs;
		StdVector<ITrTypeSPtr> subTypes;
		ITrExprSPtr expr;
		TypeHandle handle;
		u64 startIdx, endIdx;
	};

	struct ITrPattern
	{
		ITrPattern(ITrPatternKind kind, u64 startIdx, u64 endIdx);
		
		ITrPatternKind patternKind;
		TypeHandle patternType;
		u64 startIdx, endIdx;
		StdString imm;
	};

	struct ITrPlaceholderPattern : ITrPattern
	{
		ITrPlaceholderPattern(bool isWildcard, u64 tokIdx);
	};

	struct ITrValueBindPattern : ITrPattern
	{
		ITrValueBindPattern(const StdString& iden, ITrPatternSPtr subPattern, u64 startIdx, u64 endIdx);
		
		StdString iden;
		ITrPatternSPtr subPattern;
	};

	struct ITrLiteralPattern : ITrPattern
	{
		ITrLiteralPattern(Token lit);
		
		Token lit;
	};

	struct ITrRangePattern : ITrPattern
	{
		ITrRangePattern(bool isInclusive, Token from, Token to);

		bool isInclusive;
		Token from;
		Token to;
	};

	struct ITrTuplePattern : ITrPattern
	{
		ITrTuplePattern(StdVector<ITrPatternSPtr>&& subPatterns, u64 startIdx, u64 endIdx);

		StdVector<ITrPatternSPtr> subPatterns;
	};

	struct ITrValueEnumPattern : ITrPattern
	{
		ITrValueEnumPattern(QualNameSPtr qualName, u64 startIdx, u64 endIdx);

		QualNameSPtr qualName;
	};

	struct ITrAdtTupleEnumPattern : ITrPattern
	{
		ITrAdtTupleEnumPattern(QualNameSPtr qualName, StdVector<ITrPatternSPtr>&& subPatterns, u64 startIdx, u64 endIdx);

		QualNameSPtr qualName;
		StdVector<ITrPatternSPtr> subPatterns;
	};

	struct ITrAmbiguousAggrPattern : ITrPattern
	{
		ITrAmbiguousAggrPattern(QualNameSPtr qualName, StdPairVector<StdString, ITrPatternSPtr>&& args, u64 startIdx, u64 endIdx);

		QualNameSPtr qualName;
		StdPairVector<StdString, ITrPatternSPtr> args;
	};

	struct ITrAggrPattern : ITrPattern
	{
		ITrAggrPattern(QualNameSPtr qualName, StdPairVector<StdString, ITrPatternSPtr>&& args, u64 startIdx, u64 endIdx);

		QualNameSPtr qualName;
		StdPairVector<StdString, ITrPatternSPtr> args;
	};

	struct ITrAdtAggrEnumPattern : ITrPattern
	{
		ITrAdtAggrEnumPattern(QualNameSPtr qualName, StdPairVector<StdString, ITrPatternSPtr>&& args, u64 startIdx, u64 endIdx);

		QualNameSPtr qualName;
		StdPairVector<StdString, ITrPatternSPtr> args;
	};

	struct ITrSlicePattern : ITrPattern
	{
		ITrSlicePattern(StdVector<ITrPatternSPtr>&& subPatterns, u64 startIdx, u64 endIdx);

		StdVector<ITrPatternSPtr> subPatterns;
	};

	struct ITrEitherPattern : ITrPattern
	{
		ITrEitherPattern(StdVector<ITrPatternSPtr>&& subPatterns);

		StdVector<ITrPatternSPtr> subPatterns;
	};

	struct ITrTypePattern : ITrPattern
	{
		ITrTypePattern(ITrTypeSPtr type, u64 startIdx);
		
		ITrTypeSPtr type;
	};

	struct ITrAttribs
	{
		ITrAttribs(Visibility vis, Attribute attribs, StdVector<ITrAtAttribSPtr>&& atAttribs, u64 startIdx, u64 endIdx);

		Visibility vis;
		Attribute attribs;
		StdVector<ITrAtAttribSPtr> atAttribs;
		u64 startIdx, endIdx;
	};

	struct ITrAtAttrib
	{
		ITrAtAttrib(bool isCompAttrib, const StdString& iden, StdVector<ITrArgSPtr>&& args, u64 startIdx, u64 endIdx);

		bool isCompAttrib;
		StdString iden;
		StdVector<ITrArgSPtr> args;
		u64 startIdx, endIdx;
	};

	FWDECL_STRUCT_SPTR(ITrGenParam);
	FWDECL_STRUCT_SPTR(ITrGenTypeBound);
	
	struct ITrGenDecl
	{
		StdVector<ITrGenParamSPtr> params;
		StdVector<ITrGenTypeBoundSPtr> bounds;
	};

	struct ITrGenParam
	{
		ITrGenParam(bool isType, u64 startIdx, u64 endIdx);
		
		bool isType;
		u64 startIdx, endIdx;
	};

	struct ITrGenTypeParam : ITrGenParam
	{
		ITrGenTypeParam(const StdString& name, ITrTypeSPtr defType, u64 startIdx, u64 endIdx);
		
		StdString iden;
		ITrTypeSPtr defType;
	};
	
	struct ITrGenValParam : ITrGenParam
	{
		ITrGenValParam(const StdString& iden, ITrTypeSPtr type, ITrExprSPtr defExpr, u64 startIdx, u64 endIdx);

		StdString iden;
		ITrTypeSPtr type;
		ITrExprSPtr defExpr;
	};

	FWDECL_STRUCT_SPTR(ITrGenBoundType);
	
	struct ITrGenAssocBound
	{
		ITrGenAssocBound(const StdString& iden, ITrGenBoundTypeSPtr type, u64 startIdx, u64 endIdx);
		
		StdString iden;
		ITrGenBoundTypeSPtr type;
		u64 startIdx, endIdx;
	};
	
	struct ITrGenBoundType
	{
		ITrGenBoundType(ITrTypeSPtr type, StdVector<ITrGenAssocBound>&& assocBounds, u64 startIdx, u64 endIdx);
		
		ITrTypeSPtr type;
		StdVector<ITrGenAssocBound> assocBounds;
		u64 startIdx, endIdx;
	};
	
	struct ITrGenTypeBound
	{
		ITrGenTypeBound(ITrTypeSPtr type, ITrGenBoundTypeSPtr bound, u64 startIdx, u64 endIdx);
		
		ITrTypeSPtr type;
		ITrGenBoundTypeSPtr bound;
		u64 startIdx, endIdx;
	};

	struct ITrBody
	{
		ITrBody(StdVector<ITrDefSPtr>&& defs, StdVector<ITrStmtSPtr>&& stmts);

		StdVector<ITrDefSPtr> defs;
		StdVector<ITrStmtSPtr> stmts;
	};
	using ITrBodySPtr = StdSharedPtr<ITrBody>;

	struct ITrModule
	{
		void AddDefinition(ITrDefSPtr def);
		void AddDefinition(ITrDefSPtr def, ITrBodySPtr body);
		u64 AddBody(ITrBodySPtr body);

		ITrBodySPtr GetBody(ITrDef& def);
		ITrBodySPtr GetBody(u64 idx);

		StdArray<StdVector<ITrDefSPtr>, u8(ITrDefKind::Count)> defMapping;
		StdVector<ITrBodySPtr> bodies;

		StdVector<ITrDefSPtr> funcsToProcess;
	};

	
}
