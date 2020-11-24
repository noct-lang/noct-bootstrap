#pragma once
#include "common/defs.hpp"
#include "common/type.hpp"
#include "module/attributes.hpp"
#include "module/operator.hpp"
#include "tokens/span.hpp"
#include "tokens/token.hpp"

namespace Noctis
{
	FWDECL_CLASS_SPTR(QualName);
	FWDECL_CLASS_SPTR(Iden);
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
	
	FWDECL_STRUCT_SPTR(AstParamVar);
	FWDECL_STRUCT_SPTR(AstParam);
	FWDECL_STRUCT_SPTR(AstArg);
	FWDECL_STRUCT_SPTR(AstStmt);
	FWDECL_STRUCT_SPTR(AstDecl);
	FWDECL_STRUCT_SPTR(AstExpr);
	FWDECL_STRUCT_SPTR(AstType);
	FWDECL_STRUCT_SPTR(AstPattern);
	FWDECL_STRUCT_SPTR(AstAttribs);
	FWDECL_STRUCT_SPTR(AstGenericDecl);
	
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
		Count,
	};

	enum class ITrStmtKind
	{
		Block,
		If,
		Loop,
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
		ErrorHandler,
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
		ITrIden(IdenSPtr iden, StdPairVector<ITrTypeSPtr, ITrExprSPtr>&& assocArgs);
		
		StdPairVector<ITrTypeSPtr, ITrExprSPtr> assocArgs;
		IdenSPtr iden;
	};

	struct ITrTypeDisambiguation
	{
		ITrTypeDisambiguation(TypeDisambiguationSPtr disambiguation, ITrTypeSPtr assocType, ITrQualNameSPtr assocQualName);
		
		ITrTypeSPtr assocType;
		ITrQualNameSPtr assocQualName;
		TypeDisambiguationSPtr disambiguation;
	};

	struct ITrQualName
	{
		ITrQualName(QualNameSPtr qualName, ITrTypeDisambiguationSPtr assocDisambiguation, StdVector<ITrIdenSPtr>&& assocIdens);

		QualNameSPtr qualName;
		ITrTypeDisambiguationSPtr assocDisambiguation;
		StdVector<ITrIdenSPtr> assocIdens;
	};

	struct ITrParam
	{
		ITrParam(ITrAttribsSPtr attribs, IdenSPtr label, IdenSPtr iden, ITrTypeSPtr type);

		ITrAttribsSPtr attribs;
		IdenSPtr label;
		IdenSPtr iden;
		ITrTypeSPtr type;
		AstParamSPtr astNode;
		AstParamVarSPtr astVarNode;
	};

	struct ITrArg
	{
		ITrArg(IdenSPtr iden, ITrExprSPtr expr);
		
		IdenSPtr iden;
		ITrExprSPtr expr;
		AstArgSPtr astNode;
	};


	FWDECL_STRUCT_SPTR(ITrDef);
	
	struct ITrDef
	{
		ITrDef(ITrDefKind kind, ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, bool isModDef);
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
		AstDeclSPtr astNode;
		ITrDefWPtr ptr;

		ITrDefSPtr impl;

		StdUnorderedMap<IdenSPtr, TypeHandle> genMapping;
	};

	struct ITrStruct : ITrDef
	{
		ITrStruct(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, bool isModDef);
	};

	struct ITrUnion : ITrDef
	{
		ITrUnion(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, bool isModDef);
	};

	struct ITrValEnum : ITrDef
	{
		ITrValEnum(ITrAttribsSPtr attribs, QualNameSPtr qualName, bool isModDef);
	};

	struct ITrValEnumMember : ITrDef
	{
		ITrValEnumMember(QualNameSPtr parent, IdenSPtr iden, ITrExprSPtr val);

		ITrExprSPtr val;
	};
	
	struct ITrAdtEnum : ITrDef
	{
		ITrAdtEnum(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, bool isModDef);
	};

	struct ITrAdtEnumMember : ITrDef
	{
		ITrAdtEnumMember(QualNameSPtr parent, IdenSPtr iden, ITrTypeSPtr type);

		ITrTypeSPtr type;
	};

	struct ITrMarkerInterface : ITrDef
	{
		ITrMarkerInterface(ITrAttribsSPtr attribs, QualNameSPtr qualName);
	};

	struct ITrStrongInterface : ITrDef
	{
		ITrStrongInterface(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, StdPairVector<QualNameSPtr, SpanId>&& implInterfaces);

		StdPairVector<QualNameSPtr, SpanId> implInterfaces;
	};

	struct ITrWeakInterface : ITrDef
	{
		ITrWeakInterface(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName);
	};

	struct ITrTypealias : ITrDef
	{
		ITrTypealias(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, ITrTypeSPtr type, bool isModDef);

		ITrTypeSPtr type;
	};

	struct ITrTypedef : ITrDef
	{
		ITrTypedef(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, ITrTypeSPtr type, bool isModDef);

		ITrTypeSPtr type;
	};

	struct ITrVar : ITrDef
	{
		ITrVar(ITrAttribsSPtr attribs, QualNameSPtr qualName, ITrTypeSPtr type, bool isModDef);

		ITrTypeSPtr type;
	};

	struct ITrFunc : ITrDef
	{
		ITrFunc(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, StdVector<ITrParamSPtr>&& params, ITrTypeSPtr retType, ITrFuncKind funcKind, bool isModDef);

		ITrFuncKind funcKind;
		StdVector<ITrParamSPtr> params;
		ITrTypeSPtr retType;
		TypeHandle selfType;
		FuncContextSPtr ctx;
	};

	struct ITrImpl : ITrDef
	{
		ITrImpl(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr scope, ITrTypeSPtr type, StdPair<QualNameSPtr, SpanId> interface);

		ITrTypeSPtr type;
		StdPair<QualNameSPtr, SpanId> interface;
	};

	struct ITrStmt
	{
		ITrStmt(ITrStmtKind kind);
		virtual  ~ITrStmt();

		ITrStmtKind stmtKind;
		StdVariant<AstStmtSPtr, AstExprSPtr> astNode;
	};

	struct ITrBlock : ITrStmt
	{
		ITrBlock(const StdString& scopeName, StdVector<ITrStmtSPtr>&& stmts);

		StdVector<ITrStmtSPtr> stmts;
		StdString scopeName;
	};
	using ITrBlockSPtr = StdSharedPtr<ITrBlock>;

	struct ITrIf : ITrStmt
	{
		ITrIf(bool isComptime, ITrLocalVarSPtr decl, ITrExprSPtr cond, ITrBlockSPtr tBlock, ITrBlockSPtr fBlock);

		bool isComptime;
		ITrLocalVarSPtr decl;
		ITrExprSPtr cond;
		ITrBlockSPtr tBlock;
		ITrBlockSPtr fBlock;
	};

	struct ITrLoop : ITrStmt
	{
		ITrLoop(const StdString& scopeName, IdenSPtr label, StdVector<ITrStmtSPtr>&& stmts);

		IdenSPtr label;
		StdVector<ITrStmtSPtr> stmts;
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
		ITrSwitchGroup(ITrSwitchGroupKind kind, usize depth)
			: kind(kind)
			, depth(depth)
			, valOrFrom(0)
			, to(0)
			, idx(0)
			, isDefCase(false)
			, indexFromBack(false)
		{}

		ITrSwitchGroupKind kind;
		usize depth;

		u64 valOrFrom;
		u64 to;
		StdString member;

		usize idx;

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
		ITrSwitch(const StdString& scopeName, IdenSPtr label, ITrExprSPtr expr, StdVector<ITrSwitchCase>&& cases);

		IdenSPtr label;
		ITrExprSPtr expr;
		StdVector<ITrSwitchCase> cases;
		StdString scopeName;

		ITrSwitchGroup baseGroup;
	};

	struct ITrLabel : ITrStmt
	{
		ITrLabel(IdenSPtr label);

		IdenSPtr label;
	};

	struct ITrBreak : ITrStmt
	{
		ITrBreak(IdenSPtr label = nullptr);

		IdenSPtr label;
	};

	struct ITrContinue : ITrStmt
	{
		ITrContinue(IdenSPtr label);

		IdenSPtr label;
	};

	struct ITrFallthrough : ITrStmt
	{
		ITrFallthrough();
	};

	struct ITrGoto : ITrStmt
	{
		ITrGoto(IdenSPtr label);

		IdenSPtr label;
	};

	struct ITrReturn : ITrStmt
	{
		ITrReturn(ITrExprSPtr expr);
		
		ITrExprSPtr expr;
	};

	struct ITrThrow : ITrStmt
	{
		ITrThrow(ITrExprSPtr expr);

		ITrExprSPtr expr;
	};

	struct ITrDefer : ITrStmt
	{
		ITrDefer(bool isErr, ITrExprSPtr block);
		
		bool isErr;
		ITrExprSPtr block;
	};

	struct ITrUnsafe : ITrStmt
	{
		ITrUnsafe(ITrBlockSPtr block);

		ITrBlockSPtr block;
	};

	struct ITrErrHandler : ITrStmt
	{
		ITrErrHandler(StdVector<ITrStmtSPtr>&& stmts);

		StdVector<ITrStmtSPtr> stmts;
	};

	struct ITrCompCond : ITrStmt
	{
		ITrCompCond(bool isDebug, IdenSPtr iden, OperatorKind op, u64 cmpVal, ITrBlockSPtr tBlock, ITrBlockSPtr fBlock);

		bool isDebug;
		OperatorKind op;
		IdenSPtr iden;
		u64 cmpVal;
		ITrBlockSPtr tBlock;
		ITrBlockSPtr fBlock;
	};

	struct ITrLocalVar : ITrStmt
	{
		ITrLocalVar(ITrAttribsSPtr attribs, StdVector<IdenSPtr>&& idens, ITrTypeSPtr type, ITrExprSPtr init);
		
		ITrAttribsSPtr attribs;
		StdVector<IdenSPtr> idens;
		ITrTypeSPtr type;
		ITrExprSPtr init;
	};

	struct ITrExpr : ITrStmt
	{
		ITrExpr(ITrExprKind kind);
		virtual ~ITrExpr();

		ITrExprKind exprKind;
		TypeInfo typeInfo;
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
		ITrUnary(OperatorKind op, ITrExprSPtr expr);

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
		ITrIndexSlice(ITrExprSPtr expr, ITrExprSPtr index);
		ITrIndexSlice(ITrExprSPtr expr, ITrExprSPtr from, ITrExprSPtr to);
		
		bool explicitSlice;
		ITrExprSPtr expr;
		ITrExprSPtr index;
		ITrExprSPtr to;

		Operator operator_;
	};

	// Can be ITrFuncCall or ITrAdtTupleEnumInit
	struct ITrAmbiguousCall : ITrExpr
	{
		ITrAmbiguousCall(ITrExprSPtr expr, StdVector<ITrArgSPtr>&& args);

		ITrExprSPtr expr;
		StdVector<ITrArgSPtr> args;
	};

	struct ITrAdtTupleEnumInit : ITrExpr
	{
		ITrAdtTupleEnumInit(ITrExprSPtr expr, StdVector<ITrArgSPtr>&& args);

		ITrExprSPtr expr;
		StdVector<ITrArgSPtr> args;
	};
	
	struct ITrFuncCall : ITrExpr
	{
		ITrFuncCall(ITrExprSPtr func, StdVector<ITrArgSPtr>&& args);
		ITrFuncCall(ITrExprSPtr caller, bool nullCoalesce, IdenSPtr iden, StdVector<ITrArgSPtr>&& args);

		bool isMethod : 1;
		bool nullCoalesce : 1;
		ITrExprSPtr callerOrFunc;
		IdenSPtr iden;
		StdVector<ITrArgSPtr> args;
	};

	struct ITrMemberAccess : ITrExpr
	{
		ITrMemberAccess(bool nullCoalesce, ITrExprSPtr expr, IdenSPtr iden);
		
		bool nullCoalesce;
		ITrExprSPtr expr;
		IdenSPtr iden;
	};

	struct ITrTupleAccess : ITrExpr
	{
		ITrTupleAccess(ITrExprSPtr expr, bool nullCoalesce, u16 index);
		
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
		ITrAmbiguousAggrInit(ITrTypeSPtr type, StdVector<ITrArgSPtr>&& args, bool hasDefInit, ITrExprSPtr defExpr);

		ITrTypeSPtr type;
		StdVector<ITrArgSPtr> args;
		ITrExprSPtr defExpr;
		bool hasDefInit;
	};

	struct ITrStructInit : ITrExpr
	{
		ITrStructInit(ITrTypeSPtr type, StdVector<ITrArgSPtr>&& args, bool hasDefInit, ITrExprSPtr defExpr);

		ITrTypeSPtr type;
		StdVector<ITrArgSPtr> args;
		ITrExprSPtr defExpr;
		bool hasDefInit;

		StdVector<u32> argOrder;
	};

	struct ITrUnionInit : ITrExpr
	{
		ITrUnionInit(ITrTypeSPtr type, StdVector<ITrArgSPtr>&& args);

		ITrTypeSPtr type;
		StdVector<ITrArgSPtr> args;
	};

	struct ITrAdtAggrEnumInit : ITrExpr
	{
		ITrAdtAggrEnumInit(ITrTypeSPtr type, StdVector<ITrArgSPtr>&& args);

		ITrTypeSPtr type;
		StdVector<ITrArgSPtr> args;
		ITrExprSPtr defExpr;
		bool hasDefInit;
	};

	struct ITrTupleInit : ITrExpr
	{
		ITrTupleInit(StdVector<ITrExprSPtr>&& exprs);

		StdVector<ITrExprSPtr> exprs;
	};

	struct ITrArrayInit : ITrExpr
	{
		ITrArrayInit(StdVector<ITrExprSPtr>&& exprs);

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
		ITrBlockExpr(const StdString& scopeName, StdVector<ITrStmtSPtr> stmts);

		StdVector<ITrStmtSPtr> stmts;
		StdString scopeName;
	};

	struct ITrUnsafeExpr : ITrExpr
	{
		ITrUnsafeExpr(ITrExprSPtr expr);

		ITrExprSPtr expr;
	};

	struct ITrMove : ITrExpr
	{
		ITrMove(ITrExprSPtr expr);

		ITrExprSPtr expr;
	};

	struct ITrComma : ITrExpr
	{
		ITrComma(StdVector<ITrExprSPtr>&& exprs);

		StdVector<ITrExprSPtr> exprs;
	};

	struct ITrClosure : ITrExpr
	{
		ITrClosure(ITrDefSPtr def);

		ITrDefSPtr def;
	};

	struct ITrIs : ITrExpr
	{
		ITrIs(ITrExprSPtr expr, ITrTypeSPtr type);

		ITrExprSPtr expr;
		ITrTypeSPtr type;
	};

	struct ITrTry : ITrExpr
	{
		ITrTry(ITrExprSPtr expr);

		ITrExprSPtr expr;
	};

	struct ITrSpecKw : ITrExpr
	{
		ITrSpecKw(TokenType kw);

		TokenType kw;
	};

	struct ITrCompRun : ITrExpr
	{
		ITrCompRun(ITrExprSPtr expr);

		ITrExprSPtr expr;
	};

	struct  ITrType
	{
		ITrType(ITrAttribsSPtr attribs, TypeHandle handle, StdVector<ITrTypeSPtr>&& subTypes, ITrExprSPtr expr = nullptr);

		ITrAttribsSPtr attribs;
		StdVector<ITrTypeSPtr> subTypes;
		ITrExprSPtr expr;
		TypeHandle handle;
		AstTypeSPtr astNode;
		u64 bodyIdx;
	};

	struct ITrPattern
	{
		ITrPattern(ITrPatternKind kind);
		
		ITrPatternKind patternKind;
		TypeHandle patternType;
		AstPatternSPtr astNode;
	};

	struct ITrPlaceholderPattern : ITrPattern
	{
		ITrPlaceholderPattern(bool isWildcard);
	};

	struct ITrValueBindPattern : ITrPattern
	{
		ITrValueBindPattern(IdenSPtr iden, ITrPatternSPtr subPattern);
		
		IdenSPtr iden;
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
		ITrTuplePattern(StdVector<ITrPatternSPtr>&& subPatterns);

		StdVector<ITrPatternSPtr> subPatterns;
	};

	struct ITrValueEnumPattern : ITrPattern
	{
		ITrValueEnumPattern(QualNameSPtr qualName);

		QualNameSPtr qualName;
	};

	struct ITrAdtTupleEnumPattern : ITrPattern
	{
		ITrAdtTupleEnumPattern(QualNameSPtr qualName, StdVector<ITrPatternSPtr>&& subPatterns);

		QualNameSPtr qualName;
		StdVector<ITrPatternSPtr> subPatterns;
	};

	struct ITrAmbiguousAggrPattern : ITrPattern
	{
		ITrAmbiguousAggrPattern(QualNameSPtr qualName, StdPairVector<StdString, ITrPatternSPtr>&& args);

		QualNameSPtr qualName;
		StdPairVector<StdString, ITrPatternSPtr> args;
	};

	struct ITrAggrPattern : ITrPattern
	{
		ITrAggrPattern(QualNameSPtr qualName, StdPairVector<StdString, ITrPatternSPtr>&& args);

		QualNameSPtr qualName;
		StdPairVector<StdString, ITrPatternSPtr> args;
	};

	struct ITrAdtAggrEnumPattern : ITrPattern
	{
		ITrAdtAggrEnumPattern(QualNameSPtr qualName, StdPairVector<StdString, ITrPatternSPtr>&& args);

		QualNameSPtr qualName;
		StdPairVector<StdString, ITrPatternSPtr> args;
	};

	struct ITrSlicePattern : ITrPattern
	{
		ITrSlicePattern(StdVector<ITrPatternSPtr>&& subPatterns);

		StdVector<ITrPatternSPtr> subPatterns;
	};

	struct ITrEitherPattern : ITrPattern
	{
		ITrEitherPattern(StdVector<ITrPatternSPtr>&& subPatterns);

		StdVector<ITrPatternSPtr> subPatterns;
	};

	struct ITrTypePattern : ITrPattern
	{
		ITrTypePattern(ITrTypeSPtr type);
		
		ITrTypeSPtr type;
	};

	struct ITrAttribs
	{
		ITrAttribs(Visibility vis, Attribute attribs, StdVector<ITrAtAttribSPtr>&& atAttribs);

		Visibility vis;
		Attribute attribs;
		StdVector<ITrAtAttribSPtr> atAttribs;

		AstAttribsSPtr astNode;
	};

	struct ITrAtAttrib
	{
		ITrAtAttrib(bool isCompAttrib, IdenSPtr iden, StdVector<ITrArgSPtr>&& args);

		bool isCompAttrib;
		IdenSPtr iden;
		StdVector<ITrArgSPtr> args;
	};

	FWDECL_STRUCT_SPTR(ITrGenParam);
	FWDECL_STRUCT_SPTR(ITrGenTypeBound);
	
	struct ITrGenDecl
	{
		StdVector<ITrGenParamSPtr> params;
		StdVector<ITrGenTypeBoundSPtr> bounds;
		AstGenericDeclSPtr astNode;
	};

	struct ITrGenParam
	{
		ITrGenParam(bool isType);
		
		bool isType;
		SymbolWPtr sym;
	};

	struct ITrGenTypeParam : ITrGenParam
	{
		ITrGenTypeParam(IdenSPtr name, ITrTypeSPtr defType);
		
		IdenSPtr iden;
		ITrTypeSPtr defType;
	};
	
	struct ITrGenValParam : ITrGenParam
	{
		ITrGenValParam(IdenSPtr iden, ITrTypeSPtr type, ITrExprSPtr defExpr);

		IdenSPtr iden;
		ITrTypeSPtr type;
		ITrExprSPtr defExpr;
	};

	FWDECL_STRUCT_SPTR(ITrGenBoundType);
	
	struct ITrGenAssocBound
	{
		ITrGenAssocBound(const StdString& iden, ITrGenBoundTypeSPtr type);
		
		StdString iden;
		ITrGenBoundTypeSPtr type;
	};
	
	struct ITrGenBoundType
	{
		ITrGenBoundType(ITrTypeSPtr type, StdVector<ITrGenAssocBound>&& assocBounds);
		
		ITrTypeSPtr type;
		StdVector<ITrGenAssocBound> assocBounds;
	};
	
	struct ITrGenTypeBound
	{
		ITrGenTypeBound(ITrTypeSPtr type, ITrGenBoundTypeSPtr bound);
		
		ITrTypeSPtr type;
		ITrGenBoundTypeSPtr bound;
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
