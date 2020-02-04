#include "idenscopepass.hpp"
#include "common/utils.hpp"
#include "common/context.hpp"
#include "common/compcontext.hpp"

namespace Noctis
{
	IdenScopePass::IdenScopePass(Context* pCtx)
		: SemanticPass("IdenScopePass", pCtx)
		, m_AllowVisit(true)
	{
	}

	void IdenScopePass::Visit(AstTree& tree)
	{
		std::hash<StdString> hasher;
		usize hash = hasher(tree.filepath);
		m_FileNameHash = Format("%llx", hash);
		Walk(tree);
	}

	void IdenScopePass::Visit(AstTypeDisambiguation& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstIden& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstQualName& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstParam& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstArg& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstModuleDecl& node)
	{
		m_CurScope = QualName::Create(node.moduleIdens);
		Walk(node);
	}

	void IdenScopePass::Visit(AstUnittestDecl& node)
	{
		UnnamedScope(node.ctx, "unittest");
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstBenchmarkDecl& node)
	{
		UnnamedScope(node.ctx, "benchmark");
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstStructDecl& node)
	{
		GenericScope(node.ctx, node.iden, node.generics);
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstUnionDecl& node)
	{
		GenericScope(node.ctx, node.iden, node.generics);
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstValueEnumDecl& node)
	{
		IdenScope(node.ctx, node.iden);
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstAdtEnumDecl& node)
	{
		GenericScope(node.ctx, node.iden, node.generics);
		if (node.attribs)
			Visit(*node.attribs);
		if (node.generics)
			Visit(*node.generics);
		for (StdPair<StdString, AstTypeSPtr>& member : node.members)
		{
			if (member.second)
			{
				if (member.second->typeKind == AstTypeKind::InlineStruct)
				{
					AstInlineStructType& structType = *static_cast<AstInlineStructType*>(member.second.get());
					IdenScope(node.ctx, node.iden + "__struct");
					for (std::pair<StdVector<StdString>, AstTypeSPtr>& structMember : structType.members)
					{
						AstVisitor::Visit(structMember.second);
					}
					m_CurScope = m_CurScope->Base();
				}
				else
				{
					AstVisitor::Visit(member.second);
				}
			}
		}
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstMarkerInterfaceDecl& node)
	{
		IdenScope(node.ctx, node.iden);
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstStrongInterfaceDecl& node)
	{
		GenericScope(node.ctx, node.iden, node.generics);
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstWeakInterfaceDecl& node)
	{
		IdenScope(node.ctx, node.iden);
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstTypeAliasDecl& node)
	{
		GenericScope(node.ctx, node.iden, node.generics);
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstTypeDefDecl& node)
	{
		GenericScope(node.ctx, node.iden, node.generics);
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstVarDecl& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstFuncDecl& node)
	{
		GenericScope(node.ctx, node.iden, node.generics);
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstMethodDecl& node)
	{
		GenericScope(node.ctx, node.iden, node.generics);
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstEmptyMethodDecl& node)
	{
		GenericScope(node.ctx, node.iden, node.generics);
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstImplDecl& node)
	{
		UnnamedScope(node.ctx, "impl");
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstImportStmt& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstBlockStmt& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstIfStmt& node)
	{
		UnnamedScope(node.ctx, "block");
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstLoopStmt& node)
	{
		UnnamedScope(node.ctx, "loop");
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstWhileStmt& node)
	{
		UnnamedScope(node.ctx, "while");
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstDoWhileStmt& node)
	{
		UnnamedScope(node.ctx, "do_while");
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstForStmt& node)
	{
		UnnamedScope(node.ctx, "for");
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstSwitchStmt& node)
	{
		UnnamedScope(node.ctx, "switch");
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstLabelStmt& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstBreakStmt& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstContinueStmt& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstFallthroughStmt& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstGotoStmt& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstReturnStmt& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstExprStmt& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstDeferStmt& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstErrDeferStmt& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstUnsafeStmt& node)
	{
		UnnamedScope(node.ctx, "unsafe");
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstErrorHandlerStmt& node)
	{
		IdenScope(node.ctx, "__errhandler");
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstCompIfStmt& node)
	{
		UnnamedScope(node.ctx, "comp_if");
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstCompCondStmt& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstCompDebugStmt& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstMacroLoopStmt& node)
	{
		UnnamedScope(node.ctx, "macro_loop");
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstAssignExpr& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstTernaryExpr& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstBinaryExpr& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstPostfixExpr& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstPrefixExpr& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstQualNameExpr& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstIndexSliceExpr& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstSliceExpr& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstFuncCallExpr& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstMemberAccessExpr& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstMethodCallExpr& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstTupleAccessExpr& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstLiteralExpr& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstAggrInitExpr& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstTupleInitExpr& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstArrayInitExpr& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstCastExpr& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstTransmuteExpr& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstMoveExpr& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstBracketExpr& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstBlockExpr& node)
	{
		UnnamedScope(node.ctx, "block_expr");
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstUnsafeExpr& node)
	{
		UnnamedScope(node.ctx, "unsafe_expr");
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstCommaExpr& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstClosureExpr& node)
	{
		UnnamedScope(node.ctx, "closure");
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstIsExpr& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstCompRunExpr& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstMacroVarExpr& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstBuiltinType& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstIdentifierType& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstPointerType& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstReferenceType& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstArrayType& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstSliceType& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstTupleType& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstOptionalType& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstInlineStructType& node)
	{
		UnnamedScope(node.ctx, "inl_struct");
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstInlineEnumType& node)
	{
		UnnamedScope(node.ctx, "inl_enum");
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstCompoundInterfaceType& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstPlaceholderPattern& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstWildcardPattern& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstValueBindPattern& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstLiteralPattern& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstRangePattern& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstTuplePattern& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstEnumPattern& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstAggrPattern& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstSlicePattern& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstEitherPattern& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstTypePattern& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstAttribs& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstCompAttrib& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstUserAttrib& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstVisibilityAttrib& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstSimpleAttrib& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstGenericDecl& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstGenericValueParam& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstGenericTypeParam& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstGenericTypeBound& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstGenericWhereClause& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstMacroVar& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstMacroSeparator& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstMacroFragment& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstMacroPattern& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstMacroRule& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstDeclMacro& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstRulesDeclMacro& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstProcMacro& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstRulesProcMacro& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::Visit(AstMacroInst& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
	}

	void IdenScopePass::GenericScope(std::unique_ptr<AstContext>& ctx, StdStringView name, AstGenericDeclSPtr generics)
	{
		IdenSPtr iden;
		if (generics)
		{
			AstGenericDecl* pGenerics = static_cast<AstGenericDecl*>(generics.get());
			StdString idenStr = Format("%s__gen_%u", name.data(), pGenerics->params.size());
			iden = Iden::Create(idenStr);
		}
		else
		{
			iden = Iden::Create(name);
		}

		ctx->iden = iden;
		ctx->scope = m_CurScope;
		m_CurScope = QualName::Create(m_CurScope, iden);
	}

	void IdenScopePass::IdenScope(std::unique_ptr<AstContext>& ctx, StdStringView name)
	{
		IdenSPtr iden = Iden::Create(name);
		ctx->iden = iden;
		ctx->scope = m_CurScope;
		m_CurScope = QualName::Create(m_CurScope, iden);
	}

	void IdenScopePass::UnnamedScope(std::unique_ptr<AstContext>& ctx, StdStringView scopeName)
	{
		StdString idenStr = Format("__%s_%s_%u", scopeName.data(), m_FileNameHash.c_str(), ctx->startIdx);
		IdenSPtr iden = Iden::Create(idenStr);
		ctx->iden = iden;
		ctx->scope = m_CurScope;
		m_CurScope = QualName::Create(m_CurScope, iden);
	}
}
