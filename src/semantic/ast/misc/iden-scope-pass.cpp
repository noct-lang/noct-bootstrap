#include "iden-scope-pass.hpp"
#include "common/utils.hpp"
#include "common/context.hpp"

namespace Noctis
{
	IdenScopePass::IdenScopePass()
		: AstSemanticPass("iden-scope pass")
		, m_pTree(nullptr)
	{
	}

	void IdenScopePass::Process(AstTree& tree)
	{
		std::hash<StdString> hasher;
		usize hash = hasher(tree.filepath);
		m_FileName = tree.filepath + Format("_%llx", hash);
		StringReplace(m_FileName, ".", "_");
		
		usize slash = m_FileName.rfind("/");
		if (slash != StdString::npos)
			m_FileName = m_FileName.substr(slash + 1);
		slash = m_FileName.rfind("\\");
		if (slash != StdString::npos)
			m_FileName = m_FileName.substr(slash + 1);
		
		m_pTree = &tree;
		m_CurScope = tree.moduleScope;
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
		StdVector<IdenGeneric> idenGens;
		for (AstGenericArg& arg : node.args)
		{
			IdenGeneric idenArg;
			idenArg.isType = arg.kind == GenericArgKind::Expr;
			idenArg.isSpecialized = true;
			idenGens.push_back(idenArg);
		}
		node.ctx->qualName = m_CurScope->Append(node.iden, idenGens);
	}

	void IdenScopePass::Visit(AstQualName& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);

		StdVector<StdString> idens;
		StdVector<IdenGeneric> idenGens;
		for (AstQualIdenSPtr iden : node.idens)
		{
			if (iden->qualIdenKind != AstQualIdenKind::Identifier)
				continue;
			
			AstIden& astIden = static_cast<AstIden&>(*iden);
			idens.push_back(astIden.iden);

			if (!astIden.ctx->qualName)
				continue;
			
			const StdVector<IdenGeneric> generics = astIden.ctx->qualName->Generics();
			if (generics.empty())
				idenGens.insert(idenGens.end(), generics.begin(), generics.end());
		}
		node.ctx->qualName = QualName::Create(idens, idenGens);
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

	void IdenScopePass::Visit(AstUnittestDecl& node)
	{
		UnnamedScope(node.ctx, "unittest", nullptr);
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstBenchmarkDecl& node)
	{
		UnnamedScope(node.ctx, "benchmark", nullptr);
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
					m_InlinePrefix = member.first;
				AstVisitor::Visit(member.second);
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
		FuncScope(node.ctx, node.iden, node.generics);
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstMethodDecl& node)
	{
		GenericScope(node.ctx, node.iden, node.generics);
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstImplDecl& node)
	{
		UnnamedScope(node.ctx, "impl", node.generics);
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
		UnnamedScope(node.ctx, "if", nullptr);
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstLoopStmt& node)
	{
		UnnamedScope(node.ctx, "loop", nullptr);
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstWhileStmt& node)
	{
		UnnamedScope(node.ctx, "while", nullptr);
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstDoWhileStmt& node)
	{
		UnnamedScope(node.ctx, "do_while", nullptr);
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstForStmt& node)
	{
		UnnamedScope(node.ctx, "for", nullptr);
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstSwitchStmt& node)
	{
		UnnamedScope(node.ctx, "switch", nullptr);
		AstVisitor::Visit(node.cond);

		for (usize i = 0; i < node.cases.size(); ++i)
		{
			AstSwitchCase& case_ = node.cases[i];
			AstBlockStmt& body = static_cast<AstBlockStmt&>(*case_.body);
			StdString name = Format("case_%u", i);
			IdenScope(body.ctx, name);

			AstVisitor::Visit(case_.pattern);
			if (case_.expr)
				AstVisitor::Visit(case_.expr);
			AstVisitor::Visit(case_.body);

			m_CurScope = m_CurScope->Base();
		}
		
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
		UnnamedScope(node.ctx, "unsafe", nullptr);
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstErrHandler& node)
	{
		StdString name = Format("__errhandler_%u", node.ctx->startIdx);
		IdenScope(node.ctx, name);
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstCompIfStmt& node)
	{
		UnnamedScope(node.ctx, "comp_if", nullptr);
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
		//UnnamedScope(node.ctx, "macro_loop");
		Walk(node);
		//m_CurScope = m_CurScope->Base();
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

	void IdenScopePass::Visit(AstRangeExpr& node)
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
		node.ctx->qualName = node.qualName->ctx->qualName;
	}

	void IdenScopePass::Visit(AstIndexSliceExpr& node)
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
		UnnamedScope(node.ctx, "block_expr", nullptr);
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstUnsafeExpr& node)
	{
		UnnamedScope(node.ctx, "unsafe_expr", nullptr);
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
		UnnamedScope(node.ctx, "closure", nullptr);
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
		if (!m_InlinePrefix.empty())
		{
			IdenScope(node.ctx, m_InlinePrefix + "__struct");
			m_InlinePrefix.clear();
		}
		else
		{
			UnnamedScope(node.ctx, "inl_struct", nullptr);
		}
		Walk(node);
		m_CurScope = m_CurScope->Base();
	}

	void IdenScopePass::Visit(AstInlineEnumType& node)
	{
		UnnamedScope(node.ctx, "inl_enum", nullptr);
		Walk(node);
		m_CurScope = m_CurScope->Base();
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
		node.ctx->qualName = m_CurScope->Append(node.iden);
		Walk(node);
	}

	void IdenScopePass::Visit(AstRulesDeclMacro& node)
	{
		node.ctx->scope = m_CurScope;
		node.ctx->qualName = m_CurScope->Append(node.iden);
		Walk(node);
	}

	void IdenScopePass::Visit(AstProcMacro& node)
	{
		node.ctx->scope = m_CurScope;
		node.ctx->qualName = m_CurScope->Append(node.iden);
		Walk(node);
	}

	void IdenScopePass::Visit(AstRulesProcMacro& node)
	{
		node.ctx->scope = m_CurScope;
		node.ctx->qualName = m_CurScope->Append(node.iden);
		Walk(node);
	}

	void IdenScopePass::Visit(AstMacroInstStmt& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
		node.ctx->qualName = node.qualName->ctx->qualName;
	}

	void IdenScopePass::Visit(AstMacroInstExpr& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
		node.ctx->qualName = node.qualName->ctx->qualName;
	}

	void IdenScopePass::Visit(AstMacroInstPattern& node)
	{
		node.ctx->scope = m_CurScope;
		Walk(node);
		node.ctx->qualName = node.qualName->ctx->qualName;
	}

	void IdenScopePass::GenericScope(AstContextPtr& ctx, StdStringView name, AstGenericDeclSPtr generics)
	{
		if (!ctx->qualName)
		{
			StdVector<IdenGeneric> idenGens;
			if (generics)
			{
				AstGenericDecl* pGenerics = static_cast<AstGenericDecl*>(generics.get());
				for (AstGenericParam& genParam : pGenerics->params)
				{
					IdenGeneric gen;
					gen.isType = genParam.kind == AstGenericParamKind::TypeParam || genParam.kind == AstGenericParamKind::TypeSpec;
					idenGens.push_back(gen);
				}
			}

			ctx->scope = m_CurScope;
			ctx->qualName = m_CurScope->Append(StdString{ name }, idenGens);
		}
		m_CurScope = ctx->qualName;
	}

	void IdenScopePass::IdenScope(AstContextPtr& ctx, StdStringView name)
	{
		if (!ctx->qualName)
		{
			ctx->scope = m_CurScope;
			ctx->qualName = m_CurScope->Append(StdString{ name });
		}
		m_CurScope = ctx->qualName;
	}

	void IdenScopePass::UnnamedScope(AstContextPtr& ctx, StdStringView name, AstGenericDeclSPtr generics)
	{
		if (!ctx->qualName)
		{
			u64 genId = m_pTree->genId;
			++m_pTree->genId;
			StdString idenStr = Format("__%s_%s_%u", name.data(), m_FileName.c_str(), genId);

			StdVector<IdenGeneric> idenGens;
			if (generics)
			{
				AstGenericDecl* pGenerics = static_cast<AstGenericDecl*>(generics.get());
				for (AstGenericParam& param : pGenerics->params)
				{
					IdenGeneric idenParam;
					idenParam.isType = param.kind == AstGenericParamKind::TypeParam || param.kind == AstGenericParamKind::TypeSpec;
					idenGens.push_back(idenParam);
				}
			}
			
			ctx->scope = m_CurScope;
			ctx->qualName = m_CurScope->Append(idenStr, idenGens);
		}
		m_CurScope = ctx->qualName;
	}

	void IdenScopePass::FuncScope(AstContextPtr& ctx, StdStringView name, AstGenericDeclSPtr generics)
	{
		if (!ctx->qualName)
		{
			StdVector<IdenGeneric> idenGens;
			if (generics)
			{
				AstGenericDecl* pGenerics = static_cast<AstGenericDecl*>(generics.get());
				for (AstGenericParam& param : pGenerics->params)
				{
					IdenGeneric idenParam;
					idenParam.isType = param.kind == AstGenericParamKind::TypeParam || param.kind == AstGenericParamKind::TypeSpec;
					idenGens.push_back(idenParam);
				}
			}

			ctx->scope = m_CurScope;
			ctx->qualName = m_CurScope->Append(StdString{ name }, idenGens);
		}
		m_CurScope = ctx->qualName;
	}
}
