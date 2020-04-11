#include "simple-attribute-pass.hpp"
#include "ast/ast.hpp"
#include "common/context.hpp"
#include "common/errorsystem.hpp"
#include "common/logger.hpp"
#include "itr/itr.hpp"

namespace Noctis
{
	SimpleAttributePass::SimpleAttributePass(Context* pCtx)
		: ITrSemanticPass("simple attribute pass", pCtx)
	{
	}

	void SimpleAttributePass::Process(ITrModule& mod)
	{
		SetModule(mod);
		
		Foreach(ITrVisitorDefKind::Any, [this](ITrStruct& node)
		{
			if (node.attribs)
			{
				Attribute attribs = node.attribs->attribs;
				Attribute invalidMask = ~(Attribute::None);
				Attribute invalid = attribs & invalidMask;

				if (invalid != Attribute::None)
				{
					Span span = m_pCtx->spanManager.GetSpan(node.astNode->ctx->startIdx);
					StdString name = ToString(invalid);
					const char* pName = name.c_str();
					g_ErrorSystem.Error(span, "'%s' are invalid for struct definitions", pName);
				}
			}

			Walk(node);
		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrUnion& node)
		{
			if (node.attribs)
			{
				Attribute attribs = node.attribs->attribs;
				Attribute invalidMask = ~(Attribute::None);
				Attribute invalid = attribs & invalidMask;

				if (invalid != Attribute::None)
				{
					Span span = m_pCtx->spanManager.GetSpan(node.astNode->ctx->startIdx);
					StdString name = ToString(invalid);
					const char* pName = name.c_str();
					g_ErrorSystem.Error(span, "'%s' are invalid for union definitions", pName);
				}
			}

			Walk(node);

		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrValEnum& node)
		{
			if (node.attribs)
			{
				Attribute attribs = node.attribs->attribs;
				Attribute invalidMask = ~(Attribute::None);
				Attribute invalid = attribs & invalidMask;

				if (invalid != Attribute::None)
				{
					Span span = m_pCtx->spanManager.GetSpan(node.astNode->ctx->startIdx);
					StdString name = ToString(invalid);
					const char* pName = name.c_str();
					g_ErrorSystem.Error(span, "'%s' are invalid for value enum definitions", pName);
				}
			}

			Walk(node);

		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrAdtEnum& node)
		{
			if (node.attribs)
			{
				Attribute attribs = node.attribs->attribs;
				Attribute invalidMask = ~(Attribute::None);
				Attribute invalid = attribs & invalidMask;

				if (invalid != Attribute::None)
				{
					Span span = m_pCtx->spanManager.GetSpan(node.astNode->ctx->startIdx);
					StdString name = ToString(invalid);
					const char* pName = name.c_str();
					g_ErrorSystem.Error(span, "'%s' are invalid for adt enum definitions", pName);
				}
			}

			Walk(node);

		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrMarkerInterface& node)
		{
			if (node.attribs)
			{
				Attribute attribs = node.attribs->attribs;
				Attribute invalidMask = ~(Attribute::None);
				Attribute invalid = attribs & invalidMask;

				if (invalid != Attribute::None)
				{
					Span span = m_pCtx->spanManager.GetSpan(node.astNode->ctx->startIdx);
					StdString name = ToString(invalid);
					const char* pName = name.c_str();
					g_ErrorSystem.Error(span, "'%s' are invalid for marker interface definitions", pName);
				}
			}

			Walk(node);

		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrWeakInterface& node)
		{
			if (node.attribs)
			{
				Attribute attribs = node.attribs->attribs;
				Attribute invalidMask = ~(Attribute::None);
				Attribute invalid = attribs & invalidMask;

				if (invalid != Attribute::None)
				{
					Span span = m_pCtx->spanManager.GetSpan(node.astNode->ctx->startIdx);
					StdString name = ToString(invalid);
					const char* pName = name.c_str();
					g_ErrorSystem.Error(span, "'%s' are invalid for weak interface definitions", pName);
				}
			}

			Walk(node);

		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrStrongInterface& node)
		{
			if (node.attribs)
			{
				Attribute attribs = node.attribs->attribs;
				Attribute invalidMask = ~(Attribute::None);
				Attribute invalid = attribs & invalidMask;

				if (invalid != Attribute::None)
				{
					Span span = m_pCtx->spanManager.GetSpan(node.astNode->ctx->startIdx);
					StdString name = ToString(invalid);
					const char* pName = name.c_str();
					g_ErrorSystem.Error(span, "'%s' are invalid for strong interface definitions", pName);
				}
			}

			Walk(node);

		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrTypealias& node)
		{
			if (node.attribs)
			{
				Attribute attribs = node.attribs->attribs;
				Attribute invalidMask = ~(Attribute::None);
				Attribute invalid = attribs & invalidMask;

				if (invalid != Attribute::None)
				{
					Span span = m_pCtx->spanManager.GetSpan(node.astNode->ctx->startIdx);
					StdString name = ToString(invalid);
					const char* pName = name.c_str();
					g_ErrorSystem.Error(span, "'%s' are invalid for typealias definitions", pName);
				}
			}

			Walk(node);

		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrTypedef& node)
		{
			if (node.attribs)
			{
				Attribute attribs = node.attribs->attribs;
				Attribute invalidMask = ~(Attribute::None);
				Attribute invalid = attribs & invalidMask;

				if (invalid != Attribute::None)
				{
					Span span = m_pCtx->spanManager.GetSpan(node.astNode->ctx->startIdx);
					StdString name = ToString(invalid);
					const char* pName = name.c_str();
					g_ErrorSystem.Error(span, "'%s' are invalid for typedef definitions", pName);
				}
			}

			Walk(node);

		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrVar& node)
		{
			if (node.attribs)
			{
				Attribute attribs = node.attribs->attribs;
				StdStringView invalidFormat;
				Attribute invalidMask;
				if (node.isModDef)
				{
					invalidMask = ~(Attribute::Const | Attribute::Immutable | Attribute::Comptime);
					invalidFormat = "'%s' are invalid for global variable definitions";
				}
				else
				{
					invalidMask = ~(Attribute::Const | Attribute::Immutable | Attribute::Static | Attribute::Comptime);
					invalidFormat = "'%s' are invalid for member variable definitions";
				}

				Attribute invalid = attribs & invalidMask;
				if (invalid != Attribute::None)
				{
					Span span = m_pCtx->spanManager.GetSpan(node.astNode->ctx->startIdx);
					StdString name = ToString(invalid);
					const char* pName = name.c_str();
					g_ErrorSystem.Error(span, invalidFormat, pName);
				}

				Attribute singleMask = Attribute::Const | Attribute::Immutable | Attribute::Comptime;
				Attribute validSingle = attribs & singleMask;
				if (CountBits(validSingle) > 1)
				{
					Span span = m_pCtx->spanManager.GetSpan(node.astNode->ctx->startIdx);
					StdString name = ToString(validSingle);
					const char* pName = name.c_str();
					g_ErrorSystem.Error(span, "'%s' can't be used together for a variable declaration, only one of these is allowed per variable declaration", pName);
				}
				
			}

			Walk(node);
		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrFunc& node)
		{
			if (node.attribs)
			{
				Attribute attribs = node.attribs->attribs;
				StdStringView format;
				Attribute invalidMask;

				switch (node.funcKind)
				{
				default:
				{
					invalidMask = ~(Attribute::Comptime);
					format = "'%s' are invalid for function definitions";
					break;
				}
				case ITrFuncKind::Method:
				{
					invalidMask = ~(Attribute::Static | Attribute::Comptime);
					format = "'%s' are invalid for method definitions";
					break;
				}
				case ITrFuncKind::EmptyMethod:
				{
					invalidMask = ~(Attribute::Static | Attribute::Comptime);
					format = "'%s' are invalid for empty method definitions";
					break;
				}
				case ITrFuncKind::Closure:
				{
					invalidMask = ~(Attribute::None);
					format = "'%s' are invalid for closure definitions";
					break;
				}
				}

				Attribute invalid = attribs & invalidMask;
				if (invalid != Attribute::None)
				{
					Span span = m_pCtx->spanManager.GetSpan(node.astNode->ctx->startIdx);
					StdString name = ToString(invalid);
					const char* pName = name.c_str();
					g_ErrorSystem.Error(span, format, pName);
				}
			}

			// params
			for (ITrParamSPtr param : node.params)
			{
				Attribute attribs = node.attribs->attribs;
				Attribute invalidMask = ~(Attribute::Comptime | Attribute::Lazy | Attribute::Move);
				Attribute invalid = attribs & invalidMask;
				if (invalid != Attribute::None)
				{
					Span span = m_pCtx->spanManager.GetSpan(node.astNode->ctx->startIdx);
					StdString name = ToString(invalid);
					const char* pName = name.c_str();
					g_ErrorSystem.Error(span, "'%s' are invalid for function parameters", pName);
				}

				Attribute singleMask = ~invalidMask;
				Attribute validSingle = attribs & singleMask;
				if (CountBits(validSingle) > 1)
				{
					Span span = m_pCtx->spanManager.GetSpan(node.astNode->ctx->startIdx);
					StdString name = ToString(validSingle);
					const char* pName = name.c_str();
					g_ErrorSystem.Error(span, "'%s' can't be used together for a parameter, only one of these is allowed per parameter", pName);
				}
			}

			Walk(node);

		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrImpl& node)
		{
			if (node.attribs)
			{
				Attribute attribs = node.attribs->attribs;
				Attribute invalidMask = ~(Attribute::None);
				Attribute invalid = attribs & invalidMask;

				if (invalid != Attribute::None)
				{
					Span span = m_pCtx->spanManager.GetSpan(node.astNode->ctx->startIdx);
					StdString name = ToString(invalid);
					const char* pName = name.c_str();
					g_ErrorSystem.Error(span, "'%s' are invalid for impl definitions", pName);
				}
			}

			Walk(node);

		});
	}

	void SimpleAttributePass::Visit(ITrLocalVar& node)
	{
		if (node.attribs)
		{
			Attribute attribs = node.attribs->attribs;
			Attribute invalidMask = ~(Attribute::Const | Attribute::Immutable | Attribute::Comptime | Attribute::Lazy);
			Attribute invalid = attribs & invalidMask;

			if (invalid != Attribute::None)
			{
				Span span = m_pCtx->spanManager.GetSpan(std::get<AstStmtSPtr>(node.astNode)->ctx->startIdx);
				StdString name = ToString(invalid);
				const char* pName = name.c_str();
				g_ErrorSystem.Error(span, "'%s' are invalid for local variable definitions", pName);
			}

			Attribute singleMask = ~invalidMask;
			Attribute validSingle = attribs & singleMask;
			if (CountBits(validSingle) > 1)
			{
				Span span = m_pCtx->spanManager.GetSpan(std::get<AstStmtSPtr>(node.astNode)->ctx->startIdx);
				StdString name = ToString(validSingle);
				const char* pName = name.c_str();
				g_ErrorSystem.Error(span, "'%s' can't be used together for a parameter, only one of these is allowed per parameter", pName);
			}
		}

		Walk(node);
	}

	void SimpleAttributePass::Visit(ITrType& node)
	{
		Walk(node);

		if (node.attribs)
		{
			Attribute attribs = node.attribs->attribs;
			Attribute invalidMask = ~(Attribute::Const | Attribute::Immutable);
			Attribute invalid = attribs & invalidMask;

			if (invalid != Attribute::None)
			{
				Span span = m_pCtx->spanManager.GetSpan(node.astNode->ctx->startIdx);
				StdString name = ToString(invalid);
				const char* pName = name.c_str();
				g_ErrorSystem.Error(span, "'%s' are invalid for local variable definitions", pName);
			}

			Attribute singleMask = ~invalidMask;
			Attribute validSingle = attribs & singleMask;
			if (CountBits(validSingle) > 1)
			{
				Span span = m_pCtx->spanManager.GetSpan(node.astNode->ctx->startIdx);
				StdString name = ToString(validSingle);
				const char* pName = name.c_str();
				g_ErrorSystem.Error(span, "'%s' can't be used together for a type modifier, only one of these is allowed per (sub)type", pName);
			}

			TypeMod mod = TypeMod::None;
			if (ENUM_IS_SET(attribs, Attribute::Const))
				mod = TypeMod::Const;
			else if (ENUM_IS_SET(attribs, Attribute::Immutable))
				mod = TypeMod::Immutable;

			node.handle = m_pCtx->typeReg.Mod(mod, node.handle);
		}
	}
}
