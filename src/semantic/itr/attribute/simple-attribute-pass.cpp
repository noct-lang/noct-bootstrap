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
			CheckAttribs(node, Attribute::None, "struct");
			Walk(node);
		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrUnion& node)
		{
			CheckAttribs(node, Attribute::None, "union");
			Walk(node);

		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrValEnum& node)
		{
			CheckAttribs(node, Attribute::None, "enum");
			Walk(node);

		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrAdtEnum& node)
		{
			CheckAttribs(node, Attribute::None, "adt enum");
			Walk(node);

		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrMarkerInterface& node)
		{
			CheckAttribs(node, Attribute::None, "marker interface");
			Walk(node);

		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrWeakInterface& node)
		{
			CheckAttribs(node, Attribute::None, "weak interface");
			Walk(node);

		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrStrongInterface& node)
		{
			CheckAttribs(node, Attribute::None, "strong interface");
			Walk(node);

		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrTypealias& node)
		{
			CheckAttribs(node, Attribute::None, "typealias");
			Walk(node);

		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrTypedef& node)
		{
			CheckAttribs(node, Attribute::None, "typedef");
			Walk(node);

		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrVar& node)
		{
			Attribute validMask;
			StdStringView defName;
			if (node.isModDef)
			{
				validMask = Attribute::Const | Attribute::Mut;
				defName = "global variable";
			}
			else
			{
				validMask = Attribute::Const | Attribute::Mut | Attribute::Static;
				defName = "member variable";
			}
			CheckAttribs(node, validMask, defName);

			if (node.attribs)
			{
				Attribute attribs = node.attribs->attribs;
				Attribute singleMask = Attribute::Const | Attribute::Mut;
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

			Attribute validMask = Attribute::Comptime | Attribute::Const;
			StdStringView defName;
			switch (node.funcKind)
			{
			case ITrFuncKind::Func:
			{
				defName = "function";
				break;
			}
			case ITrFuncKind::Method:
			{
				defName = "method";
				break;
			}
			case ITrFuncKind::EmptyMethod:
			{
				defName = "empty method";
				break;
			}
			case ITrFuncKind::Closure:
			{
				validMask = Attribute::None;
				defName = "closure";
				break;
			}
			}
			CheckAttribs(node, validMask, defName);

			// params
			for (ITrParamSPtr param : node.params)
			{
				if (!param->attribs)
					continue;
				
				Attribute attribs = param->attribs->attribs;
				Attribute paramValidMask = Attribute::Comptime | Attribute::Lazy | Attribute::Move;
				SpanId spanId = param->astNode->ctx->startIdx;
				CheckAttribs(attribs, paramValidMask, spanId, "function parameter");
				CheckSingleAttrib(attribs, paramValidMask, spanId, "parameter");
			}

			Walk(node);

		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrImpl& node)
		{
			CheckAttribs(node, Attribute::None, "impl");
			Walk(node);

		});
	}

	void SimpleAttributePass::Visit(ITrLocalVar& node)
	{
		if (node.attribs)
		{
			Attribute attribs = node.attribs->attribs;
			Attribute validMask = Attribute::Const | Attribute::Mut | Attribute::Lazy;
			SpanId spanId = std::get<AstStmtSPtr>(node.astNode)->ctx->startIdx;
			CheckAttribs(attribs, validMask, spanId, "local variable");
			CheckSingleAttrib(attribs, validMask, spanId, "local variable");
		}

		Walk(node);
	}

	void SimpleAttributePass::Visit(ITrType& node)
	{
		Walk(node);

		if (node.attribs)
		{
			Attribute attribs = node.attribs->attribs;
			Attribute validMask = Attribute::Mut;
			SpanId spanId = node.astNode->ctx->startIdx;
			CheckAttribs(attribs, validMask, spanId, "type");
			CheckSingleAttrib(attribs, validMask, spanId, "type");
		}
	}

	void SimpleAttributePass::CheckAttribs(ITrDef& node, Attribute validMask, StdStringView defName)
	{
		if (node.attribs)
		{
			Attribute attribs = node.attribs->attribs;
			Attribute invalid = attribs & ~validMask;

			if (invalid != Attribute::None)
			{
				Span span = m_pCtx->spanManager.GetSpan(node.astNode->ctx->startIdx);
				StdString name = ToString(invalid);
				g_ErrorSystem.Error(span, "'%s' are invalid for %s definitions", name.c_str(), defName.data());
			}
		}
	}

	void SimpleAttributePass::CheckAttribs(Attribute attribs, Attribute validMask, SpanId spanId,
		StdStringView defName)
	{
		Attribute invalid = attribs & ~validMask;

		if (invalid != Attribute::None)
		{
			Span span = m_pCtx->spanManager.GetSpan(spanId);
			StdString name = ToString(invalid);
			g_ErrorSystem.Error(span, "'%s' are invalid for %s definitions", name.c_str(), defName.data());
		}
	}

	void SimpleAttributePass::CheckSingleAttrib(Attribute attribs, Attribute singleMask, SpanId spanId
												, StdStringView defName)
	{
		Attribute validSingle = attribs & singleMask;
		if (CountBits(validSingle) > 1)
		{
			Span span = m_pCtx->spanManager.GetSpan(spanId);
			StdString name = ToString(validSingle);
			const char* pName = name.c_str();
			g_ErrorSystem.Error(span, "'%s' can't be used together for a %s, only one of these is allowed per parameter", name.c_str(), defName.data());
		}
	}
}
