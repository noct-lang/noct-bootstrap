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
					invalidMask = ~(Attribute::Const | Attribute::Mut);
					invalidFormat = "'%s' are invalid for global variable definitions";
				}
				else
				{
					invalidMask = ~(Attribute::Const | Attribute::Mut | Attribute::Static);
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
					invalidMask = ~(Attribute::Comptime);
					format = "'%s' are invalid for method definitions";
					break;
				}
				case ITrFuncKind::EmptyMethod:
				{
					invalidMask = ~(Attribute::Comptime);
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
				if (!param->attribs)
					continue;
				
				Attribute attribs = param->attribs->attribs;
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
			Attribute invalidMask = ~(Attribute::Const | Attribute::Mut | Attribute::Lazy);
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

		TypeMod mod = TypeMod::None;
		if (node.attribs)
		{
			Attribute attribs = node.attribs->attribs;
			Attribute invalidMask = ~(Attribute::Mut);
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

			if (ENUM_IS_SET(attribs, Attribute::Mut))
				mod = TypeMod::Mut;
		}

		TypeHandle handle = node.handle;
		TypeSPtr type = handle.Type();
		if (type->Mod() != TypeMod::None && mod == TypeMod::None)
			mod = type->Mod();
		
		switch (type->typeKind)
		{
		case TypeKind::Ptr:
		{
			if (!node.subTypes.empty())
			{
				ITrTypeSPtr subType = node.subTypes[0];
				node.handle = m_pCtx->typeReg.Ptr(mod, subType->handle);
			}
			break;
		}
		case TypeKind::Ref:
		{
			if (!node.subTypes.empty())
			{
				ITrTypeSPtr subType = node.subTypes[0];
				node.handle = m_pCtx->typeReg.Ref(mod, subType->handle);
			}
			break;
		}
		case TypeKind::Slice:
		{
			if (!node.subTypes.empty())
			{
				ITrTypeSPtr subType = node.subTypes[0];
				node.handle = m_pCtx->typeReg.Slice(mod, subType->handle);
			}
			break;
		}
		case TypeKind::Array:
		{
			if (!node.subTypes.empty())
			{
				ITrTypeSPtr subType = node.subTypes[0];
				node.handle = m_pCtx->typeReg.Array(mod, subType->handle, node.expr);
			}
			break;
		}
		case TypeKind::Tuple:
		{
			if (!node.subTypes.empty())
			{
				StdVector<TypeHandle> handles;
				for (ITrTypeSPtr subType : node.subTypes)
				{
					handles.push_back(subType->handle);
				}
				node.handle = m_pCtx->typeReg.Tuple(mod, handles);
			}
			break;
		}
		case TypeKind::Opt:
		{
			if (!node.subTypes.empty())
			{
				ITrTypeSPtr subType = node.subTypes[0];
				node.handle = m_pCtx->typeReg.Opt(mod, subType->handle);
			}
			break;
		}
		case TypeKind::Compound:
		{
			if (!node.subTypes.empty())
			{
				StdVector<TypeHandle> handles;
				for (ITrTypeSPtr subType : node.subTypes)
				{
					handles.push_back(subType->handle);
				}
				node.handle = m_pCtx->typeReg.Compound(mod, handles);
			}
			break;
		}
		case TypeKind::Func:
		{
			if (!node.subTypes.empty())
			{
				StdVector<TypeHandle> handles;
				for (ITrTypeSPtr subType : node.subTypes)
				{
					handles.push_back(subType->handle);
				}
				TypeHandle retType = handles.back();
				handles.pop_back();
				node.handle = m_pCtx->typeReg.Func(mod, handles, retType);
			}
			break;
		}
		default:
		{
			node.handle = m_pCtx->typeReg.Mod(mod, node.handle);
			break;
		}
		}
	}
}
