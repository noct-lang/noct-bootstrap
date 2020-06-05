#include "operator.hpp"


#include "tokens/token.hpp"
#include "common/context.hpp"
#include "common/qualname.hpp"
#include "symbol.hpp"

namespace Noctis
{
	bool IsPrefix(OperatorKind op)
	{
		return u8(op) <= u8(OperatorKind::BoolConv);
	}

	bool IsPostfix(OperatorKind op)
	{
		return u8(op) >= u8(OperatorKind::PostInc) &&
			u8(op) <= u8(OperatorKind::NullPanic);
	}

	bool IsBinary(OperatorKind op)
	{
		return u8(op) >= u8(OperatorKind::Add) &&
			u8(op) <= u8(OperatorKind::NotIn);
	}

	bool IsAssign(OperatorKind op)
	{
		return u8(op) >= u8(OperatorKind::Assign) &&
			u8(op) <= u8(OperatorKind::NullCoalesceAssign);
	}

	OperatorKind TokenTypeToOperator(TokenType type, bool unary, bool postfix)
	{
		switch (type)
		{
		case TokenType::PlusPlus: return postfix ? OperatorKind::PostInc : OperatorKind::PreInc;
		case TokenType::MinusMinus: return postfix ? OperatorKind::PostDec : OperatorKind::PreDec;
		case TokenType::ExclaimExclaim: return postfix ? OperatorKind::NullPanic : OperatorKind::BoolConv;
		case TokenType::Exclaim: return OperatorKind::Not;
		case TokenType::Plus: return unary ? OperatorKind::Pos : OperatorKind::Add;
		case TokenType::Minus: return unary ? OperatorKind::Neg : OperatorKind::Sub;
		case TokenType::Asterisk: return unary ? OperatorKind::Deref : OperatorKind::Mul;
		case TokenType::Slash: return OperatorKind::Div;
		case TokenType::Percent: return OperatorKind::Rem;
		case TokenType::Tilde: return unary ? OperatorKind::BinNeg : OperatorKind::Concat;
		case TokenType::OrOr: return OperatorKind::Or;
		case TokenType::AndAnd: return OperatorKind::And;
		case TokenType::LessLess: return OperatorKind::LShl;
		case TokenType::LessLessLess: return OperatorKind::AShl;
		case TokenType::LessLessAsterisk: return OperatorKind::Rotl;
		case TokenType::GreaterGreater: return OperatorKind::LShr;
		case TokenType::GreaterGreaterGreater: return OperatorKind::AShr;
		case TokenType::GreaterGreaterAsterisk: return OperatorKind::Rotr;
		case TokenType::Or: return OperatorKind::BinOr;
		case TokenType::Caret: return OperatorKind::BinXor;
		case TokenType::And: return unary ? OperatorKind::RefOrAddrOf : OperatorKind::BinAnd;
		case TokenType::DotDot: return OperatorKind::Range;
		case TokenType::DotDotEq: return OperatorKind::IncRange;
		case TokenType::QuestionQuestion: return OperatorKind::NullCoalesce;
		case TokenType::QuestionColon: return OperatorKind::Elvis;
		case TokenType::In: return OperatorKind::In;
		case TokenType::NotIn: return OperatorKind::NotIn;
		case TokenType::EqEq: return OperatorKind::Eq;
		case TokenType::ExclaimEq: return OperatorKind::Ne;
		case TokenType::Less: return OperatorKind::Lt;
		case TokenType::LessEq: return OperatorKind::Le;
		case TokenType::Greater: return OperatorKind::Gt;
		case TokenType::GreaterEq: return OperatorKind::Ge;
		case TokenType::Eq: return OperatorKind::Assign;
		case TokenType::PlusEq: return OperatorKind::AddAssign;
		case TokenType::MinusEq: return OperatorKind::SubAssign;
		case TokenType::AsteriskEq: return OperatorKind::MulAssign;
		case TokenType::SlashEq: return OperatorKind::DivAssign;
		case TokenType::PercentEq: return OperatorKind::RemAssign;
		case TokenType::TildeEq: return OperatorKind::ConcatAssign;
		case TokenType::LessLessEq: return OperatorKind::LShlAssign;
		case TokenType::LessLessLessEq: return OperatorKind::AShlAssign;
		case TokenType::LessLessAsteriskEq: return OperatorKind::RotlAssign;
		case TokenType::GreaterGreaterEq: return OperatorKind::LShrAssign;
		case TokenType::GreaterGreaterGreaterEq: return OperatorKind::AShrAssign;
		case TokenType::GreaterGreaterAsteriskEq: return OperatorKind::RotrAssign;
		case TokenType::OrEq: return OperatorKind::BinOrAssign;
		case TokenType::CaretEq: return OperatorKind::BinXorAssign;
		case TokenType::AndEq: return OperatorKind::BinAndAssign;
		case TokenType::QuestionQuestionEq: return OperatorKind::NullCoalesceAssign;
		default: return OperatorKind::Invalid;
		}
	}

	StdStringView GetOpName(OperatorKind op)
	{
		switch (op)
		{
		case OperatorKind::Pos: return "+";
		case OperatorKind::Neg: return "-";
		case OperatorKind::PreInc: return "X++";
		case OperatorKind::PreDec: return "X--";
		case OperatorKind::Not: return "!";
		case OperatorKind::BinNeg: return "~";
		case OperatorKind::Deref: return "*";
		case OperatorKind::RefOrAddrOf: return "&";
		case OperatorKind::BoolConv: return "!!";
		case OperatorKind::PostInc: return "++X";
		case OperatorKind::PostDec: return "--X";
		case OperatorKind::NullPanic: return "!!";
		case OperatorKind::Add: return "+";
		case OperatorKind::Sub: return "-";
		case OperatorKind::Mul: return "*";
		case OperatorKind::Div: return "/";
		case OperatorKind::Rem: return "%";
		case OperatorKind::Concat: return "~";
		case OperatorKind::Or: return "||";
		case OperatorKind::And: return "&&";
		case OperatorKind::LShl: return "<<";
		case OperatorKind::AShl: return "<<<";
		case OperatorKind::Rotl: return "<<*";
		case OperatorKind::LShr: return ">>";
		case OperatorKind::AShr: return ">>>";
		case OperatorKind::Rotr: return ">>*";
		case OperatorKind::BinOr: return "|";
		case OperatorKind::BinXor: return "^";
		case OperatorKind::BinAnd: return "&";
		case OperatorKind::Eq: return "==";
		case OperatorKind::Ne: return "!=";
		case OperatorKind::Lt: return "<";
		case OperatorKind::Le: return "<=";
		case OperatorKind::Gt: return ">";
		case OperatorKind::Ge: return ">=";
		case OperatorKind::Range: return "..";
		case OperatorKind::IncRange: return "..=";
		case OperatorKind::NullCoalesce: return "??";
		case OperatorKind::Elvis: return "?:";
		case OperatorKind::In: return "in";
		case OperatorKind::NotIn: return "!in";
		case OperatorKind::Assign: return "=";
		case OperatorKind::AddAssign: return "+=";
		case OperatorKind::SubAssign: return "-=";
		case OperatorKind::MulAssign: return "*=";
		case OperatorKind::DivAssign: return "/=";
		case OperatorKind::RemAssign: return "%=";
		case OperatorKind::ConcatAssign: return "~=";
		case OperatorKind::LShlAssign: return "<<=";
		case OperatorKind::AShlAssign: return "<<<=";
		case OperatorKind::RotlAssign: return "<<*=";
		case OperatorKind::LShrAssign: return ">>=";
		case OperatorKind::AShrAssign: return ">>>=";
		case OperatorKind::RotrAssign: return ">>*=";
		case OperatorKind::BinOrAssign: return "|=";
		case OperatorKind::BinXorAssign: return "^=";
		case OperatorKind::BinAndAssign: return "&=";
		case OperatorKind::NullCoalesceAssign: return "??=";
		default: return "";
		}
	}

	OperatorTable::OperatorTable(Context* pCtx)
		: m_pCtx(pCtx)
	{
	}

	void OperatorTable::Collect(ModuleSymbolTable& table)
	{
		
		for (u8 i = 0; i < u8(OperatorKind::NullPanic); ++i)
		{
			OperatorKind kind = OperatorKind(i);
			QualNameSPtr interfaceQualName = GetOpInterfaceQualName(kind);

			// Skip non-overloadable
			if (!interfaceQualName)
				continue;
			
			SymbolSPtr interfaceSym = table.Find(nullptr, interfaceQualName);
			if (interfaceSym)
			{
				for (StdPair<SymbolSPtr, bool>& pair : interfaceSym->impls)
				{
					HandleUnaryOp(kind, pair.first, interfaceSym);
				}

				for (SymbolSPtr variant : interfaceSym->variants)
				{
					for (StdPair<SymbolSPtr, bool>& pair : variant->impls)
					{
						HandleUnaryOp(kind, pair.first, variant);
					}
				}
			}
		}
		
		for (u8 i = u8(OperatorKind::Add); i <= u8(OperatorKind::BinAnd); ++i)
		{
			OperatorKind kind = OperatorKind(i);
			QualNameSPtr interfaceQualName = GetOpInterfaceQualName(kind);

			// Skip non-overloadable
			if (!interfaceQualName)
				continue;
			
			SymbolSPtr interfaceSym = table.Find(nullptr, interfaceQualName);
			if (interfaceSym)
			{
				for (StdPair<SymbolSPtr, bool>& pair : interfaceSym->impls)
				{
					HandleBinaryOp(kind, pair.first, interfaceSym);
				}

				for (SymbolSPtr variant : interfaceSym->variants)
				{
					for (StdPair<SymbolSPtr, bool>& pair : variant->impls)
					{
						HandleBinaryOp(kind, pair.first, variant);
					}
				}
			}
		}

		for (u8 i = u8(OperatorKind::AddAssign); i <= u8(OperatorKind::BinAndAssign); ++i)
		{
			OperatorKind kind = OperatorKind(i);
			QualNameSPtr interfaceQualName = GetOpInterfaceQualName(kind);

			// Skip non-overloadable
			if (!interfaceQualName)
				continue;

			SymbolSPtr interfaceSym = table.Find(nullptr, interfaceQualName);
			if (interfaceSym)
			{
				for (StdPair<SymbolSPtr, bool>& pair : interfaceSym->impls)
				{
					HandleBinaryOp(kind, pair.first, interfaceSym);
				}

				for (SymbolSPtr variant : interfaceSym->variants)
				{
					for (StdPair<SymbolSPtr, bool>& pair : variant->impls)
					{
						HandleBinaryOp(kind, pair.first, variant);
					}
				}
			}
		}

		// Casting
		{
			QualNameSPtr baseQualName = QualName::Create(StdVector<StdString>{ "core", "convert" });

			{
				QualNameSPtr interfaceQualName = QualName::Create(baseQualName, Iden::Create("From"));
				SymbolSPtr interfaceSym = table.Find(nullptr, interfaceQualName);

				if (interfaceSym)
				{
					for (StdPair<SymbolSPtr, bool>& pair : interfaceSym->impls)
					{
						HandleFromOp(false, pair.first, interfaceSym);
					}

					for (SymbolSPtr variant : interfaceSym->variants)
					{
						for (StdPair<SymbolSPtr, bool>& pair : variant->impls)
						{
							HandleFromOp(false, pair.first, variant);
						}
					}
				}
			}

			{
				QualNameSPtr interfaceQualName = QualName::Create(baseQualName, Iden::Create("TryFrom"));
				SymbolSPtr interfaceSym = table.Find(nullptr, interfaceQualName);

				if (interfaceSym)
				{
					for (StdPair<SymbolSPtr, bool>& pair : interfaceSym->impls)
					{
						HandleFromOp(true, pair.first, interfaceSym);
					}

					for (SymbolSPtr variant : interfaceSym->variants)
					{
						for (StdPair<SymbolSPtr, bool>& pair : variant->impls)
						{
							HandleFromOp(true, pair.first, variant);
						}
					}
				}
			}

			{
				QualNameSPtr interfaceQualName = QualName::Create(baseQualName, Iden::Create("To"));
				SymbolSPtr interfaceSym = table.Find(nullptr, interfaceQualName);

				if (interfaceSym)
				{
					for (StdPair<SymbolSPtr, bool>& pair : interfaceSym->impls)
					{
						HandleToOp(false, pair.first, interfaceSym);
					}

					for (SymbolSPtr variant : interfaceSym->variants)
					{
						for (StdPair<SymbolSPtr, bool>& pair : variant->impls)
						{
							HandleToOp(false, pair.first, variant);
						}
					}
				}
			}

			{
				QualNameSPtr interfaceQualName = QualName::Create(baseQualName, Iden::Create("TryTo"));
				SymbolSPtr interfaceSym = table.Find(nullptr, interfaceQualName);

				if (interfaceSym)
				{
					for (StdPair<SymbolSPtr, bool>& pair : interfaceSym->impls)
					{
						HandleToOp(true, pair.first, interfaceSym);
					}

					for (SymbolSPtr variant : interfaceSym->variants)
					{
						for (StdPair<SymbolSPtr, bool>& pair : variant->impls)
						{
							HandleToOp(true, pair.first, variant);
						}
					}
				}
			}
			
		}
	}

	Operator& OperatorTable::GetOperator(OperatorKind kind, TypeHandle expr)
	{
		static Operator empty;

		TypeRegistry& typeReg = m_pCtx->typeReg;
		TypeHandle searchHandle = expr;
		TypeSPtr exprType = typeReg.GetType(searchHandle);

		if (exprType->typeKind == TypeKind::Ref)
			searchHandle = exprType->AsRef().subType;

		if (kind == OperatorKind::PreInc ||
			kind == OperatorKind::PostInc ||
			kind == OperatorKind::PreDec ||
			kind == OperatorKind::PostDec)
		{
			TypeSPtr tmp = typeReg.GetType(searchHandle);
			if (tmp->mod != TypeMod::None)
				return empty;
		}
		
		searchHandle = typeReg.Mod(TypeMod::None, searchHandle);

		TypeSPtr searchType = typeReg.GetType(searchHandle);

		StdUnorderedMap<TypeSPtr, StdVector<Operator>>& ops = m_OpSymbols[u8(kind)];
		auto it = ops.find(searchType);
		if (it != ops.end())
			return it->second[0];
		return empty;
	}

	Operator& OperatorTable::GetOperator(OperatorKind kind, TypeHandle left, TypeHandle right)
	{
		static Operator empty;

		TypeRegistry& typeReg = m_pCtx->typeReg;
		TypeHandle searchHandle = left;
		TypeSPtr leftType = typeReg.GetType(searchHandle);

		if (leftType->typeKind == TypeKind::Ref)
			searchHandle = leftType->AsRef().subType;
		searchHandle = typeReg.Mod(TypeMod::None, searchHandle);

		TypeSPtr searchType = typeReg.GetType(searchHandle);

		TypeSPtr rightType = typeReg.GetType(right);
		if (rightType->typeKind == TypeKind::Ref)
			right = rightType->AsRef().subType;
		right = typeReg.Mod(TypeMod::None, right);

		StdUnorderedMap<TypeSPtr, StdVector<Operator>>& ops = m_OpSymbols[u8(kind)];
		auto it = ops.find(searchType);
		if (it != ops.end())
		{
			for (Operator& op : it->second)
			{
				if (typeReg.AreTypesEqual(op.right, right))
					return op;
			}
		}

		return empty;
	}

	void OperatorTable::HandleBinaryOp(OperatorKind kind, SymbolSPtr impl, SymbolSPtr interfaceSym)
	{
		TypeHandle rType = interfaceSym->qualName->Iden()->Generics()[0].type;

		StdString funcName = interfaceSym->qualName->Iden()->Name();
		funcName[0] = 'o';

		IdenGeneric idenGen;
		idenGen.isType = idenGen.isSpecialized = true;
		idenGen.type = impl->type;
		
		IdenSPtr funcIden = Iden::Create(funcName);

		IdenSPtr ifaceIden = Iden::Create(interfaceSym->qualName->Iden()->Name(), { idenGen }, m_pCtx->typeReg);
		QualNameSPtr ifaceQualName = QualName::Create(interfaceSym->qualName->Base(), ifaceIden);
		
		SymbolSPtr funcSym = impl->children->FindChild(ifaceQualName, funcIden, {});

		TypeSPtr funcType = m_pCtx->typeReg.GetType(funcSym->type);
		Operator op;
		op.left = impl->type;
		op.right = rType;
		op.result = funcType->AsFunc().retType;
		op.sym = funcSym;

		op.isBuiltin = m_pCtx->typeReg.IsType(op.left, TypeKind::Builtin) &&
			m_pCtx->typeReg.IsType(op.right, TypeKind::Builtin);

		StdUnorderedMap<TypeSPtr, StdVector<Operator>>& entry = m_OpSymbols[u8(kind)];
		TypeSPtr type = m_pCtx->typeReg.GetType(op.left);
		auto it = entry.find(type);
		if (it == entry.end())
			it = entry.try_emplace(type, StdVector<Operator>{}).first;

		bool found = false;
		for (Operator& tmpOp : it->second)
		{
			if (m_pCtx->typeReg.AreTypesEqual(tmpOp.right, op.right))
			{
				found = true;
				break;
			}
		}
		if (!found)
			it->second.push_back(op);
	}

	void OperatorTable::HandleUnaryOp(OperatorKind kind, SymbolSPtr impl, SymbolSPtr interfaceSym)
	{
		StdString funcName = interfaceSym->qualName->Iden()->Name();
		funcName[0] = 'o';
		IdenSPtr funcIden = Iden::Create(funcName);
		SymbolSPtr funcSym = impl->children->FindChild(interfaceSym->qualName, funcIden, {});

		TypeSPtr funcType = m_pCtx->typeReg.GetType(funcSym->type);
		Operator op;
		op.left = impl->type;
		op.result = funcType->AsFunc().retType;
		op.sym = funcSym;

		StdUnorderedMap<TypeSPtr, StdVector<Operator>>& entry = m_OpSymbols[u8(kind)];
		TypeSPtr type = m_pCtx->typeReg.GetType(op.left);
		auto it = entry.find(type);
		if (it == entry.end())
			it = entry.try_emplace(type, StdVector<Operator>{}).first;

		if (it->second.empty())
			it->second.push_back(op);
	}

	void OperatorTable::HandleFromOp(bool isTry, SymbolSPtr impl, SymbolSPtr interfaceSym)
	{
		TypeHandle fromType = interfaceSym->qualName->Iden()->Generics()[0].type;
		StdString funcName = interfaceSym->qualName->Iden()->Name();

		IdenGeneric idenGen;
		idenGen.isType = idenGen.isSpecialized = true;
		idenGen.type = impl->type;

		IdenSPtr funcIden = Iden::Create(funcName);

		IdenSPtr ifaceIden = Iden::Create(interfaceSym->qualName->Iden()->Name(), { idenGen }, m_pCtx->typeReg);
		QualNameSPtr ifaceQualName = QualName::Create(interfaceSym->qualName->Base(), ifaceIden);

		SymbolSPtr funcSym = impl->children->FindChild(ifaceQualName, funcIden, {});

		TypeSPtr funcType = m_pCtx->typeReg.GetType(funcSym->type);
		Operator op;
		op.left = fromType;
		op.right = impl->type;
		op.result = funcType->AsFunc().retType;
		op.sym = funcSym;

		op.isBuiltin = m_pCtx->typeReg.IsType(op.left, TypeKind::Builtin) &&
			m_pCtx->typeReg.IsType(op.right, TypeKind::Builtin);

		OperatorKind kind = isTry ? OperatorKind::TryCast : OperatorKind::Cast;
		StdUnorderedMap<TypeSPtr, StdVector<Operator>>& entry = m_OpSymbols[u8(kind)];
		TypeSPtr type = m_pCtx->typeReg.GetType(op.left);
		auto it = entry.find(type);
		if (it == entry.end())
			it = entry.try_emplace(type, StdVector<Operator>{}).first;

		bool found = false;
		for (Operator& tmpOp : it->second)
		{
			if (m_pCtx->typeReg.AreTypesEqual(tmpOp.right, op.right))
			{
				found = true;
				break;
			}
		}
		if (!found)
			it->second.push_back(op);
	}

	void OperatorTable::HandleToOp(bool isTry, SymbolSPtr impl, SymbolSPtr interfaceSym)
	{
		TypeHandle toType = interfaceSym->qualName->Iden()->Generics()[0].type;

		StdString funcName = interfaceSym->qualName->Iden()->Name();

		IdenGeneric idenGen;
		idenGen.isType = idenGen.isSpecialized = true;
		idenGen.type = impl->type;

		IdenSPtr funcIden = Iden::Create(funcName);

		IdenSPtr ifaceIden = Iden::Create(interfaceSym->qualName->Iden()->Name(), { idenGen }, m_pCtx->typeReg);
		QualNameSPtr ifaceQualName = QualName::Create(interfaceSym->qualName->Base(), ifaceIden);

		SymbolSPtr funcSym = impl->children->FindChild(ifaceQualName, funcIden, {});

		TypeSPtr funcType = m_pCtx->typeReg.GetType(funcSym->type);
		Operator op;
		op.left = impl->type;
		op.right = toType;
		op.result = funcType->AsFunc().retType;
		op.sym = funcSym;

		op.isBuiltin = m_pCtx->typeReg.IsType(op.left, TypeKind::Builtin) &&
			m_pCtx->typeReg.IsType(op.right, TypeKind::Builtin);

		OperatorKind kind = isTry ? OperatorKind::TryCast : OperatorKind::Cast;
		StdUnorderedMap<TypeSPtr, StdVector<Operator>>& entry = m_OpSymbols[u8(kind)];
		TypeSPtr type = m_pCtx->typeReg.GetType(op.left);
		auto it = entry.find(type);
		if (it == entry.end())
			it = entry.try_emplace(type, StdVector<Operator>{}).first;

		bool found = false;
		for (Operator& tmpOp : it->second)
		{
			if (m_pCtx->typeReg.AreTypesEqual(tmpOp.right, op.right))
			{
				found = true;
				break;
			}
		}
		if (!found)
			it->second.push_back(op);
	}

	QualNameSPtr OperatorTable::GetOpInterfaceQualName(OperatorKind kind)
	{
		switch (kind)
		{
		case OperatorKind::Pos:					return QualName::Create({ "core", "ops", "OpPos" });
		case OperatorKind::Neg:					return QualName::Create({ "core", "ops", "OpNeg" });
		case OperatorKind::PreInc:				return QualName::Create({ "core", "ops", "OpInc" });
		case OperatorKind::PreDec:				return QualName::Create({ "core", "ops", "OpDec" });
		case OperatorKind::Not:					return QualName::Create({ "core", "ops", "OpNot" });
		case OperatorKind::BinNeg:				return QualName::Create({ "core", "ops", "OpBinNeg" });
		case OperatorKind::Deref:				return QualName::Create({ "core", "ops", "OpDeref" });
		case OperatorKind::RefOrAddrOf:			return nullptr; // Not overloadable
		case OperatorKind::BoolConv:			return QualName::Create({ "core", "ops", "OpBoolConv" });
		case OperatorKind::PostInc:				return QualName::Create({ "core", "ops", "OpInc" });
		case OperatorKind::PostDec:				return QualName::Create({ "core", "ops", "OpDec" });
		case OperatorKind::NullPanic:			return QualName::Create({ "core", "ops", "OpNullPanic" });
		case OperatorKind::Add:					return QualName::Create({ "core", "ops", "OpAdd" });
		case OperatorKind::Sub:					return QualName::Create({ "core", "ops", "OpSub" });
		case OperatorKind::Mul:					return QualName::Create({ "core", "ops", "OpMul" });
		case OperatorKind::Div:					return QualName::Create({ "core", "ops", "OpDiv" });
		case OperatorKind::Rem:					return QualName::Create({ "core", "ops", "OpRem" });
		case OperatorKind::Concat:				return QualName::Create({ "core", "ops", "OpConcat" });
		case OperatorKind::Or:					return nullptr; // Not overloadable
		case OperatorKind::And:					return nullptr; // Not overloadable
		case OperatorKind::LShl:				return QualName::Create({ "core", "ops", "OpShl" });
		case OperatorKind::AShl:				return QualName::Create({ "core", "ops", "OpAShl" });
		case OperatorKind::Rotl:				return QualName::Create({ "core", "ops", "OpRotl" });
		case OperatorKind::LShr:				return QualName::Create({ "core", "ops", "OpShr" });
		case OperatorKind::AShr:				return QualName::Create({ "core", "ops", "OpAShr" });
		case OperatorKind::Rotr:				return QualName::Create({ "core", "ops", "OpRotr" });
		case OperatorKind::BinOr:				return QualName::Create({ "core", "ops", "OpBinOr" });
		case OperatorKind::BinXor:				return QualName::Create({ "core", "ops", "OpBinXor" });
		case OperatorKind::BinAnd:				return QualName::Create({ "core", "ops", "OpBinAnd" });
		case OperatorKind::Eq:					return QualName::Create({ "core", "ops", "OpPartialEq" });
		case OperatorKind::Ne:					return QualName::Create({ "core", "ops", "OpPartialEq" });
		case OperatorKind::Lt:					return QualName::Create({ "core", "ops", "OpPartialOrd" });
		case OperatorKind::Le:					return QualName::Create({ "core", "ops", "OpPartialOrd" });
		case OperatorKind::Gt:					return QualName::Create({ "core", "ops", "OpPartialOrd" });
		case OperatorKind::Ge:					return QualName::Create({ "core", "ops", "OpPartialOrd" });
		case OperatorKind::Range:				return QualName::Create({ "core", "ops", "OpRange" });
		case OperatorKind::IncRange:			return QualName::Create({ "core", "ops", "OpRange" });
		case OperatorKind::NullCoalesce:		return nullptr; // Not overloadable
		case OperatorKind::Elvis:				return nullptr; // Not overloadable
		case OperatorKind::In:					return QualName::Create({ "core", "ops", "OpContains" });
		case OperatorKind::NotIn:				return QualName::Create({ "core", "ops", "OpContains" });
		case OperatorKind::Assign:				return nullptr; // Not overloadable
		case OperatorKind::AddAssign:			return QualName::Create({ "core", "ops", "OpAddAssign" });
		case OperatorKind::SubAssign:			return QualName::Create({ "core", "ops", "OpSubAssign" });
		case OperatorKind::MulAssign:			return QualName::Create({ "core", "ops", "OpMulAssign" });
		case OperatorKind::DivAssign:			return QualName::Create({ "core", "ops", "OpDivAssign" });
		case OperatorKind::RemAssign:			return QualName::Create({ "core", "ops", "OpRemAssign" });
		case OperatorKind::ConcatAssign:		return QualName::Create({ "core", "ops", "OpConcatAssign" });
		case OperatorKind::LShlAssign:			return QualName::Create({ "core", "ops", "OpShlAssign" });
		case OperatorKind::AShlAssign:			return QualName::Create({ "core", "ops", "OpAShlAssign" });
		case OperatorKind::RotlAssign:			return QualName::Create({ "core", "ops", "OpRotlAssign" });
		case OperatorKind::LShrAssign:			return QualName::Create({ "core", "ops", "OpShrAssign" });
		case OperatorKind::AShrAssign:			return QualName::Create({ "core", "ops", "OpAShrAssign" });
		case OperatorKind::RotrAssign:			return QualName::Create({ "core", "ops", "OpRotrAssign" });
		case OperatorKind::BinOrAssign:			return QualName::Create({ "core", "ops", "OpBinOrAssign" });
		case OperatorKind::BinXorAssign:		return QualName::Create({ "core", "ops", "OpBinXorAssign" });
		case OperatorKind::BinAndAssign:		return QualName::Create({ "core", "ops", "OpBinAndAssign" });
		case OperatorKind::NullCoalesceAssign:	return nullptr; // Not overloadable
		case OperatorKind::Invalid:				return nullptr;
		default:								return nullptr;
		}
	}
}
