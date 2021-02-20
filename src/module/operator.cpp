#include "operator.hpp"


#include "tokens/token.hpp"
#include "common/context.hpp"
#include "common/qualname.hpp"
#include "itr/itr.hpp"
#include "module.hpp"
#include "symbol.hpp"

namespace Noctis
{
	bool IsPrefix(OperatorKind op)
	{
		return u8(op) <= u8(OperatorKind::RefOrAddrOf);
	}

	bool IsPostfix(OperatorKind op)
	{
		return u8(op) >= u8(OperatorKind::PostInc) &&
			u8(op) <= u8(OperatorKind::NullPanic);
	}

	bool IsBinary(OperatorKind op)
	{
		return u8(op) >= u8(OperatorKind::Add) &&
			u8(op) <= u8(OperatorKind::Ge);
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
		case TokenType::Exclaim: return postfix ? OperatorKind::NullPanic : OperatorKind::Not;
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
		case OperatorKind::PreInc: return "++X";
		case OperatorKind::PreDec: return "--X";
		case OperatorKind::Not: return "!";
		case OperatorKind::BinNeg: return "~";
		case OperatorKind::Deref: return "*";
		case OperatorKind::RefOrAddrOf: return "&";
		case OperatorKind::PostInc: return "X++";
		case OperatorKind::PostDec: return "X--";
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
				HandleUnaryOp(kind, nullptr, interfaceSym->baseInst);
				
				for (StdPair<SymbolWPtr, SymbolInstWPtr>& pair : interfaceSym->impls)
				{	
					HandleUnaryOp(kind, pair.first.lock(), pair.second.lock());
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
				HandleBinaryOp(kind, nullptr, interfaceSym->baseInst);

				for (StdPair<SymbolWPtr, SymbolInstWPtr>& pair : interfaceSym->impls)
				{
					HandleBinaryOp(kind, pair.first.lock(), pair.second.lock());
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
				HandleBinaryOp(kind, nullptr, interfaceSym->baseInst);

				for (StdPair<SymbolWPtr, SymbolInstWPtr>& pair : interfaceSym->impls)
				{
					HandleBinaryOp(kind, pair.first.lock(), pair.second.lock());
				}
			}
		}

		// Comparison
		{
			QualNameSPtr interfaceQualName = QualName::Create(StdVector<StdString>{ "core", "ops", "OpPartialEq" });
			SymbolSPtr interfaceSym = table.Find(nullptr, interfaceQualName);

			if (interfaceSym)
			{
				HandleBinaryOp(OperatorKind::Eq, nullptr, interfaceSym->baseInst);
				HandleBinaryOp(OperatorKind::Ne, nullptr, interfaceSym->baseInst);
				
				for (StdPair<SymbolWPtr, SymbolInstWPtr>& pair : interfaceSym->impls)
				{
					HandleBinaryOp(OperatorKind::Eq, pair.first.lock(), pair.second.lock());
					HandleBinaryOp(OperatorKind::Ne, pair.first.lock(), pair.second.lock());
				}
			}
		}

		{
			QualNameSPtr interfaceQualName = QualName::Create(StdVector<StdString>{ "core", "ops", "OpPartialOrd" });
			SymbolSPtr interfaceSym = table.Find(nullptr, interfaceQualName);

			if (interfaceSym)
			{
				HandleBinaryOp(OperatorKind::Lt, nullptr, interfaceSym->baseInst);
				HandleBinaryOp(OperatorKind::Le, nullptr, interfaceSym->baseInst);
				HandleBinaryOp(OperatorKind::Gt, nullptr, interfaceSym->baseInst);
				HandleBinaryOp(OperatorKind::Ge, nullptr, interfaceSym->baseInst);
				
				for (StdPair<SymbolWPtr, SymbolInstWPtr>& pair : interfaceSym->impls)
				{
					HandleBinaryOp(OperatorKind::Lt, pair.first.lock(), pair.second.lock());
					HandleBinaryOp(OperatorKind::Le, pair.first.lock(), pair.second.lock());
					HandleBinaryOp(OperatorKind::Gt, pair.first.lock(), pair.second.lock());
					HandleBinaryOp(OperatorKind::Ge, pair.first.lock(), pair.second.lock());
				}
			}
		}

		// Casting
		{
			QualNameSPtr baseQualName = QualName::Create(StdVector<StdString>{ "core", "convert" });

			{
				QualNameSPtr interfaceQualName = baseQualName->Append("From");
				SymbolSPtr interfaceSym = table.Find(nullptr, interfaceQualName);

				if (interfaceSym)
				{
					for (StdPair<SymbolWPtr, SymbolInstWPtr>& pair : interfaceSym->impls)
					{
						HandleConvOp(pair.first.lock(), pair.second.lock());
					}
				}
			}

			{
				QualNameSPtr interfaceQualName = baseQualName->Append("TryFrom");
				SymbolSPtr interfaceSym = table.Find(nullptr, interfaceQualName);

				if (interfaceSym)
				{
					for (StdPair<SymbolWPtr, SymbolInstWPtr>& pair : interfaceSym->impls)
					{
						HandleConvOp(pair.first.lock(), pair.second.lock());
					}
				}
			}

			{
				QualNameSPtr interfaceQualName = baseQualName->Append("To");
				SymbolSPtr interfaceSym = table.Find(nullptr, interfaceQualName);

				if (interfaceSym)
				{
					for (StdPair<SymbolWPtr, SymbolInstWPtr>& pair : interfaceSym->impls)
					{
						HandleConvOp(pair.first.lock(), pair.second.lock());
					}
				}
			}

			{
				QualNameSPtr interfaceQualName = baseQualName->Append("TryTo");
				SymbolSPtr interfaceSym = table.Find(nullptr, interfaceQualName);

				if (interfaceSym)
				{
					for (StdPair<SymbolWPtr, SymbolInstWPtr>& pair : interfaceSym->impls)
					{
						HandleConvOp(pair.first.lock(), pair.second.lock());
					}
				}
			}
			
		}

		// Index
		{
			QualNameSPtr baseQualName = QualName::Create(StdVector<StdString>{ "core", "ops" });
			
			{
				QualNameSPtr interfaceQualName = baseQualName->Append("OpIndex");
				SymbolSPtr interfaceSym = table.Find(nullptr, interfaceQualName);

				if (interfaceSym)
				{
					HandleBinaryOp(OperatorKind::Index, nullptr, interfaceSym->baseInst);

					for (StdPair<SymbolWPtr, SymbolInstWPtr>& pair : interfaceSym->impls)
					{
						HandleBinaryOp(OperatorKind::Index, pair.first.lock(), pair.second.lock());
					}
				}
			}

			{
				QualNameSPtr interfaceQualName = baseQualName->Append("OpMutIndex");
				SymbolSPtr interfaceSym = table.Find(nullptr, interfaceQualName);

				if (interfaceSym)
				{
					HandleBinaryOp(OperatorKind::MutIndex, nullptr, interfaceSym->baseInst);

					for (StdPair<SymbolWPtr, SymbolInstWPtr>& pair : interfaceSym->impls)
					{
						HandleBinaryOp(OperatorKind::MutIndex, pair.first.lock(), pair.second.lock());
					}
				}
			}
		}
	}

	Operator OperatorTable::GetOperator(OperatorKind kind, TypeHandle expr, BoundsInfo& boundsInfo)
	{
		TypeRegistry& typeReg = g_Ctx.typeReg;
		TypeHandle searchHandle = expr;
		TypeSPtr exprType = searchHandle.Type();

		if (exprType->typeKind == TypeKind::Ref)
			searchHandle = exprType->AsRef().subType;

		TypeSPtr tmp = searchHandle.Type();
		if (kind == OperatorKind::PreInc ||
			kind == OperatorKind::PostInc ||
			kind == OperatorKind::PreDec ||
			kind == OperatorKind::PostDec)
		{
			if (tmp->Mod() != TypeMod::Mut)
				return Operator{};
		}
		else
		{
			searchHandle = typeReg.Mod(TypeMod::None, searchHandle);
		}

		//StdVector<TypeHandle> = g_Ctx.typeReg.Get;

		StdUnorderedMap<TypeSPtr, StdVector<Operator>>& ops = m_OpSymbols[u8(kind)];
		auto it = ops.find(searchHandle.Type());
		if (it != ops.end())
			return it->second[0];

		// If no direct operator is found, look for any possible generic version
		StdVector<TypeSPtr> allCandidiates;
		for (StdPair<TypeSPtr, StdVector<Operator>> op : ops)
		{
			allCandidiates.push_back(op.first);
		}
		StdVector<TypeSPtr> possibleGenerics = g_Ctx.typeReg.GetBestVariants(searchHandle, allCandidiates, boundsInfo);

		if (possibleGenerics.size() == 1)
		{
			it = ops.find(possibleGenerics[0]);
			if (it != ops.end())
				return it->second[0];
		}

		// If there are no possibilities and type is generic, check if the associated interface bound is present
		if (possibleGenerics.empty() && searchHandle.Type()->typeKind == TypeKind::Generic)
		{
			QualNameSPtr interfaceQualName = GetOpInterfaceQualName(kind);

			TypeHandle interfaceType = g_Ctx.typeReg.Iden(TypeMod::None, interfaceQualName);
			bool found = false;

			const Bounds& bounds = boundsInfo.GetBounds(searchHandle);
			for (TypeHandle handle : bounds.bounds)
			{
				if (handle == interfaceType)
				{
					found = true;
					break;
				}
			}
			if (found)
			{
				auto it = ops.find(interfaceType.Type());
				if (it != ops.end())
				{
					Operator op = it->second[0];
					if (!op.result.IsValid() ||
						op.result.Kind() != TypeKind::Iden ||
						!op.isInterfaceOp)
						return op;

					SymbolSPtr resSym = op.result.AsIden().sym.lock();
					if (resSym->kind != SymbolKind::AssocType)
						return op;

					TypeDisambiguationSPtr disambig = TypeDisambiguation::Create(searchHandle, resSym->parent.lock()->qualName);
					QualNameSPtr qualName = QualName::Create(disambig);
					qualName = qualName->Append(resSym->qualName->LastIden());
					op.result = g_Ctx.typeReg.Iden(op.result.Mod(), qualName);
					
					return op;
				}
			}
		}
		
		// TODO: Error for multiple possibilities
		
		return Operator{};
	}

	Operator OperatorTable::GetOperator(OperatorKind kind, TypeHandle left, TypeHandle right, BoundsInfo& boundsInfo)
	{
		TypeRegistry& typeReg = g_Ctx.typeReg;
		TypeHandle searchHandle = left;
		TypeSPtr leftType = searchHandle.Type();

		if (leftType->typeKind == TypeKind::Ref)
			searchHandle = leftType->AsRef().subType;

		if (IsAssign(kind))
		{
			if (searchHandle.Mod() != TypeMod::Mut)
				return Operator{};
		}
		else
		{
			searchHandle = typeReg.Mod(TypeMod::None, searchHandle);
		}

		TypeSPtr rightType = right.Type();
		if (rightType->typeKind == TypeKind::Ref)
			right = rightType->AsRef().subType;
		right = typeReg.Mod(TypeMod::None, right);

		StdUnorderedMap<TypeSPtr, StdVector<Operator>>& ops = m_OpSymbols[u8(kind)];
		auto it = ops.find(searchHandle.Type());
		if (it != ops.end())
		{
			for (Operator& op : it->second)
			{
				if (op.right == right)
					return op;
			}
		}

		// If no direct operator is found, look for any possible generic version
		StdVector<TypeSPtr> allCandidiates;
		for (StdPair<TypeSPtr, StdVector<Operator>> op : ops)
		{
			allCandidiates.push_back(op.first);
		}
		StdVector<TypeSPtr> possibleGenerics = g_Ctx.typeReg.GetBestVariants(searchHandle, allCandidiates, boundsInfo);
		allCandidiates.clear();

		if (possibleGenerics.size() == 1)
		{
			it = ops.find(possibleGenerics[0]);
			if (it != ops.end())
			{
				Operator* bestOp = nullptr;
				i64 bestScore = -1;
				
				for (Operator& op : it->second)
				{
					i64 tmp = g_Ctx.typeReg.ScorePossibleVariant(searchHandle, op.right.Type(), boundsInfo);
					if (tmp == -1)
						continue;

					if (bestScore == -1)
					{
						bestOp = &op;
						bestScore = tmp;
					}
					else if (tmp < bestScore)
					{
						bestOp = &op;
						bestScore = tmp;
					}
					else if (tmp == bestScore)
					{
						// TODO: error
					}
				}

				if (bestOp)
					return *bestOp;
			}
		}

		// If there are no possibilities and type is generic, check if the associated interface bound is present
		if (possibleGenerics.empty() && searchHandle.Type()->typeKind == TypeKind::Generic)
		{
			QualNameSPtr interfaceQualName = GetOpInterfaceQualName(kind);

			TypeHandle interfaceType = g_Ctx.typeReg.Iden(TypeMod::None, interfaceQualName);
			bool found = false;

			const Bounds& bounds = boundsInfo.GetBounds(searchHandle);
			for (TypeHandle handle : bounds.bounds)
			{
				if (handle == interfaceType)
				{
					found = true;
					break;
				}
			}
			
			if (found)
			{
				auto it = ops.find(interfaceType.Type());
				if (it != ops.end())
				{
					Operator op;

					for (Operator& operator_ : it->second)
					{
						if (operator_.right.Type() == rightType)
						{
							op = operator_;
							break;
						}
					}
					
					if (!op.result.IsValid() ||
						op.result.Kind() != TypeKind::Iden ||
						!op.isInterfaceOp)
						return op;

					SymbolSPtr resSym = op.result.AsIden().sym.lock();
					if (resSym->kind != SymbolKind::AssocType)
						return op;

					TypeDisambiguationSPtr disambig = TypeDisambiguation::Create(searchHandle, resSym->parent.lock()->qualName);
					QualNameSPtr qualName = QualName::Create(disambig);
					qualName = qualName->Append(resSym->qualName->LastIden());
					op.result = g_Ctx.typeReg.Iden(op.result.Mod(), qualName);

					return op;
				}
			}
		}
		
		// TODO: Error for multiple possibilities

		return Operator{};
	}

	Operator OperatorTable::GetConstriantOperator(OperatorKind kind, TypeHandle expr, ITrGenDeclSPtr genDecl,
		BoundsInfo& boundsInfo)
	{
		TypeRegistry& typeReg = g_Ctx.typeReg;
		TypeHandle searchHandle = expr;
		TypeSPtr leftType = searchHandle.Type();

		if (leftType->typeKind == TypeKind::Ref)
			searchHandle = leftType->AsRef().subType;

		QualNameSPtr ifaceQualName = GetOpInterfaceQualName(kind);

		bool found = false;
		QualNameSPtr boundedQualName;
		if (searchHandle.Kind() != TypeKind::Generic)
		{
			const Bounds& bounds = boundsInfo.GetBounds(searchHandle);
			for (TypeHandle bound : bounds.bounds)
			{
				if (bound.Kind() != TypeKind::Iden)
					continue;

				boundedQualName = bound.AsIden().qualName;
				if (boundedQualName == ifaceQualName)
				{
					found = true;
					break;
				}
				boundedQualName = nullptr;
			}
		}
		else
		{
			const Bounds& bounds = boundsInfo.GetBounds(searchHandle);
			auto it = std::find_if(bounds.bounds.begin(), bounds.bounds.end(), [&ifaceQualName](const TypeHandle& handle)
			{
				return handle.Kind() == TypeKind::Iden && handle.AsIden().qualName == ifaceQualName;
			});

			if (it != bounds.bounds.end())
			{
				boundedQualName = it->AsIden().qualName;
				found = true;
			}
		}

		if (found)
		{
			TypeHandle retType;
			switch (kind)
			{
			case OperatorKind::RefOrAddrOf:
			{
				retType = typeReg.Ptr(TypeMod::None, expr);
				break;
			}

			case OperatorKind::Pos:
			case OperatorKind::Neg:
			case OperatorKind::PreInc:
			case OperatorKind::PreDec:
			case OperatorKind::Not:
			case OperatorKind::BinNeg:
			case OperatorKind::Deref:
			case OperatorKind::MutDeref:
			case OperatorKind::PostInc:
			case OperatorKind::PostDec:
			case OperatorKind::NullPanic:
			default:
			{
				TypeDisambiguationSPtr disambig = TypeDisambiguation::Create(searchHandle, boundedQualName);
				QualNameSPtr qualName = QualName::Create(disambig);
				qualName = qualName->Append("ResultT");
				
				retType = typeReg.Iden(TypeMod::None, qualName);
				break;
			}
			}

			Operator op;
			op.left = expr;
			op.result = retType;
			op.isInterfaceOp = true;

			SymbolSPtr ifaceSym = g_Ctx.activeModule->symTable.Find(boundedQualName);
			op.sym = ifaceSym->children->FindChild(nullptr, GetOpFuncIden(kind));

			return op;
		}

		return {};
	}

	Operator OperatorTable::GetConstriantOperator(OperatorKind kind, TypeHandle left, TypeHandle right,
	                                              ITrGenDeclSPtr genDecl, BoundsInfo& boundsInfo)
	{
		TypeRegistry& typeReg = g_Ctx.typeReg;
		TypeHandle searchHandle = left;
		TypeSPtr leftType = searchHandle.Type();

		if (leftType->typeKind == TypeKind::Ref)
			searchHandle = leftType->AsRef().subType;

		if (right.Kind() == TypeKind::Ref)
			right = right.AsRef().subType;
		
		QualNameSPtr ifaceQualName = GetOpInterfaceQualName(kind);
		IdenGeneric ifaceIdenGen;
		ifaceIdenGen.isSpecialized = ifaceIdenGen.isType = true;
		ifaceIdenGen.type = right;
		ifaceQualName = ifaceQualName->Base()->Append(ifaceQualName->LastIden(), { ifaceIdenGen });

		bool found = false;
		QualNameSPtr boundQualName;
		if (searchHandle.Kind() != TypeKind::Generic)
		{
			const Bounds& bounds = boundsInfo.GetBounds(searchHandle);
			for (TypeHandle bound : bounds.bounds)
			{
				if (bound.Kind() != TypeKind::Iden)
					continue;

				boundQualName = bound.AsIden().qualName;
				if (boundQualName == ifaceQualName)
				{
					found = true;
					break;
				}
				boundQualName = nullptr;
			}
		}
		else
		{
			const Bounds& bounds = boundsInfo.GetBounds(searchHandle);
			auto it = std::find_if(bounds.bounds.begin(), bounds.bounds.end(), [&ifaceQualName](const TypeHandle& handle)
			{
				return handle.Kind() == TypeKind::Iden && handle.AsIden().qualName == ifaceQualName;
			});

			if (it != bounds.bounds.end())
			{
				boundQualName = it->AsIden().qualName;
				found = true;
			}
		}

		if (found)
		{
			TypeHandle retType;
			switch (kind)
			{
			case OperatorKind::In:
			case OperatorKind::NotIn:
			case OperatorKind::Eq:
			case OperatorKind::Ne:
			case OperatorKind::Lt:
			case OperatorKind::Le:
			case OperatorKind::Gt:
			case OperatorKind::Ge:
			{
				retType = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::Bool);
				break;
			}
			case OperatorKind::NullCoalesce:
			{
				retType = right;
				break;
			}
			case OperatorKind::Elvis:
			case OperatorKind::Assign:
			case OperatorKind::AddAssign:
			case OperatorKind::SubAssign:
			case OperatorKind::MulAssign:
			case OperatorKind::DivAssign:
			case OperatorKind::RemAssign:
			case OperatorKind::ConcatAssign:
			case OperatorKind::LShlAssign:
			case OperatorKind::AShlAssign:
			case OperatorKind::RotlAssign:
			case OperatorKind::LShrAssign:
			case OperatorKind::AShrAssign:
			case OperatorKind::RotrAssign:
			case OperatorKind::BinOrAssign:
			case OperatorKind::BinXorAssign:
			case OperatorKind::BinAndAssign:
			case OperatorKind::NullCoalesceAssign:
			{
				retType = left;
				break;
			}
			default:
			{
				retType = typeReg.Iden(TypeMod::None, boundQualName->Append("ResultT"));
				break;
			}
			}

			Operator op;
			op.left = left;
			op.right = right;
			op.result = retType;
			op.isInterfaceOp = true;

			SymbolSPtr ifaceSym = g_Ctx.activeModule->symTable.Find(boundQualName);
			op.sym = ifaceSym->children->FindChild(nullptr, GetOpFuncIden(kind));
			
			return op;
		}
		
		return {};
	}

	void OperatorTable::HandleBinaryOp(OperatorKind kind, SymbolSPtr impl, SymbolInstSPtr ifaceInst)
	{
		TypeHandle rType = ifaceInst->qualName->Generics()[0].type;

		IdenGeneric idenGen;
		idenGen.isType = idenGen.isSpecialized = true;
		idenGen.type = rType;
		
		const StdString& funcIden = GetOpFuncIden(kind);
		QualNameSPtr ifaceQualName = ifaceInst->qualName->Base()->Append(ifaceInst->qualName->LastIden(), { idenGen });
		
		SymbolSPtr funcSym;
		if (impl)
			funcSym = impl->children->FindChild(ifaceQualName, funcIden);
		else
			funcSym = ifaceInst->sym.lock()->children->FindChild(nullptr, funcIden);

		Operator op;
		op.left = impl ? impl->type : ifaceInst->type;
		if (IsAssign(kind))
			op.left = g_Ctx.typeReg.Mod(TypeMod::Mut, op.left);
		
		op.right = rType;
		op.result = funcSym->type.AsFunc().retType;
		if (op.result.Kind() == TypeKind::Iden)
		{
			SymbolSPtr sym = op.result.AsIden().sym.lock();
			if (sym &&
				sym->kind == SymbolKind::Typealias)
				op.result = op.result.AsIden().sym.lock()->type;
		}
		
		op.sym = funcSym;

		op.isBuiltin = IsBuiltinOp(op.left, op.right, impl ? impl->boundsInfo : ifaceInst->sym.lock()->boundsInfo);
		op.isInterfaceOp = !impl;

		StdUnorderedMap<TypeSPtr, StdVector<Operator>>& entry = m_OpSymbols[u8(kind)];
		TypeSPtr type = op.left.Type();
		auto it = entry.find(type);
		if (it == entry.end())
			it = entry.try_emplace(type, StdVector<Operator>{}).first;

		bool found = false;
		for (Operator& tmpOp : it->second)
		{
			if (tmpOp.right == op.right)
			{
				found = true;
				break;
			}
		}
		if (!found)
			it->second.push_back(op);
	}

	void OperatorTable::HandleUnaryOp(OperatorKind kind, SymbolSPtr impl, SymbolInstSPtr ifaceInst)
	{
		const StdString& funcIden = GetOpFuncIden(kind);
		SymbolSPtr funcSym;
		if (impl)
			funcSym = impl->children->FindChild(ifaceInst->qualName, funcIden);
		else
			funcSym = ifaceInst->sym.lock()->children->FindChild(nullptr, funcIden);

 		TypeSPtr funcType = funcSym->type.Type();
		Operator op;

		op.left = impl ? impl->type : ifaceInst->type;
		if (kind == OperatorKind::PreInc ||
			kind == OperatorKind::PostInc ||
			kind == OperatorKind::PreDec ||
			kind == OperatorKind::PostDec)
		{
			op.left = g_Ctx.typeReg.Mod(TypeMod::Mut, op.left);
		}
		
		op.result = funcType->AsFunc().retType;
		if (op.result.Kind() == TypeKind::Iden)
		{
			SymbolSPtr sym = op.result.AsIden().sym.lock();
			if (sym &&
				sym->kind == SymbolKind::Typealias)
				op.result = op.result.AsIden().sym.lock()->type;
		}
		
		op.sym = funcSym;

		op.isBuiltin = IsBuiltinOp(op.left, impl ? impl->boundsInfo : ifaceInst->sym.lock()->boundsInfo);
		op.isInterfaceOp = !impl;

		if ((kind == OperatorKind::Deref || kind == OperatorKind::MutDeref) && g_Ctx.typeReg.IsType(op.left, TypeKind::Ptr))
			op.isBuiltin = true;

		StdUnorderedMap<TypeSPtr, StdVector<Operator>>& entry = m_OpSymbols[u8(kind)];
		TypeSPtr type = op.left.Type();
		auto it = entry.find(type);
		if (it == entry.end())
			it = entry.try_emplace(type, StdVector<Operator>{}).first;

		if (it->second.empty())
		{
			it->second.push_back(op);
		}
		else
		{
			// TODO: Error
		}
	}

	void OperatorTable::HandleConvOp(SymbolSPtr impl, SymbolInstSPtr ifaceInst)
	{	
		TypeHandle genType = ifaceInst->qualName->Generics()[0].type;
		StdString funcName = ifaceInst->qualName->LastIden();

		bool isTry = funcName.find("Try") != StdString::npos;
		bool isTo = funcName.find("To") != StdString::npos;

		IdenGeneric idenGen;
		idenGen.isType = idenGen.isSpecialized = true;
		idenGen.type = isTo ? impl->type : genType;

		QualNameSPtr ifaceQualName = ifaceInst->qualName->Base()->Append(ifaceInst->qualName->LastIden(), { idenGen });
		SymbolSPtr funcSym = impl->children->FindChild(ifaceQualName, funcName);

		TypeSPtr funcType = funcSym->type.Type();
		Operator op;
		op.left = isTo ? impl->type : genType;
		op.right = isTo ? genType : impl->type;
		op.result = funcType->AsFunc().retType;
		if (op.result.Kind() == TypeKind::Iden)
		{
			SymbolSPtr sym = op.result.AsIden().sym.lock();
			if (sym && sym->kind == SymbolKind::Typealias)
				op.result = op.result.AsIden().sym.lock()->type;
		}
		
		op.sym = funcSym;

		op.isBuiltin = IsBuiltinOp(op.left, op.right, impl ? impl->boundsInfo : ifaceInst->sym.lock()->boundsInfo);

		OperatorKind kind = isTry ? OperatorKind::TryCast : OperatorKind::Cast;
		StdUnorderedMap<TypeSPtr, StdVector<Operator>>& entry = m_OpSymbols[u8(kind)];
		TypeSPtr type = op.left.Type();
		auto it = entry.find(type);
		if (it == entry.end())
			it = entry.try_emplace(type, StdVector<Operator>{}).first;

		bool found = false;
		for (Operator& tmpOp : it->second)
		{
			if (tmpOp.right == op.right)
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
		static StdVector<QualNameSPtr> ifaces;
		if (ifaces.empty())
		{
			for (u8 i = 0; i < u8(OperatorKind::Count); ++i)
			{
				QualNameSPtr qualName;
				OperatorKind opKind = OperatorKind(i);
				switch (opKind)
				{
				case OperatorKind::Pos:					qualName = QualName::Create({ "core", "ops", "OpPos" }); break;
				case OperatorKind::Neg:					qualName = QualName::Create({ "core", "ops", "OpNeg" }); break;
				case OperatorKind::PreInc:				qualName = QualName::Create({ "core", "ops", "OpInc" }); break;
				case OperatorKind::PreDec:				qualName = QualName::Create({ "core", "ops", "OpDec" }); break;
				case OperatorKind::Not:					qualName = QualName::Create({ "core", "ops", "OpNot" }); break;
				case OperatorKind::BinNeg:				qualName = QualName::Create({ "core", "ops", "OpBinNeg" }); break;
				case OperatorKind::Deref:				qualName = QualName::Create({ "core", "ops", "OpDeref" }); break;
				case OperatorKind::MutDeref:			qualName = QualName::Create({ "core", "ops", "OpMutDeref" }); break;
				case OperatorKind::RefOrAddrOf:			break; // Not overloadable
				case OperatorKind::PostInc:				qualName = QualName::Create({ "core", "ops", "OpInc" }); break;
				case OperatorKind::PostDec:				qualName = QualName::Create({ "core", "ops", "OpDec" }); break;
				case OperatorKind::NullPanic:			qualName = QualName::Create({ "core", "ops", "OpNullPanic" }); break;
				case OperatorKind::Add:					qualName = QualName::Create({ "core", "ops", "OpAdd" }); break;
				case OperatorKind::Sub:					qualName = QualName::Create({ "core", "ops", "OpSub" }); break;
				case OperatorKind::Mul:					qualName = QualName::Create({ "core", "ops", "OpMul" }); break;
				case OperatorKind::Div:					qualName = QualName::Create({ "core", "ops", "OpDiv" }); break;
				case OperatorKind::Rem:					qualName = QualName::Create({ "core", "ops", "OpRem" }); break;
				case OperatorKind::Concat:				qualName = QualName::Create({ "core", "ops", "OpConcat" }); break;
				case OperatorKind::Or:					break; // Not overloadable
				case OperatorKind::And:					break; // Not overloadable
				case OperatorKind::LShl:				qualName = QualName::Create({ "core", "ops", "OpShl" }); break;
				case OperatorKind::AShl:				qualName = QualName::Create({ "core", "ops", "OpAShl" }); break;
				case OperatorKind::Rotl:				qualName = QualName::Create({ "core", "ops", "OpRotl" }); break;
				case OperatorKind::LShr:				qualName = QualName::Create({ "core", "ops", "OpShr" }); break;
				case OperatorKind::AShr:				qualName = QualName::Create({ "core", "ops", "OpAShr" }); break;
				case OperatorKind::Rotr:				qualName = QualName::Create({ "core", "ops", "OpRotr" }); break;
				case OperatorKind::BinOr:				qualName = QualName::Create({ "core", "ops", "OpBinOr" }); break;
				case OperatorKind::BinXor:				qualName = QualName::Create({ "core", "ops", "OpBinXor" }); break;
				case OperatorKind::BinAnd:				qualName = QualName::Create({ "core", "ops", "OpBinAnd" }); break;
				case OperatorKind::Eq:					qualName = QualName::Create({ "core", "ops", "OpPartialEq" }); break;
				case OperatorKind::Ne:					qualName = QualName::Create({ "core", "ops", "OpPartialEq" }); break;
				case OperatorKind::Lt:					qualName = QualName::Create({ "core", "ops", "OpPartialOrd" }); break;
				case OperatorKind::Le:					qualName = QualName::Create({ "core", "ops", "OpPartialOrd" }); break;
				case OperatorKind::Gt:					qualName = QualName::Create({ "core", "ops", "OpPartialOrd" }); break;
				case OperatorKind::Ge:					qualName = QualName::Create({ "core", "ops", "OpPartialOrd" }); break;
				case OperatorKind::Range:				qualName = QualName::Create({ "core", "ops", "OpRange" }); break;
				case OperatorKind::IncRange:			qualName = QualName::Create({ "core", "ops", "OpRange" }); break;
				case OperatorKind::NullCoalesce:		break; // Not overloadable
				case OperatorKind::Elvis:				break; // Not overloadable
				case OperatorKind::In:					qualName = QualName::Create({ "core", "ops", "OpContains" }); break;
				case OperatorKind::NotIn:				qualName = QualName::Create({ "core", "ops", "OpContains" }); break;
				case OperatorKind::Assign:				break; // Not overloadable
				case OperatorKind::AddAssign:			qualName = QualName::Create({ "core", "ops", "OpAddAssign" }); break;
				case OperatorKind::SubAssign:			qualName = QualName::Create({ "core", "ops", "OpSubAssign" }); break;
				case OperatorKind::MulAssign:			qualName = QualName::Create({ "core", "ops", "OpMulAssign" }); break;
				case OperatorKind::DivAssign:			qualName = QualName::Create({ "core", "ops", "OpDivAssign" }); break;
				case OperatorKind::RemAssign:			qualName = QualName::Create({ "core", "ops", "OpRemAssign" }); break;
				case OperatorKind::ConcatAssign:		qualName = QualName::Create({ "core", "ops", "OpConcatAssign" }); break;
				case OperatorKind::LShlAssign:			qualName = QualName::Create({ "core", "ops", "OpShlAssign" }); break;
				case OperatorKind::AShlAssign:			qualName = QualName::Create({ "core", "ops", "OpAShlAssign" }); break;
				case OperatorKind::RotlAssign:			qualName = QualName::Create({ "core", "ops", "OpRotlAssign" }); break;
				case OperatorKind::LShrAssign:			qualName = QualName::Create({ "core", "ops", "OpShrAssign" }); break;
				case OperatorKind::AShrAssign:			qualName = QualName::Create({ "core", "ops", "OpAShrAssign" }); break;
				case OperatorKind::RotrAssign:			qualName = QualName::Create({ "core", "ops", "OpRotrAssign" }); break;
				case OperatorKind::BinOrAssign:			qualName = QualName::Create({ "core", "ops", "OpBinOrAssign" }); break;
				case OperatorKind::BinXorAssign:		qualName = QualName::Create({ "core", "ops", "OpBinXorAssign" }); break;
				case OperatorKind::BinAndAssign:		qualName = QualName::Create({ "core", "ops", "OpBinAndAssign" }); break;
				case OperatorKind::NullCoalesceAssign:	break; // Not overloadable
				case OperatorKind::Invalid:				break;
				default:								break;
				}

				if (qualName && (IsBinary(opKind) || IsAssign(opKind)))
				{
					IdenGeneric idenGen;
					idenGen.isType = true;
					idenGen.iden = "__T";
					qualName->Generics().push_back(idenGen);
				}

				ifaces.push_back(qualName);
			}
		}

		return ifaces[u8(kind)];
	}

	const StdString& OperatorTable::GetOpFuncIden(OperatorKind kind)
	{
		static StdVector<StdString> funcIdens;
		if (funcIdens.empty())
		{
			for (u8 i = 0; i < u8(OperatorKind::Count); ++i)
			{
				StdString funcIden;
				switch (OperatorKind(i))
				{
				case OperatorKind::Pos:					funcIden = "opPos"; break;
				case OperatorKind::Neg:					funcIden = "opNeg"; break;
				case OperatorKind::PreInc:				funcIden = "opInc"; break;
				case OperatorKind::PreDec:				funcIden = "opDec"; break;
				case OperatorKind::Not:					funcIden = "opNot"; break;
				case OperatorKind::BinNeg:				funcIden = "opBinNeg"; break;
				case OperatorKind::Deref:				funcIden = "opDeref"; break;
				case OperatorKind::MutDeref:			funcIden = "opMutDeref"; break;
				case OperatorKind::RefOrAddrOf:			break; // Not overloadable
				case OperatorKind::PostInc:				funcIden = "opInc"; break;
				case OperatorKind::PostDec:				funcIden = "opDec"; break;
				case OperatorKind::NullPanic:			funcIden = "opNullPanic"; break;
				case OperatorKind::Add:					funcIden = "opAdd"; break;
				case OperatorKind::Sub:					funcIden = "opSub"; break;
				case OperatorKind::Mul:					funcIden = "opMul"; break;
				case OperatorKind::Div:					funcIden = "opDiv"; break;
				case OperatorKind::Rem:					funcIden = "opRem"; break;
				case OperatorKind::Concat:				funcIden = "opConcat"; break;
				case OperatorKind::Or:					break; // Not overloadable
				case OperatorKind::And:					break; // Not overloadable
				case OperatorKind::LShl:				funcIden = "opShl"; break;
				case OperatorKind::AShl:				funcIden = "opAShl"; break;
				case OperatorKind::Rotl:				funcIden = "opRotl"; break;
				case OperatorKind::LShr:				funcIden = "opShr"; break;
				case OperatorKind::AShr:				funcIden = "opAShr"; break;
				case OperatorKind::Rotr:				funcIden = "opRotr"; break;
				case OperatorKind::BinOr:				funcIden = "opBinOr"; break;
				case OperatorKind::BinXor:				funcIden = "opBinXor"; break;
				case OperatorKind::BinAnd:				funcIden = "opBinAnd"; break;
				case OperatorKind::Eq:					funcIden = "opEq"; break;
				case OperatorKind::Ne:					funcIden = "opNe"; break;
				case OperatorKind::Lt:					funcIden = "opLt"; break;
				case OperatorKind::Le:					funcIden = "opLe"; break;
				case OperatorKind::Gt:					funcIden = "opGt"; break;
				case OperatorKind::Ge:					funcIden = "opGe"; break;
				case OperatorKind::Range:				funcIden = "opRange"; break;
				case OperatorKind::IncRange:			funcIden = "opRange"; break;
				case OperatorKind::NullCoalesce:		break; // Not overloadable
				case OperatorKind::Elvis:				break; // Not overloadable
				case OperatorKind::In:					funcIden = "opContains"; break;
				case OperatorKind::NotIn:				funcIden = "opNotContains"; break;
				case OperatorKind::Assign:				break; // Not overloadable
				case OperatorKind::AddAssign:			funcIden = "opAddAssign"; break;
				case OperatorKind::SubAssign:			funcIden = "opSubAssign"; break;
				case OperatorKind::MulAssign:			funcIden = "opMulAssign"; break;
				case OperatorKind::DivAssign:			funcIden = "opDivAssign"; break;
				case OperatorKind::RemAssign:			funcIden = "opRemAssign"; break;
				case OperatorKind::ConcatAssign:		funcIden = "opConcatAssign"; break;
				case OperatorKind::LShlAssign:			funcIden = "opShlAssign"; break;
				case OperatorKind::AShlAssign:			funcIden = "opAShlAssign"; break;
				case OperatorKind::RotlAssign:			funcIden = "opRotlAssign"; break;
				case OperatorKind::LShrAssign:			funcIden = "opShrAssign"; break;
				case OperatorKind::AShrAssign:			funcIden = "opAShrAssign"; break;
				case OperatorKind::RotrAssign:			funcIden = "opRotrAssign"; break;
				case OperatorKind::BinOrAssign:			funcIden = "opBinOrAssign"; break;
				case OperatorKind::BinXorAssign:		funcIden = "opBinXorAssign"; break;
				case OperatorKind::BinAndAssign:		funcIden = "opBinAndAssign"; break;
				case OperatorKind::NullCoalesceAssign:	break; // Not overloadable
				case OperatorKind::Index:               funcIden = "opIndex"; break;
				case OperatorKind::MutIndex:            funcIden = "opMutIndex"; break;
				case OperatorKind::Cast:                break;
				case OperatorKind::Invalid:             break;
				case OperatorKind::TryCast:             break;
				default:;
				}
				funcIdens.push_back(funcIden);
			}
		}

		return funcIdens[u8(kind)];
	}

	bool OperatorTable::IsBuiltinOp(TypeHandle handle, BoundsInfo& boundsInfo)
	{
		TypeSPtr type = handle.Type();
		QualNameSPtr valEnumMarkerQualName = QualName::Create({ "core", "marker", "ValEnum" });
		switch (type->typeKind)
		{
		case TypeKind::Builtin:
			return true;
		case TypeKind::Iden:
		{
			SymbolSPtr sym = type->AsIden().sym.lock();
			return sym->HasMarker(valEnumMarkerQualName);
		}
		case TypeKind::Generic:
		{
			const Bounds& bounds = boundsInfo.GetBounds(type);
			for (const TypeHandle& constraint : bounds.bounds)
			{
				if (constraint.Kind() == TypeKind::Iden &&
					constraint.AsIden().qualName == valEnumMarkerQualName)
					return true;
			}
		}
		default:
			return false;
		}
	}

	bool OperatorTable::IsBuiltinOp(TypeHandle left, TypeHandle right, BoundsInfo& boundsInfo)
	{
		return IsBuiltinOp(left, boundsInfo) && IsBuiltinOp(right, boundsInfo);
	}
}
