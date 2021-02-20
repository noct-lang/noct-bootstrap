#include "misc-passes.hpp"



#include "ast/ast.hpp"
#include "common/context.hpp"
#include "common/errorsystem.hpp"
#include "common/name-mangling.hpp"
#include "common/qualname.hpp"
#include "itr/itr.hpp"
#include "module/function.hpp"
#include "module/symbol.hpp"

namespace Noctis
{
	NameManglePass::NameManglePass()
		: ITrSemanticPass("name mangle pass")
	{
	}

	void NameManglePass::Process(ITrModule& mod)
	{
		SetModule(mod);

		Foreach(ITrVisitorDefKind::Any, [this](ITrFunc& node)
		{
			SymbolSPtr sym = node.sym.lock();
			sym->mangledName = NameMangling::Mangle(sym);
		});
	}

	MarkingPass::MarkingPass()
		: ITrSemanticPass("marking pass")
		, m_CompileTimeOnly(false)
		, m_DependsOnValueGenerics(false)
	{
	}

	void MarkingPass::Process(ITrModule& mod)
	{
		SetModule(mod);
		
		Foreach(ITrVisitorDefKind::Module, [&](ITrFunc& node)
		{
			SymbolSPtr sym = node.sym.lock();
			Mark(node, node.attribs ? node.attribs->attribs : Attribute::None);
			ITrBodySPtr body = mod.GetBody(node);
			ITrVisitor::Visit(body);
		});

		Foreach(ITrVisitorDefKind::Any, [&](ITrImpl& node)
		{
			SymbolSPtr sym = node.sym.lock();
			Mark(node, node.attribs ? node.attribs->attribs : Attribute::None);
			ITrBodySPtr body = mod.GetBody(node);
			ITrVisitor::Visit(body);
		});
	}

	void MarkingPass::Visit(ITrStruct& node)
	{
		SymbolSPtr sym = node.sym.lock();
		Mark(node, node.attribs ? node.attribs->attribs : Attribute::None);
		ITrBodySPtr body = m_pMod->GetBody(node);
		ITrVisitor::Visit(body);
	}

	void MarkingPass::Visit(ITrUnion& node)
	{
		SymbolSPtr sym = node.sym.lock();
		Mark(node, node.attribs ? node.attribs->attribs : Attribute::None);
		ITrBodySPtr body = m_pMod->GetBody(node);
		ITrVisitor::Visit(body);
	}

	void MarkingPass::Visit(ITrValEnum& node)
	{
		SymbolSPtr sym = node.sym.lock();
		Mark(node, node.attribs ? node.attribs->attribs : Attribute::None);
		ITrBodySPtr body = m_pMod->GetBody(node);
		ITrVisitor::Visit(body);
	}

	void MarkingPass::Visit(ITrAdtEnum& node)
	{
		SymbolSPtr sym = node.sym.lock();
		Mark(node, node.attribs ? node.attribs->attribs : Attribute::None);
		ITrBodySPtr body = m_pMod->GetBody(node);
		ITrVisitor::Visit(body);
	}

	void MarkingPass::Visit(ITrVar& node)
	{
		SymbolSPtr sym = node.sym.lock();
		Mark(node, node.attribs ? node.attribs->attribs : Attribute::None);
		ITrBodySPtr body = m_pMod->GetBody(node);
		ITrVisitor::Visit(body);
	}

	void MarkingPass::Visit(ITrFunc& node)
	{
		SymbolSPtr sym = node.sym.lock();
		Mark(node, node.attribs ? node.attribs->attribs : Attribute::None);
		ITrBodySPtr body = m_pMod->GetBody(node);
		ITrVisitor::Visit(body);
	}

	void MarkingPass::Mark(ITrDef& def, Attribute attribs)
	{
		SymbolSPtr sym = def.sym.lock();
		
		sym->comptimeOnly = m_CompileTimeOnly;
		if (!m_CompileTimeOnly && ENUM_IS_SET(attribs, Attribute::Comptime))
			sym->comptimeOnly = m_CompileTimeOnly = true;

		sym->dependsOnValueGenerics = m_DependsOnValueGenerics;
		if (!m_DependsOnValueGenerics)
		{
			if (sym->qualName)
			{
				for (IdenGeneric& idenGen : sym->qualName->Generics())
				{
					if (!idenGen.isType)
					{
						sym->dependsOnValueGenerics = m_DependsOnValueGenerics = true;
						break;
					}
				}
			}
			else if (def.genDecl)
			{
				for (ITrGenParamSPtr param : def.genDecl->params)
				{
					if (!param->isType)
					{
						sym->dependsOnValueGenerics = m_DependsOnValueGenerics = true;
						break;
					}
				}
			}
		}
	}

	SwitchProcessPass::SwitchProcessPass()
		: ITrSemanticPass("switch process pass")
	{
	}

	void SwitchProcessPass::Process(ITrModule& mod)
	{
		Foreach(ITrVisitorDefKind::Any, [&, this](ITrFunc& node)
		{
			ITrBodySPtr body = m_pMod->GetBody(node);
			if (body)
				ITrVisitor::Visit(body);
		});
	}

	void SwitchProcessPass::Visit(ITrSwitch& node)
	{
		for (usize i = 0; i < node.cases.size(); ++i)
		{
			ITrSwitchCase& case_ = node.cases[i];

			StdVector<ITrPatternSPtr> patterns = SplitEither(case_.pattern);
			if (patterns.empty())
			{
				ITrSwitchGroup group = CreateGroup(case_.pattern, node.expr->handle, i);
				MergeGroups(node.baseGroup, group);
			}
			else
			{
				for (ITrPatternSPtr pattern : patterns)
				{
					ITrSwitchGroup group = CreateGroup(case_.pattern, node.expr->handle, i);
					MergeGroups(node.baseGroup, group);
				}	
			}
		}
	}

	StdVector<ITrPatternSPtr> SwitchProcessPass::SplitEither(ITrPatternSPtr pattern)
	{
		switch (pattern->patternKind)
		{
		case ITrPatternKind::ValueBind:
		{
			ITrValueBindPattern& valueBind = static_cast<ITrValueBindPattern&>(*pattern);
			if (!valueBind.subPattern)
				return {};

			StdVector<ITrPatternSPtr> subPatterns = SplitEither(valueBind.subPattern);
			if (subPatterns.empty())
				return {};

			StdVector<ITrPatternSPtr> patterns;
			for (ITrPatternSPtr subPattern : subPatterns)
			{
				patterns.emplace_back(new ITrValueBindPattern{ valueBind.iden, subPattern, subPattern->startIdx, subPattern->endIdx });
			}
			return patterns;
		}
		case ITrPatternKind::Tuple:
		case ITrPatternKind::AdtTupleEnum:
		case ITrPatternKind::Slice:
		{
			StdVector<ITrPatternSPtr>& tupPatterns = pattern->patternKind == ITrPatternKind::Tuple ?
				static_cast<ITrTuplePattern&>(*pattern).subPatterns :
				pattern->patternKind == ITrPatternKind::AdtTupleEnum ?
				static_cast<ITrAdtTupleEnumPattern&>(*pattern).subPatterns :
				static_cast<ITrSlicePattern&>(*pattern).subPatterns;
			
			StdVector<StdVector<ITrPatternSPtr>> subPatterns;
			usize numVariations = 1;
			for (ITrPatternSPtr subPattern : tupPatterns)
			{
				StdVector<ITrPatternSPtr> tmp = SplitEither(subPattern);
				if (!tmp.empty())
				{
					numVariations *= tmp.size();
					subPatterns.push_back(std::move(tmp));
				}
				else
				{
					subPatterns.push_back({ subPattern });
				}
			}

			if (numVariations == 1)
				return {};

			StdVector<StdVector<ITrPatternSPtr>> tmp;
			tmp.resize(numVariations);
			for (StdVector<ITrPatternSPtr> patterns : subPatterns)
			{
				if (patterns.size() == 1)
				{
					for (StdVector<ITrPatternSPtr>& tmp0 : tmp)
					{
						tmp0.push_back(patterns[0]);
					}
				}
				else
				{
					usize stride = numVariations / patterns.size();
					for (usize i = 0, cnt = 0; i < numVariations; ++cnt)
					{
						for (usize j = 0; j < stride; ++j, ++i)
							tmp[i].push_back(patterns[cnt]);
					}
					numVariations = stride;
				}
			}

			StdVector<ITrPatternSPtr> patterns;
			for (StdVector<ITrPatternSPtr>& tmp0 : tmp)
			{
				if (pattern->patternKind == ITrPatternKind::Tuple)
					patterns.emplace_back(new ITrTuplePattern{ std::move(tmp0), pattern->startIdx, pattern->endIdx });
				else if (pattern->patternKind == ITrPatternKind::AdtTupleEnum)
					patterns.emplace_back(new ITrAdtTupleEnumPattern{ static_cast<ITrAdtTupleEnumPattern&>(*pattern).qualName, std::move(tmp0), pattern->startIdx, pattern->endIdx });
				else
					patterns.emplace_back(new ITrSlicePattern{ std::move(tmp0), pattern->startIdx, pattern->endIdx });
			}

			return patterns;
		}
		
		case ITrPatternKind::AdtAggrEnum:
		case ITrPatternKind::Aggr:
		{
			StdPairVector<StdString, ITrPatternSPtr>& args = pattern->patternKind == ITrPatternKind::AdtAggrEnum ?
				static_cast<ITrAdtAggrEnumPattern&>(*pattern).args :
				static_cast<ITrAggrPattern&>(*pattern).args;
			
			StdPairVector<StdString, StdVector<ITrPatternSPtr>> subPatterns;
			usize numVariations = 1;
			for (StdPair<StdString, ITrPatternSPtr>& pair : args)
			{
				StdVector<ITrPatternSPtr> tmp = SplitEither(pair.second);
				if (!tmp.empty())
				{
					numVariations *= tmp.size();
					subPatterns.emplace_back(pair.first, std::move(tmp));
				}
				else
				{
					subPatterns.emplace_back(pair.first, StdVector<ITrPatternSPtr>{ pair.second });
				}
			}

			if (numVariations == 1)
				return {};

			StdVector<StdPairVector<StdString, ITrPatternSPtr>> tmp;
			tmp.resize(numVariations);
			for (StdPair<StdString, StdVector<ITrPatternSPtr>>& pair : subPatterns)
			{
				StdVector<ITrPatternSPtr>& patterns = pair.second;
				if (patterns.size() == 1)
				{
					for (StdPairVector<StdString, ITrPatternSPtr>& tmp0 : tmp)
					{
						tmp0.emplace_back(pair.first, patterns[0]);
					}
				}
				else
				{
					usize stride = numVariations / patterns.size();
					for (usize i = 0, cnt = 0; i < numVariations; ++cnt)
					{
						for (usize j = 0; j < stride; ++j, ++i)
							tmp[i].emplace_back(pair.first, patterns[cnt]);
					}
					numVariations = stride;
				}
			}

			QualNameSPtr qualName = pattern->patternKind == ITrPatternKind::AdtAggrEnum ?
				static_cast<ITrAdtAggrEnumPattern&>(*pattern).qualName :
				static_cast<ITrAggrPattern&>(*pattern).qualName;
			
			StdVector<ITrPatternSPtr> patterns;
			for (StdPairVector<StdString, ITrPatternSPtr>& tmp0 : tmp)
			{
				if (pattern->patternKind == ITrPatternKind::AdtAggrEnum)
					patterns.emplace_back(new ITrAdtAggrEnumPattern{ qualName, std::move(tmp0), pattern->startIdx, pattern->endIdx });
				else
					patterns.emplace_back(new ITrAggrPattern{ qualName, std::move(tmp0), pattern->startIdx, pattern->endIdx });
			}

			return patterns;
		}
		case ITrPatternKind::Either:
		{
			ITrEitherPattern& either = static_cast<ITrEitherPattern&>(*pattern);
			return either.subPatterns;
		}
		default:
			return {};
		}
	}

	ITrSwitchGroup SwitchProcessPass::CreateGroup(ITrPatternSPtr pattern, TypeHandle& type, usize caseId, usize depth)
	{
		StdString bindName;
		if (pattern->patternKind == ITrPatternKind::ValueBind)
		{
			ITrValueBindPattern& valueBind = static_cast<ITrValueBindPattern&>(*pattern);
			bindName = valueBind.iden;
			pattern = valueBind.subPattern;
		}

		ITrSwitchGroup group{ ITrSwitchGroupKind::Leaf, depth };
		if (pattern)
		{
			switch (pattern->patternKind)
			{
			case ITrPatternKind::Placeholder: group = CreateLeaf(pattern, type, caseId, depth); group.isDefCase = true; break;
			case ITrPatternKind::Wildcard: break;
			case ITrPatternKind::ValueBind: break;
			case ITrPatternKind::Literal: group = CreateLitMatch(pattern, type, caseId, depth); break;
			case ITrPatternKind::Range: group = CreateRange(pattern, type, caseId, depth); break;
			case ITrPatternKind::Tuple: group = CreateTuple(pattern, type, caseId, depth); break;
			case ITrPatternKind::ValueEnum:
			case ITrPatternKind::AdtTupleEnum:
			case ITrPatternKind::AdtAggrEnum: group = CreateEnum(pattern, type, caseId, depth); break;
			case ITrPatternKind::Aggr: group = CreateAggr(pattern, type, caseId, depth); break;
			case ITrPatternKind::Slice: group = CreateSlice(pattern, type, caseId, depth); break;
			case ITrPatternKind::Either: break;
			case ITrPatternKind::Type: break;
			default:;
			}
		}
		else
		{
			group = CreateLeaf(pattern, type, caseId, depth);
			group.isDefCase = true;
		}
		
		group.bindName = bindName;
		return group;
	}

	ITrSwitchGroup SwitchProcessPass::CreateLeaf(ITrPatternSPtr pattern, TypeHandle& type, usize caseId, usize depth)
	{
		ITrSwitchGroup group{ ITrSwitchGroupKind::Leaf, depth };
		group.depth = depth;
		group.cases.push_back(caseId);
		return group;
	}

	ITrSwitchGroup SwitchProcessPass::CreateRange(ITrPatternSPtr pattern, TypeHandle& type, usize caseId, usize depth)
	{
		ITrSwitchGroup group{ ITrSwitchGroupKind::Range, depth };
		group.depth = depth;
		group.cases.push_back(caseId);

		ITrRangePattern& range = static_cast<ITrRangePattern&>(*pattern);
		group.valOrFrom = GetLitVal(range.from);
		group.to = GetLitVal(range.to);
		if (!range.isInclusive)
		{
			--group.to;
			if (group.valOrFrom == group.to)
			{
				group.kind = ITrSwitchGroupKind::LitMatch;
			}
		}
		
		ITrSwitchGroup subGroup = CreateLeaf(pattern, type, caseId, depth);
		group.subGroups.push_back(subGroup);
		return group;
	}

	ITrSwitchGroup SwitchProcessPass::CreateLitMatch(ITrPatternSPtr pattern, TypeHandle& type, usize caseId,
	                                                 usize depth)
	{
		ITrSwitchGroup group{ ITrSwitchGroupKind::LitMatch, depth };
		group.depth = depth;
		group.cases.push_back(caseId);

		ITrSwitchGroup subGroup = CreateLeaf(pattern, type, caseId, depth);
		Token& lit = static_cast<ITrLiteralPattern&>(*pattern).lit;
		subGroup.valOrFrom = GetLitVal(lit);
		
		group.subGroups.push_back(subGroup);
		return group;
	}

	ITrSwitchGroup SwitchProcessPass::CreateEnum(ITrPatternSPtr pattern, TypeHandle& type, usize caseId, usize depth)
	{
		SymbolSPtr sym = type.AsIden().sym.lock();

		if (sym->kind == SymbolKind::ValEnum)
			return CreateEnumMatch(pattern, type, caseId, depth);

		if (pattern->patternKind != ITrPatternKind::ValueEnum)
		{
			Span span = g_Ctx.spanManager.GetSpan(pattern->startIdx);
			g_ErrorSystem.Error(span, "No members are matched");
			return CreateLeaf(pattern, type, caseId, depth);
		}

		if (sym->type.Kind() == TypeKind::Tuple)
			return CreateTupleEnumMatch(pattern, type, caseId, depth);
		return CreateAggrEnumMatch(pattern, type, caseId, depth);
	}

	ITrSwitchGroup SwitchProcessPass::CreateEnumMatch(ITrPatternSPtr pattern, TypeHandle& type, usize caseId, usize depth)
	{
		ITrSwitchGroup group{ ITrSwitchGroupKind::EnumMatch, depth };
		group.cases.push_back(caseId);

		ITrSwitchGroup subGroup = CreateLeaf(pattern, type, caseId, depth);
		subGroup.member = static_cast<ITrValueEnumPattern&>(*pattern).qualName->LastIden();
		group.subGroups.push_back(subGroup);
		return group;
	}

	ITrSwitchGroup SwitchProcessPass::CreateTupleEnumMatch(ITrPatternSPtr pattern, TypeHandle& type, usize caseId, usize depth)
	{
		ITrSwitchGroup group{ ITrSwitchGroupKind::EnumMatch, depth };
		group.cases.push_back(caseId);

		ITrSwitchGroup subGroup = CreateTuple(pattern, type, caseId, depth + 1);
		subGroup.member = static_cast<ITrAdtTupleEnumPattern&>(*pattern).qualName->LastIden();
		group.subGroups.push_back(subGroup);
		return group;
	}

	ITrSwitchGroup SwitchProcessPass::CreateAggrEnumMatch(ITrPatternSPtr pattern, TypeHandle& type, usize caseId, usize depth)
	{
		ITrSwitchGroup group{ ITrSwitchGroupKind::EnumMatch, depth };
		group.cases.push_back(caseId);

		ITrSwitchGroup subGroup = CreateAggr(pattern, type, caseId, depth + 1);
		subGroup.member = static_cast<ITrAdtTupleEnumPattern&>(*pattern).qualName->LastIden();
		group.subGroups.push_back(subGroup);
		return group;
	}

	ITrSwitchGroup SwitchProcessPass::CreateTuple(ITrPatternSPtr pattern, TypeHandle& type, usize caseId, usize depth)
	{
		ITrSwitchGroup tupleGroup{ ITrSwitchGroupKind::Tuple, depth };
		tupleGroup.cases.push_back(caseId);

		TupleType& tupType = type.AsTuple();
		usize numElems = tupType.subTypes.size();
		
		StdVector<ITrPatternSPtr> subPatterns = pattern->patternKind == ITrPatternKind::Tuple ?
			static_cast<ITrTuplePattern&>(*pattern).subPatterns :
			static_cast<ITrAdtTupleEnumPattern&>(*pattern).subPatterns;

		bool foundWildcard = false;
		for (usize i = 0; i < subPatterns.size(); ++i)
		{
			ITrPatternSPtr subPattern = subPatterns[i];
			if (subPattern->patternKind == ITrPatternKind::Wildcard)
			{
				if (foundWildcard)
				{
					Span span = g_Ctx.spanManager.GetSpan(subPattern->startIdx);
					g_ErrorSystem.Error(span, "Cannot have more than 1 wildcard in a tuple pattern");
				}
				foundWildcard = true;

				subPatterns[i].reset(new ITrPlaceholderPattern{ false, subPattern->startIdx } );
				for (usize j = subPatterns.size(); j < numElems; ++i, ++j)
					subPatterns.emplace(subPatterns.begin() + i, new ITrPlaceholderPattern{ false, subPattern->startIdx });
			}
		}

		ITrSwitchGroup* pLastGroup = &tupleGroup;
		for (usize i = 0; i < tupType.subTypes.size(); ++i)
		{
			TypeHandle subType = tupType.subTypes[i];
			ITrPatternSPtr subPattern = subPatterns[i];
			
			ITrSwitchGroup indexGroup{ ITrSwitchGroupKind::TupleIndex, depth + 1 };
			indexGroup.cases.push_back(caseId);
			indexGroup.idx = i;
			
			ITrSwitchGroup subGroup = CreateGroup(subPattern, subType, caseId, depth + 2);
			// Skip empty node
			if (i + 1 != numElems && subGroup.kind == ITrSwitchGroupKind::Leaf)
				continue;
			
			indexGroup.subGroups.push_back(subGroup);
			pLastGroup->subGroups.push_back(indexGroup);
			
			if (i + 1 != numElems)
			{
				pLastGroup = &GetSecondToLastSubgroup(*pLastGroup);
				if (pLastGroup->subGroups[0].kind == ITrSwitchGroupKind::Leaf)
					pLastGroup->subGroups.clear();
				else
					pLastGroup = &pLastGroup->subGroups[0];
			}
		}

		return tupleGroup;
	}

	ITrSwitchGroup SwitchProcessPass::CreateAggr(ITrPatternSPtr pattern, TypeHandle& type, usize caseId, usize depth)
	{
		SymbolSPtr structSym = type.AsIden().sym.lock();
		StdVector<SymbolSPtr> children;
		structSym->children->Foreach([&children](SymbolSPtr sym, QualNameSPtr iface)
		{
			if (!iface && sym->kind == SymbolKind::Var)
				children.push_back(sym);
		});
		std::sort(children.begin(), children.end(), [](const SymbolSPtr& sym0, const SymbolSPtr& sym1)
		{
			return sym0->offset < sym1->offset;
		});

		StdPairVector<StdString, ITrPatternSPtr>& members = pattern->patternKind == ITrPatternKind::Aggr ?
			static_cast<ITrAggrPattern&>(*pattern).args :
			static_cast<ITrAdtAggrEnumPattern&>(*pattern).args;

		ITrSwitchGroup aggrGroup{ ITrSwitchGroupKind::Aggr, depth };
		
		ITrSwitchGroup* pLastGroup = &aggrGroup;
		usize numMembers = members.size();
		usize memIdx = 0;
		for (SymbolSPtr child : children)
		{
			const StdString& name = child->qualName->LastIden();

			for (StdPair<StdString, ITrPatternSPtr> member : members)
			{
				if (member.first != name)
					continue;

				ITrSwitchGroup memberGroup{ ITrSwitchGroupKind::AggrMember, depth + 1 };
				memberGroup.member = name;
				memberGroup.cases.push_back(caseId);
				
				ITrSwitchGroup subGroup = CreateGroup(member.second, child->type, caseId, depth + 2);
				memberGroup.subGroups.push_back(subGroup);
				pLastGroup->subGroups.push_back(memberGroup);
			}

			if (memIdx + 1 != numMembers)
			{
				pLastGroup = &GetSecondToLastSubgroup(*pLastGroup);
				if (pLastGroup->subGroups[0].kind == ITrSwitchGroupKind::Leaf)
					pLastGroup->subGroups.clear();
				else
					pLastGroup = &pLastGroup->subGroups[0];
				++memIdx;
			}
		}

		return aggrGroup;
	}

	ITrSwitchGroup SwitchProcessPass::CreateSlice(ITrPatternSPtr pattern, TypeHandle& type, usize caseId, usize depth)
	{
		TypeHandle subType = type.Kind() == TypeKind::Slice ? type.AsSlice().subType : type.AsArray().subType;

		StdVector<ITrPatternSPtr> subPatterns = static_cast<ITrSlicePattern&>(*pattern).subPatterns;

		ITrSwitchGroup group{ ITrSwitchGroupKind::Slice, depth };
		group.cases.push_back(caseId);

		bool foundWildcard = false;
		ITrSwitchGroup* pLastGroup = &group;
		for (usize i = 0; i < subPatterns.size(); ++i)
		{
			ITrPatternSPtr subPattern = subPatterns[i];
			if (foundWildcard)
			{
				if (subPattern->patternKind == ITrPatternKind::Wildcard)
				{
					Span span = g_Ctx.spanManager.GetSpan(subPattern->startIdx);
					g_ErrorSystem.Error(span, "Cannot have more than 1 wildcard");
					continue;
				}

				ITrSwitchGroup indexGroup{ ITrSwitchGroupKind::SliceIndex, depth + 1 };
				indexGroup.idx = subPatterns.size() - i;
				indexGroup.indexFromBack = true;
				indexGroup.cases.push_back(caseId);

				ITrSwitchGroup subGroup = CreateGroup(subPattern, subType, caseId, depth + 2);
				indexGroup.subGroups.push_back(subGroup);
				pLastGroup->subGroups.push_back(indexGroup);
			}
			else
			{
				if (subPattern->patternKind == ITrPatternKind::Wildcard)
				{
					foundWildcard = true;
					continue;
				}

				ITrSwitchGroup indexGroup{ ITrSwitchGroupKind::SliceIndex, depth + 1 };
				indexGroup.idx = i;
				indexGroup.cases.push_back(caseId);
				
				ITrSwitchGroup subGroup = CreateGroup(subPattern, subType, caseId, depth + 2);
				indexGroup.subGroups.push_back(subGroup);
				pLastGroup->subGroups.push_back(indexGroup);
			}

			if (i != subPatterns.size() - 1)
			{
				pLastGroup = &GetSecondToLastSubgroup(*pLastGroup);
				if (pLastGroup->subGroups[0].kind == ITrSwitchGroupKind::Leaf)
					pLastGroup->subGroups.clear();
				else
					pLastGroup = &pLastGroup->subGroups[0];
			}
		}

		return group;
	}

	ITrSwitchGroup& SwitchProcessPass::GetSecondToLastSubgroup(ITrSwitchGroup& group)
	{
		ITrSwitchGroup* pGroup = &group;
		while (!pGroup->subGroups.empty())
		{
			ITrSwitchGroup& tmp = pGroup->subGroups[0];
			if (tmp.subGroups.empty())
				return *pGroup;
			
			pGroup = &pGroup->subGroups[0];
		}
		return *pGroup;
	}

	void SwitchProcessPass::MergeGroups(ITrSwitchGroup& baseGroup, ITrSwitchGroup& subGroup)
	{
		if (baseGroup.subGroups.empty())
		{
			baseGroup.subGroups.push_back(subGroup);
			return;
		}

		switch (subGroup.kind)
		{
		case ITrSwitchGroupKind::Leaf:
		{
			if (subGroup.isDefCase)
			{
				baseGroup.subGroups[0].subGroups.push_back(subGroup);
			}
			else
			{
				baseGroup.cases.push_back(subGroup.cases[0]);
			}
			
			break;
		}
		case ITrSwitchGroupKind::Range:
		{
			auto it = std::find_if(baseGroup.subGroups.begin(), baseGroup.subGroups.end(), [&subGroup](const ITrSwitchGroup& other)
			{
				return other.kind == ITrSwitchGroupKind::Range && other.valOrFrom == subGroup.valOrFrom && other.to == subGroup.to;
			});

			if (it != baseGroup.subGroups.end())
				MergeGroups(*it, subGroup.subGroups[0]);
			else
				baseGroup.subGroups.push_back(subGroup);
			break;
		}
		case ITrSwitchGroupKind::LitMatch:
		{
			auto it = std::find_if(baseGroup.subGroups[0].subGroups.begin(), baseGroup.subGroups[0].subGroups.end(), [&subGroup](const ITrSwitchGroup& other)
			{
				return other.valOrFrom == subGroup.subGroups[0].valOrFrom;
			});

			if (it != baseGroup.subGroups[0].subGroups.end())
				MergeGroups(*it, subGroup.subGroups[0]);
			else
				baseGroup.subGroups[0].subGroups.push_back(subGroup.subGroups[0]);
			break;
		}
		case ITrSwitchGroupKind::EnumMatch:
		{
			auto it = std::find_if(baseGroup.subGroups[0].subGroups.begin(), baseGroup.subGroups[0].subGroups.end(), [&subGroup](const ITrSwitchGroup& other)
			{
				return other.member == subGroup.subGroups[0].member;
			});

			if (it != baseGroup.subGroups[0].subGroups.end())
				MergeGroups(*it, subGroup.subGroups[0]);
			else
				baseGroup.subGroups[0].subGroups.push_back(subGroup.subGroups[0]);
			break;
		}
		case ITrSwitchGroupKind::TupleIndex:
		{
			if (!baseGroup.subGroups.empty() && baseGroup.subGroups.back().idx == subGroup.idx)
				MergeGroups(baseGroup.subGroups.back(), subGroup.subGroups[0]);
			else
				baseGroup.subGroups.push_back(subGroup);
			break;
		}
		case ITrSwitchGroupKind::AggrMember:
		{
			if (!baseGroup.subGroups.empty() && baseGroup.subGroups.back().member == subGroup.member)
				MergeGroups(baseGroup.subGroups.back(), subGroup.subGroups[0]);
			else
				baseGroup.subGroups.push_back(subGroup);
			break;
		}
		default:
		{
			if (!subGroup.subGroups.empty())
			{
				if (baseGroup.subGroups.empty())
					baseGroup.subGroups.push_back(subGroup);
				else
					MergeGroups(baseGroup.subGroups[0], subGroup.subGroups[0]);
			}
		}
		}
	}

	u64 SwitchProcessPass::GetLitVal(Token& tok)
	{
		switch (tok.Type())
		{
		case TokenType::False: return 0;
		case TokenType::True: return 1;

		case TokenType::CharLit:
		case TokenType::U8Lit:
		case TokenType::U16Lit:
		case TokenType::U32Lit:
		case TokenType::U64Lit:
		case TokenType::U128Lit:
			return tok.Unsigned();
		case TokenType::I8Lit:
		case TokenType::I16Lit:
		case TokenType::I32Lit:
		case TokenType::I64Lit:
		case TokenType::I128Lit:
			return tok.Signed();
		case TokenType::F16Lit:
		case TokenType::F32Lit:
		case TokenType::F64Lit:
		case TokenType::F128Lit:
		{
			Span span = g_Ctx.spanManager.GetSpan(tok.Idx());
			g_ErrorSystem.Error(span, "float literal patterns are not allowed");
			return 0;
		}
		default:
		{
			Span span = g_Ctx.spanManager.GetSpan(tok.Idx());
			g_ErrorSystem.Error(span, "Unknown literal pattern");
			return 0;
		}
		}
	}

	CopyCheckPass::CopyCheckPass()
		: ITrSemanticPass("Copy Check Pass")
		, m_pBoundsInfo(nullptr)
	{
	}

	void CopyCheckPass::Process(ITrModule& mod)
	{
		SetModule(mod);

		Foreach(ITrVisitorDefKind::Any, [&](ITrFunc& node)
		{
			m_pBoundsInfo = &node.sym.lock()->boundsInfo;
			Walk(node);
		});
	}

	void CopyCheckPass::Visit(ITrAssign& node)
	{
		bool needsCheck;
		switch (node.rExpr->exprKind)
		{
		case ITrExprKind::Move:
		case ITrExprKind::FuncOrMethodCall:
			needsCheck = false;
			break;
		default:
			needsCheck = true;
		}

		if (needsCheck && !CheckCopyable(node.rExpr->handle, *m_pBoundsInfo))
		{
			Span span = g_Ctx.spanManager.GetSpan(node.rExpr->startIdx);
			StdString typeName = node.rExpr->handle.ToString();
			g_ErrorSystem.Error(span, "type %s is not copyable", typeName.c_str());
		}
	}

	bool CopyCheckPass::CheckCopyable(TypeHandle type, BoundsInfo& boundsInfo)
	{
		static QualNameSPtr coreCopyMarker = QualName::Create({ "core", "marker", "Copy" });

		switch (type.Kind())
		{
		case TypeKind::Invalid:
		case TypeKind::Builtin:
		case TypeKind::Ptr:
		case TypeKind::Ref:
		case TypeKind::Slice:
		case TypeKind::Func:
			return true;
		case TypeKind::Array:
			return CheckCopyable(type.AsArray().subType, boundsInfo);
		case TypeKind::Tuple:
		{
			TupleType& tup = type.AsTuple();
			for (TypeHandle subType : tup.subTypes)
			{
				if (!CheckCopyable(subType, boundsInfo))
					return false;
			}
			return true;
		}
		case TypeKind::Opt:
			return CheckCopyable(type.AsOpt().subType, boundsInfo);
		case TypeKind::Generic:
		{
			const Bounds& bounds = boundsInfo.GetBounds(type);
			for (TypeHandle handle : bounds.bounds)
			{
				if (handle.Kind() == TypeKind::Iden)
				{
					QualNameSPtr qualName = handle.AsIden().qualName;
					if (qualName == coreCopyMarker)
						return true;
				}
			}
			return false;
		}
		case TypeKind::Iden:
		{
			SymbolSPtr sym = type.AsIden().sym.lock();
			return sym->HasMarker(coreCopyMarker);
		}
		default: ;
			return false;
		}
	}

	ErrHandlerCollectionPass::ErrHandlerCollectionPass()
		: ITrSemanticPass("error handler collection pass")
	{
	}

	void ErrHandlerCollectionPass::Process(ITrModule& mod)
	{
		SetModule(mod);
		m_VisitDefs = true;
		Foreach(ITrVisitorDefKind::Any, [&](ITrFunc& node)
		{
			m_FuncCtx = node.ctx;
			Walk(node);
		});
	}

	void ErrHandlerCollectionPass::Visit(ITrErrHandler& node)
	{
		TypeHandle errType = node.errType->handle;
		m_FuncCtx->errHandlers.emplace_back(errType.Type(), node.qualName);
	}

	TryCheckPass::TryCheckPass()
		: ITrSemanticPass("try check pass")
	{
	}

	void TryCheckPass::Process(ITrModule& mod)
	{
		Foreach(ITrVisitorDefKind::Any, [&](ITrFunc& node)
		{
			m_FuncCtx = node.ctx;
			m_ErrorHandle = node.errorType ? node.errorType->handle : TypeHandle{};
			Walk(node);
		});
	}

	void TryCheckPass::Visit(ITrTry& node)
	{
		TypeHandle exprType = node.expr->handle;
		StdVector<SymbolInstWPtr>& ifaces = exprType.AsIden().sym.lock()->ifaces;
		
		QualNameSPtr possibleHandler;
		for (StdPair<TypeSPtr, QualNameSPtr>& pair : m_FuncCtx->errHandlers)
		{
			if (exprType.Type() == pair.first)
			{
				possibleHandler = pair.second;
				break;
			}

			for (SymbolInstWPtr& inst : ifaces)
			{
				TypeHandle ifaceType = inst.lock()->type;
				if (ifaceType.Type() == pair.first)
				{
					possibleHandler = pair.second;
					break;
				}
			}
		}

		if (possibleHandler)
		{
			TypeHandle foundType = node.expr->handle;
			bool found = false;

			if (m_ErrorHandle.IsValid())
			{
				for (SymbolInstWPtr& inst : ifaces)
				{
					if (inst.lock()->type == m_ErrorHandle)
					{
						found = true;
						break;
					}
				}
			}

			if (!found)
			{
				Span span = g_Ctx.spanManager.GetSpan(node.startIdx);
				StdString foundTypeName = foundType.ToString();
				StdString expectedTypeName = m_ErrorHandle.ToString();
				g_ErrorSystem.Error(span, "Cannot find an error handler for type '%s', this error can also not be rethrown, since it's not compatible with '%s'", foundTypeName.c_str(), expectedTypeName.c_str());
			}
		}

		node.errHandlerName = possibleHandler;
	}
}
