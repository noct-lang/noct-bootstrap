#include "qualname.hpp"

#include <utility>

#include "utils.hpp"

namespace Noctis
{
	IdenGeneric::IdenGeneric()
		: isType(true)
		, isSpecialized(false)
	{
	}

	StdUnorderedMap<TypeHandle, StdUnorderedMap<QualNameSPtr, TypeDisambiguationSPtr>> TypeDisambiguation::s_TypeDisambiguations;

	TypeDisambiguationSPtr TypeDisambiguation::Create(TypeHandle type, QualNameSPtr ifaceQualName)
	{
		auto it = s_TypeDisambiguations.find(type);
		if (it == s_TypeDisambiguations.end())
			it = s_TypeDisambiguations.insert(std::pair{ type, StdUnorderedMap<QualNameSPtr, TypeDisambiguationSPtr>{} }).first;

		auto subIt = it->second.find(ifaceQualName);
		if (subIt != it->second.end())
			return subIt->second;

		TypeDisambiguationSPtr td{ new TypeDisambiguation{ type, ifaceQualName } };
		it->second.try_emplace(ifaceQualName, td);
		return td;
	}

	TypeDisambiguation::TypeDisambiguation(TypeHandle type, QualNameSPtr qualName)
		: m_Type(type)
		, m_QualName(qualName)
	{
	}

	StdUnorderedMap<StdString, StdVector<QualNameSPtr>> QualName::s_BaseNames = {};
	StdUnorderedMap<TypeDisambiguationSPtr, QualNameSPtr> QualName::s_TypeDisambiguationBaseNames = {};

	QualNameSPtr QualName::Create(const StdString& iden)
	{
		return Create(StdVector<StdString>{ iden }, {});
	}

	QualNameSPtr QualName::Create(const StdString& iden, const StdVector<IdenGeneric>& generics)
	{
		return Create(StdVector<StdString>{ iden }, generics);
	}

	QualNameSPtr QualName::Create(const StdVector<StdString>& idens)
	{
		return Create(idens, {});
	}

	QualNameSPtr QualName::Create(const StdVector<StdString>& idens, const StdVector<IdenGeneric>& generics)
	{
		if (idens.empty())
			return nullptr;
		
		auto it = s_BaseNames.find(idens[0]);
		if (it == s_BaseNames.end())
			it = s_BaseNames.try_emplace(idens[0], StdVector<QualNameSPtr>{}).first;

		if (it->second.empty())
		{
			QualNameSPtr noGenBase{ new QualName{ nullptr, idens[0] } };
			noGenBase->m_Fuzzy = noGenBase->m_NoGenSelf = noGenBase->m_Self = noGenBase;
			it->second.push_back(noGenBase);
		}

		if (idens.size() == 1)
		{
			if (generics.empty())
				return it->second[0];

			for (QualNameSPtr tmp : it->second)
			{
				if (CompareGenerics(tmp->m_Generics, generics))
					return tmp;
			}

			QualNameSPtr qualName{ new QualName{ nullptr, idens[0] } };
			qualName->m_Self = qualName;
			qualName->m_Generics = generics;
			
			// first add, then create fuzzy to prevent infinite recursion
			it->second.push_back(qualName);
			qualName->m_NoGenSelf = it->second[0];
			qualName->m_Fuzzy = CreateFuzzy(nullptr, idens, generics);
			return qualName;
		}

		QualNameSPtr qualName = it->second[0];
		for (usize i = 1; i < idens.size() - 1; ++i)
		{
			qualName = qualName->Append(idens[i]);
		}
		return qualName->Append(idens.back(), generics);
	}

	QualNameSPtr QualName::Create(TypeDisambiguationSPtr disambiguation)
	{
		if (!disambiguation)
			return nullptr;

		auto it = s_TypeDisambiguationBaseNames.find(disambiguation);
		if (it != s_TypeDisambiguationBaseNames.end())
			return it->second;

		QualNameSPtr qualName{ new QualName{ disambiguation } };
		qualName->m_Self = qualName->m_NoGenSelf = qualName;
		qualName->m_Fuzzy = CreateFuzzy(disambiguation, {}, {});
		
		s_TypeDisambiguationBaseNames.try_emplace(disambiguation, qualName);
		return qualName;
	}

	QualNameSPtr QualName::Append(QualNameSPtr qualName)
	{
		return Append(qualName->Idens(), qualName->Generics());
	}

	QualNameSPtr QualName::Append(const StdString& iden)
	{
		return Append(iden, {});
	}

	QualNameSPtr QualName::Append(const StdString& iden, const StdVector<IdenGeneric>& generics)
	{
		if (!m_Generics.empty())
		{
			QualNameSPtr qualName = m_NoGenSelf.lock();
			StdVector<IdenGeneric> tmpGens = m_Generics;
			if (!generics.empty())
				tmpGens.insert(tmpGens.end(), generics.begin(), generics.end());
			return qualName->Append(iden, tmpGens);
		}
		
		auto it = m_Children.find(iden);
		if (it == m_Children.end())
		{
			it = m_Children.try_emplace(iden, StdVector<QualNameSPtr>{}).first;
			QualNameSPtr qualName{ new QualName{ m_Self.lock(), iden } };
			qualName->m_Fuzzy = qualName->m_NoGenSelf = qualName->m_Self = qualName;
			it->second.push_back(qualName);
		}

		for (QualNameSPtr qualName : it->second)
		{
			if (CompareGenerics(qualName->Generics(), generics))
				return qualName;
		}

		QualNameSPtr qualName{ new QualName{ m_Self.lock(), iden } };
		qualName->m_Self = qualName;
		qualName->m_Generics = generics;
		qualName->m_NoGenSelf = it->second[0];

		// first add, then create fuzzy to prevent infinite recursion
		it->second.push_back(qualName);
		StdVector<StdString> fuzzyIdens = m_Idens;
		fuzzyIdens.push_back(iden);
		qualName->m_Fuzzy = CreateFuzzy(nullptr, fuzzyIdens, generics);
		return qualName;
	}

	QualNameSPtr QualName::Append(const StdVector<StdString>& idens)
	{
		QualNameSPtr qualName = m_Self.lock();
		for (const StdString& iden : idens)
		{
			qualName = qualName->Append(iden);
		}
		return qualName;
	}

	QualNameSPtr QualName::Append(const StdVector<StdString>& idens, const StdVector<IdenGeneric>& generics)
	{
		QualNameSPtr qualName = m_Self.lock();
		for (usize i = 0; i < idens.size() - 1; ++i)
		{
			qualName = qualName->Append(idens[i]);
		}
		return qualName->Append(idens.back(), generics);
	}

	QualNameSPtr QualName::AppendLastIden(QualNameSPtr qualName)
	{
		return Append(qualName->LastIden(), qualName->Generics());
	}

	QualNameSPtr QualName::WithGenerics(const StdVector<IdenGeneric>& generics)
	{
		if (m_Base)
			return m_Base->Append(m_Idens.back(), generics);
		return Create(m_Idens.back(), generics);
	}

	StdString QualName::ToString() const
	{
		StdString name;

		// TODO
		//if (m_Disambiguation)
		//	name += m_Disambiguation->To

		if (m_Base)
		{
			name += m_Base->ToString();
			name += "::" + m_Idens.back();
		}
		else
		{
			name += m_Idens.back();
		}

		if (!m_Generics.empty())
		{
			name += '<';
			for (const IdenGeneric& idenGen : m_Generics)
			{
				if (idenGen.isType)
				{
					if (idenGen.isSpecialized)
						name += idenGen.type.ToString();
					else
						name += idenGen.iden;
				}
				else
				{
					if (idenGen.isSpecialized)
					{
						// TODO
						name += "{}";
					}
					else
					{
						name += idenGen.iden + ':' + idenGen.type.ToString();
					}
				}
			}
			name += '>';
		}
		return name;
	}

	QualNameSPtr QualName::GetSubName(QualNameSPtr base)
	{
		const StdVector<StdString>& baseIdens = base->Idens();

		if (m_Idens.size() <= baseIdens.size())
			return m_Self.lock();

		usize size = baseIdens.size();
		for (usize i = 0; i < size; ++i)
		{
			if (m_Idens[i] != baseIdens[i])
				return nullptr;
		}

		usize numIdens = m_Idens.size() - baseIdens.size();
		usize numGenerics = m_Generics.size() - base->Generics().size();
		return GetSubName(numIdens, numGenerics);
	}

	QualNameSPtr QualName::GetSubName(usize depth, usize numGenerics)
	{
		if (depth >= m_Idens.size())
			return m_Self.lock();
		
		StdVector<StdString> idens;
		idens.assign(m_Idens.end() - depth, m_Idens.end());
		StdVector<IdenGeneric> gens;

		numGenerics = Min(numGenerics, m_Generics.size());
		if (numGenerics)
			gens.assign(m_Generics.end() - numGenerics, m_Generics.end());
		
		return Create(idens, gens);
	}

	QualNameSPtr QualName::GetBaseName(usize depth)
	{
		usize curDepth = Depth();
		if (curDepth == depth)
			return m_Self.lock();

		QualNameSPtr qualName = m_Base;
		--curDepth;
		for (;curDepth > depth; --curDepth)
		{
			qualName = qualName->Base();
		}
		return qualName;
	}

	bool QualName::IsBase()
	{
		return (!m_Disambiguation && m_Idens.size() == 1) ||
			(m_Disambiguation && m_Idens.empty());
	}

	bool QualName::IsSubnameOf(QualNameSPtr base)
	{
		QualNameSPtr qualName = m_Self.lock();
		usize depth = Depth();
		usize baseDepth = base->Depth();
		for (usize i = depth; i >= baseDepth; --i)
		{
			if (qualName == base)
				return true;

			qualName = qualName->Base();
		}
		return false;
	}

	// TODO: Tell how many generic values need to be kept in the base 
	QualNameSPtr QualName::Base(usize numGenerics)
	{
		if (!m_Base)
			return nullptr;

		if (numGenerics == 0)
			return m_Base;

		numGenerics = Min(numGenerics, m_Generics.size());
		StdVector<IdenGeneric> generics = m_Generics;
		generics.resize(numGenerics);
		if (m_Base->m_Base)
			return m_Base->m_Base->Append(m_Base->LastIden(), generics);
		return Create(m_Base->LastIden(), generics);
	}

	QualName::QualName(QualNameSPtr base, StdString iden)
		: m_Base(base)
		, m_Idens({ iden })
	{
		if (base)
			m_Idens.insert(m_Idens.begin(), base->m_Idens.begin(), base->m_Idens.end());
	}

	QualName::QualName(TypeDisambiguationSPtr disambiguation)
		: m_Disambiguation(std::move(disambiguation))
	{
	}

	bool QualName::CompareGenerics(const StdVector<IdenGeneric>& gens0, const StdVector<IdenGeneric>& gens1)
	{
		if (gens0.size() != gens1.size())
			return false;

		for (usize i = 0; i < gens0.size(); ++i)
		{
			const IdenGeneric& gen0 = gens0[i];
			const IdenGeneric& gen1 = gens1[i];

			if (gen0.isType != gen1.isType ||
				gen0.isSpecialized != gen1.isSpecialized)
				return false;

			if (gen0.isType)
			{
				if (gen0.isSpecialized)
				{
					if (gen0.type != gen1.type)
						return false;
				}
				else
				{
					if (gen0.iden != gen1.iden)
						return false;
				}
			}
			else
			{
				if (gen0.itrExpr != gen1.itrExpr)
					return false;
			}
		}

		return true;
	}

	QualNameSPtr QualName::CreateFuzzy(TypeDisambiguationSPtr disambig, const StdVector<StdString>& idens, const StdVector<IdenGeneric>& generics)
	{
		StdVector<IdenGeneric> fuzzyGenerics;
		for (const IdenGeneric& idenGen : generics)
		{
			if (idenGen.isType)
			{
				if (idenGen.isSpecialized)
				{
					// TODO: need access to context
				}
				else
				{
					fuzzyGenerics.push_back(idenGen);
				}
			}
			else
			{
				IdenGeneric tmp = idenGen;
				tmp.itrExpr = nullptr;
				fuzzyGenerics.push_back(tmp);
			}
		}

		if (disambig)
			return Create(disambig)->Append(idens, generics);
		return Create(idens, generics);
	}
}
