#include "qualname.hpp"

namespace Noctis
{
	StdUnorderedMap<StdString, IdenSPtr> Iden::s_Idens = {};
	StdString Iden::s_SearchString = {};

	IdenSPtr Iden::Create(StdStringView name)
	{
		// Use static string to as key, to try to decrease allocations
		s_SearchString.assign(name);
		auto it = s_Idens.find(s_SearchString);
		if (it != s_Idens.end())
			return it->second;

		IdenSPtr iden{ new Iden{ s_SearchString } };
		s_Idens.try_emplace(s_SearchString, iden);
		return iden;
	}

	Iden::Iden(StdString name)
		: m_Name(std::move(name))
	{
	}

	StdUnorderedMap<QualNameSPtr, StdUnorderedMap<TypeHandle, TypeDisambiguationSPtr>> TypeDisambiguation::m_sTypeDisambiguations;

	TypeDisambiguationSPtr TypeDisambiguation::Create(QualNameSPtr qualName, TypeHandle type)
	{
		auto qIt = m_sTypeDisambiguations.find(qualName);
		if (qIt == m_sTypeDisambiguations.end())
			qIt = m_sTypeDisambiguations.insert(std::pair{ qualName, StdUnorderedMap<TypeHandle, TypeDisambiguationSPtr>{} }).first;

		auto it = qIt->second.find(type);
		if (it != qIt->second.end())
			return it->second;

		TypeDisambiguationSPtr td{ new TypeDisambiguation{ qualName, type } };
		qIt->second.try_emplace(type, td);
		return td;
	}

	TypeDisambiguation::TypeDisambiguation(QualNameSPtr qualName, TypeHandle type)
		: m_QualName(qualName)
		, m_Type(type)
	{
	}

	StdUnorderedMap<IdenSPtr, QualNameSPtr> QualName::s_BaseNames = {};
	StdUnorderedMap<TypeDisambiguationSPtr, QualNameSPtr> QualName::s_TypeDisambiguationBaseNames = {};

	QualNameSPtr QualName::Create(IdenSPtr iden)
	{
		auto it = s_BaseNames.find(iden);
		if (it != s_BaseNames.end())
			return it->second;

		QualNameSPtr qualName{ new QualName{ nullptr, iden } };
		s_BaseNames.try_emplace(iden, qualName);
		return qualName;
	}

	QualNameSPtr QualName::Create(TypeDisambiguationSPtr disambiguation)
	{
		if (!disambiguation)
			return nullptr;

		auto it = s_TypeDisambiguationBaseNames.find(disambiguation);
		if (it != s_TypeDisambiguationBaseNames.end())
			return it->second;

		QualNameSPtr qualName{ new QualName{ disambiguation } };
		s_TypeDisambiguationBaseNames.try_emplace(disambiguation, qualName);
		return qualName;
	}

	QualNameSPtr QualName::Create(QualNameSPtr base, IdenSPtr iden)
	{
		if (!base)
			return Create(iden);
		
		auto it = base->m_Children.find(iden);
		if (it != base->m_Children.end())
			return it->second;

		QualNameSPtr qualName{ new QualName{ base, iden } };
		base->m_Children.try_emplace(iden, qualName);
		return qualName;
	}

	QualNameSPtr QualName::Create(QualNameSPtr base, StdStringView name)
	{
		return Create(base, Iden::Create(name));
	}

	QualNameSPtr QualName::Create(const StdVector<StdString>& names)
	{
		QualNameSPtr qualName;
		for (const StdString& name : names)
		{
			IdenSPtr iden = Iden::Create(name);
			qualName = Create(qualName, iden);
		}
		return qualName;
	}

	QualNameSPtr QualName::Create(const StdVector<IdenSPtr>& idens)
	{
		QualNameSPtr qualName;
		for (const IdenSPtr iden : idens)
		{
			qualName = Create(qualName, iden);
		}
		return qualName;
	}

	QualNameSPtr QualName::Create(QualNameSPtr first, const StdVector<IdenSPtr>& idens)
	{
		QualNameSPtr tmp = first;
		for (IdenSPtr iden : idens)
		{
			tmp = QualName::Create(tmp, iden);
		}
		return tmp;
	}

	StdString QualName::ToString()
	{
		QualName* qualName = this;
		StdString name;
		do
		{
			StdString str = "::" + qualName->Iden()->Name();
			name.insert(name.begin(), str.begin(), str.end());
			qualName = qualName->Base().get();
		}
		while (qualName);
		return name;
	}

	StdVector<IdenSPtr> QualName::AllIdens()
	{
		StdVector<IdenSPtr> idens;
		QualName* qualName = this;
		do
		{
			idens.push_back(qualName->m_Iden);
			qualName = qualName->m_Base.get();
		}
		while (qualName);
		std::reverse(idens.begin(), idens.end());
		return idens;
	}

	QualName::QualName(QualNameSPtr base, IdenSPtr iden)
		: m_Base(base)
		, m_Iden(iden)
	{
	}

	QualName::QualName(TypeDisambiguationSPtr disambiguation)
		: m_Disambiguation(disambiguation)
	{
	}
}
