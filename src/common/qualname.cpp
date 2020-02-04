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
	
	StdUnorderedMap<IdenSPtr, QualNameSPtr> QualName::s_BaseNames = {};

	QualNameSPtr QualName::Create(IdenSPtr iden)
	{
		auto it = s_BaseNames.find(iden);
		if (it != s_BaseNames.end())
			return it->second;

		QualNameSPtr qualName{ new QualName{ nullptr, iden } };
		s_BaseNames.try_emplace(iden, qualName);
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

	QualName::QualName(QualNameSPtr base, IdenSPtr iden)
		: m_Base(base)
		, m_Iden(iden)
	{
	}

}
