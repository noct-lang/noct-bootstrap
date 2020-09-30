#include "qualname.hpp"

#include <utility>

#include "utils.hpp"

namespace Noctis
{
	StdUnorderedMap<StdString, StdUnorderedMap<u64, StdVector<IdenSPtr>>> Iden::s_Idens = {};
	StdString Iden::s_SearchString = {};

	IdenGeneric::IdenGeneric()
		: isType(true)
		, isSpecialized(false)
	{
	}

	IdenSPtr Iden::Create(StdStringView name)
	{
		return Create(name, 0, StdVector<StdString>{});
	}

	IdenSPtr Iden::Create(StdStringView name, u64 numGenerics)
	{
		return Create(name, numGenerics, StdVector<StdString>{});
	}

	IdenSPtr Iden::Create(StdStringView name, u64 numGenerics, const StdVector<StdString>& paramNames)
	{
		// Use static string to as key, to try to decrease allocations
		s_SearchString.assign(name);
		auto it = s_Idens.find(s_SearchString);
		if (it == s_Idens.end())
			it = s_Idens.try_emplace(s_SearchString, StdUnorderedMap<u64, StdVector<IdenSPtr>>{}).first;

		auto subIt = it->second.find(numGenerics);
		if (subIt == it->second.end())
			subIt = it->second.try_emplace(numGenerics, StdVector<IdenSPtr>{}).first;

		for (IdenSPtr iden : subIt->second)
		{
			StdVector<StdString>& idenParamNames = iden->ParamNames();
			if (paramNames == idenParamNames)
				return iden;
		}

		IdenSPtr iden{ new Iden{ s_SearchString, numGenerics } };
		iden->m_ParamNames = paramNames;
		subIt->second.push_back(iden);
		return iden;
	}

	IdenSPtr Iden::Create(StdStringView name, const StdVector<IdenGeneric>& generics)
	{
		if (generics.empty())
			return Create(name);
		
		// Use static string to as key, to try to decrease allocations
		s_SearchString.assign(name);
		auto it = s_Idens.find(s_SearchString);
		if (it == s_Idens.end())
			it = s_Idens.try_emplace(s_SearchString, StdUnorderedMap<u64, StdVector<IdenSPtr>>{}).first;

		u64 numGenerics = u64(generics.size());
		
		auto subIt = it->second.find(numGenerics);
		if (subIt == it->second.end())
			subIt = it->second.try_emplace(0, StdVector<IdenSPtr>{}).first;

		for (IdenSPtr iden : subIt->second)
		{
			bool found = true;
			StdVector<IdenGeneric>& idenGens = iden->Generics();
			for (u64 i = 0; i < numGenerics; ++i)
			{
				if (idenGens[i].isSpecialized || generics[i].isSpecialized)
				{
					if (idenGens[i].isType != generics[i].isType ||
						!AreTypesEqual(idenGens[i].type, generics[i].type))
					{
						found = false;
						break;
					}
				}

				if (!idenGens[i].isType)
				{
					// TODO: Value
					
				}
			}

			if (found)
				return iden;
		}

		IdenSPtr iden{ new Iden{ s_SearchString, generics } };
		subIt->second.push_back(iden);
		return iden;
	}

	IdenSPtr Iden::Create(StdStringView name, const StdVector<IdenGeneric>& generics, const StdVector<StdString>& paramNames)
	{
		if (generics.empty())
			return Create(name, 0, paramNames);

		// Use static string to as key, to try to decrease allocations
		s_SearchString.assign(name);
		auto it = s_Idens.find(s_SearchString);
		if (it == s_Idens.end())
			it = s_Idens.try_emplace(s_SearchString, StdUnorderedMap<u64, StdVector<IdenSPtr>>{}).first;

		u64 numGenerics = u64(generics.size());

		auto subIt = it->second.find(numGenerics);
		if (subIt == it->second.end())
			subIt = it->second.try_emplace(0, StdVector<IdenSPtr>{}).first;

		for (IdenSPtr iden : subIt->second)
		{
			bool found = true;
			StdVector<IdenGeneric>& idenGens = iden->Generics();
			for (u64 i = 0; i < numGenerics; ++i)
			{
				if (idenGens[i].isSpecialized || generics[i].isSpecialized)
				{
					if (idenGens[i].isType != generics[i].isType ||
						!AreTypesEqual(idenGens[i].type, generics[i].type))
					{
						found = false;
						break;
					}
				}

				if (!idenGens[i].isType)
				{
					// TODO: Value

				}

				StdVector<StdString>& idenParamNames = iden->ParamNames();
				found &= paramNames == idenParamNames;
			}

			if (found)
				return iden;
		}

		IdenSPtr iden{ new Iden{ s_SearchString, generics } };
		iden->m_ParamNames = paramNames;
		subIt->second.push_back(iden);
		return iden;
	}

	StdString Iden::ToFuncSymName() const
	{
		StdString str = m_Name;
		for (const StdString& paramName : m_ParamNames)
		{
			str += "__" + paramName;
		}
		return str;
	}

	StdString Iden::ToString() const
	{
		StdString str = m_Name;

		if (!m_Generics.empty())
		{
			str += '<';
			for (usize i = 0; i < m_Generics.size(); ++i)
			{
				if (i != 0)
					str += ',';
				
				const IdenGeneric& generic = m_Generics[i];
				if (generic.isType)
				{
					// TODO
				}
				else
				{
					// TODO
					str += ':';
				}
			}
			str += '>';
		}

		if (!m_ParamNames.empty())
		{
			str += '(';
			for (usize i = 0; i < m_ParamNames.size(); ++i)
			{
				if (i != 0)
					str += ',';
				str += m_ParamNames[i];
			}
			str += ')';
		}

		return str;
	}

	Iden::Iden(StdString name, u64 numGenerics)
		: m_Name(std::move(name))
	{
		m_Generics.resize(numGenerics);
	}

	Iden::Iden(StdString name, StdVector<IdenGeneric> generics)
		: m_Name(std::move(name))
		, m_Generics(std::move(generics))
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
			tmp = Create(tmp, iden);
		}
		return tmp;
	}

	StdString QualName::ToString() const
	{
		StdString name;

		//if (m_Disambiguation)
		//	name += m_Disambiguation->

		for (usize i = 0; i < m_Idens.size(); ++i)
		{
			if (i != 0)
				name += "::";
			
			IdenSPtr iden = m_Idens[i];
			name += iden->Name();
		}
		return name;
	}

	QualNameSPtr QualName::GetSubName(QualNameSPtr base)
	{
		const StdVector<IdenSPtr>& ownIdens = Idens();
		const StdVector<IdenSPtr>& baseIdens = base->Idens();

		if (ownIdens.size() <= baseIdens.size())
			return Create(ownIdens);

		usize size = baseIdens.size();
		for (usize i = 0; i < size; ++i)
		{
			IdenSPtr ownIden = ownIdens[i];
			IdenSPtr baseIden = baseIdens[i];

			if (ownIden != baseIden)
				return nullptr;
		}

		StdVector<IdenSPtr> idens;
		idens.assign(ownIdens.begin() + size, ownIdens.end());
		return Create(idens);
	}

	QualNameSPtr QualName::GetSubName(usize depth)
	{
		if (depth > m_Idens.size())
			depth = m_Idens.size();
		
		StdVector<IdenSPtr> idens;
		idens.assign(m_Idens.end() - depth, m_Idens.end());
		return Create(idens);
	}

	bool QualName::IsBase()
	{
		return (!m_Disambiguation && m_Idens.size() == 1) ||
			(m_Disambiguation && m_Idens.empty());
	}

	bool QualName::IsSubnameOf(QualNameSPtr base)
	{
		QualName* qualName = this;
		usize depth = Depth();
		usize baseDepth = base->Depth();
		for (usize i = depth; i >= baseDepth; --i)
		{
			if (qualName == base.get())
				return true;

			qualName = qualName->Base().get();
		}
		return false;
	}

	QualName::QualName(QualNameSPtr base, IdenSPtr iden)
		: m_Base(std::move(base))
	{
		if (m_Base)
			m_Idens = m_Base->Idens();
		m_Idens.push_back(iden);
	}

	QualName::QualName(TypeDisambiguationSPtr disambiguation)
		: m_Disambiguation(std::move(disambiguation))
	{
	}
}
