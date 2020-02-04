#pragma once
#include "common/defs.hpp"

namespace Noctis
{
	class Iden;
	using IdenSPtr = std::shared_ptr<Iden>;
	class Iden
	{
	public:

		static IdenSPtr Create(StdStringView name);

		const StdString& Name() { return m_Name; }
 
	private:
		Iden(StdString name);
		
		StdString m_Name;


		static StdUnorderedMap<StdString, IdenSPtr> s_Idens;
		static StdString s_SearchString;
	};

	class QualName;
	using QualNameSPtr = std::shared_ptr<QualName>;
	class QualName
	{
	public:
		static QualNameSPtr Create(IdenSPtr iden);
		static QualNameSPtr Create(QualNameSPtr base, IdenSPtr iden);
		static QualNameSPtr Create(QualNameSPtr base, StdStringView name);
		static QualNameSPtr Create(const StdVector<StdString>& names);

		StdString ToString();

		IdenSPtr Iden() { return m_Iden; };
		QualNameSPtr Base() { return m_Base; }
		
	private:
		QualName(QualNameSPtr base, IdenSPtr iden);

		QualNameSPtr m_Base;
		IdenSPtr m_Iden;
		StdUnorderedMap<IdenSPtr, QualNameSPtr> m_Children;
		
		static StdUnorderedMap<IdenSPtr, QualNameSPtr> s_BaseNames;
	};
	
}
