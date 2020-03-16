#pragma once
#include "common/defs.hpp"
#include "type.hpp"

namespace Noctis
{
	FWDECL_CLASS_SPTR(Iden);
	FWDECL_CLASS_SPTR(TypeDisambiguation);
	FWDECL_CLASS_SPTR(QualName);
	
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

	class TypeDisambiguation
	{
	public:
		static TypeDisambiguationSPtr Create(QualNameSPtr qualName, TypeHandle type);


	private:
		TypeDisambiguation(QualNameSPtr qualName, TypeHandle type);
		
		QualNameSPtr m_QualName;
		TypeHandle m_Type;

		static StdUnorderedMap<QualNameSPtr, StdUnorderedMap<TypeHandle, TypeDisambiguationSPtr>> m_sTypeDisambiguations;
	};
	
	class QualName
	{
	public:
		static QualNameSPtr Create(IdenSPtr iden);
		static QualNameSPtr Create(TypeDisambiguationSPtr disambiguation);
		
		static QualNameSPtr Create(QualNameSPtr base, IdenSPtr iden);
		static QualNameSPtr Create(QualNameSPtr base, StdStringView name);
		static QualNameSPtr Create(const StdVector<StdString>& names);
		static QualNameSPtr Create(const StdVector<IdenSPtr>& idens);
		static QualNameSPtr Create(QualNameSPtr first, const StdVector<IdenSPtr>& idens);

		StdString ToString();
		StdVector<IdenSPtr> AllIdens();

		IdenSPtr Iden() { return m_Iden; };
		QualNameSPtr Base() { return m_Base; }
		TypeDisambiguationSPtr Disambiguation() { return m_Disambiguation; }
		
	private:
		QualName(QualNameSPtr base, IdenSPtr iden);
		QualName(TypeDisambiguationSPtr disambiguation);

		TypeDisambiguationSPtr m_Disambiguation;
		QualNameSPtr m_Base;
		IdenSPtr m_Iden;
		StdUnorderedMap<IdenSPtr, QualNameSPtr> m_Children;
		
		static StdUnorderedMap<IdenSPtr, QualNameSPtr> s_BaseNames;
		static StdUnorderedMap<TypeDisambiguationSPtr, QualNameSPtr> s_TypeDisambiguationBaseNames;
	};
	
}
