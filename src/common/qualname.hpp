#pragma once
#include "common/defs.hpp"
#include "type.hpp"

namespace Noctis
{
	FWDECL_CLASS_SPTR(Iden);
	FWDECL_CLASS_WPTR(Iden);
	FWDECL_CLASS_SPTR(TypeDisambiguation);
	FWDECL_CLASS_SPTR(QualName);
	FWDECL_CLASS_WPTR(QualName);

	struct IdenGeneric
	{
		IdenGeneric();
		
		bool isType;
		bool isSpecialized;
		IdenSPtr iden;
		TypeHandle type;

		ITrExprSPtr itrExpr;
	};
	
	class Iden
	{
	public:
		static IdenSPtr Create(StdStringView name);
		static IdenSPtr Create(StdStringView name, const StdVector<IdenGeneric>& generics);

		const StdString& Name() const { return m_Name; }
		StdVector<IdenGeneric>& Generics() { return m_Generics; }
		IdenSPtr Fuzzy() { return fuzzy.lock(); }

		StdString ToString() const;

	private:
		Iden(StdString name, StdVector<IdenGeneric> generics);

		static IdenSPtr CreateFuzzy(StdStringView name, const StdVector<IdenGeneric>& generics);
		
		StdString m_Name;
		StdVector<IdenGeneric> m_Generics;

		IdenWPtr fuzzy;

		static StdUnorderedMap<StdString, StdUnorderedMap<u64, StdVector<IdenSPtr>>> s_Idens;
		static StdUnorderedMap<StdString, StdUnorderedMap<u64, StdVector<IdenSPtr>>> s_FuzzyIdens;
		static StdString s_SearchString;
	};

	class TypeDisambiguation
	{
	public:
		static TypeDisambiguationSPtr Create(TypeHandle type, QualNameSPtr ifaceQualName);

		TypeHandle Type() { return m_Type; }
		QualNameSPtr IfaceQualName() { return m_QualName; }

	private:
		TypeDisambiguation(TypeHandle type, QualNameSPtr qualName);
		
		TypeHandle m_Type;
		QualNameSPtr m_QualName;

		static StdUnorderedMap<TypeHandle, StdUnorderedMap<QualNameSPtr, TypeDisambiguationSPtr>> s_TypeDisambiguations;
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

		StdString ToString() const;
		QualNameSPtr GetSubName(QualNameSPtr base);
		QualNameSPtr GetSubName(usize depth);

		QualNameSPtr GetBaseName(usize depth);
		 
		bool IsBase();
		bool IsSubnameOf(QualNameSPtr base);

		TypeDisambiguationSPtr Disambiguation() { return m_Disambiguation; }
		IdenSPtr LastIden() { return m_Idens.back(); }
		const IdenSPtr LastIden() const { return m_Idens.back(); }
		const StdVector<IdenSPtr>& Idens() { return m_Idens; }
		usize Depth() { return m_Idens.size(); }

		QualNameSPtr Base() { return m_Base; }
		const QualNameSPtr Base() const { return m_Base; }

		QualNameSPtr Fuzzy() { return m_Fuzzy.lock(); }

	private:
		QualName(QualNameSPtr base, IdenSPtr iden);
		QualName(TypeDisambiguationSPtr disambiguation);
		
		TypeDisambiguationSPtr m_Disambiguation;
		StdVector<IdenSPtr> m_Idens;
		QualNameWPtr m_Fuzzy;
		
		QualNameSPtr m_Base;
		StdUnorderedMap<IdenSPtr, QualNameSPtr> m_Children;
		
		static StdUnorderedMap<IdenSPtr, QualNameSPtr> s_BaseNames;
		static StdUnorderedMap<TypeDisambiguationSPtr, QualNameSPtr> s_TypeDisambiguationBaseNames;
	};
	
}
