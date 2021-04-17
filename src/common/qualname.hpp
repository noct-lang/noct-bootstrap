#pragma once
#include "common/defs.hpp"
#include "type.hpp"

namespace Noctis
{
	FWDECL_CLASS_SPTR(TypeDisambiguation);
	FWDECL_CLASS_SPTR(QualName);
	FWDECL_CLASS_WPTR(QualName);

	struct IdenGeneric
	{
		IdenGeneric();
		
		bool isType;
		bool isSpecialized;
		StdString iden;
		TypeHandle type;

		ITrExprSPtr itrExpr;
	};
	
	class TypeDisambiguation
	{
	public:
		static TypeDisambiguationSPtr Create(TypeHandle type, QualNameSPtr ifaceQualName);

		TypeHandle Type() { return m_Type; }
		QualNameSPtr IfaceQualName() { return m_QualName; }
		StdString ToString();

	private:
		TypeDisambiguation(TypeHandle type, QualNameSPtr qualName);
		
		TypeHandle m_Type;
		QualNameSPtr m_QualName;

		static StdUnorderedMap<TypeHandle, StdUnorderedMap<QualNameSPtr, TypeDisambiguationSPtr>> s_TypeDisambiguations;
	};
	
	class QualName
	{
	public:
		static QualNameSPtr Create(const StdString& iden);
		static QualNameSPtr Create(const StdString& iden, const StdVector<IdenGeneric>& generics);
		static QualNameSPtr Create(const StdVector<StdString>& idens);
		static QualNameSPtr Create(const StdVector<StdString>& idens, const StdVector<IdenGeneric>& generics);
		static QualNameSPtr Create(TypeDisambiguationSPtr disambig);

		QualNameSPtr Append(QualNameSPtr qualName);
		QualNameSPtr Append(const StdString& iden);
		QualNameSPtr Append(const StdString& iden, const StdVector<IdenGeneric>& generics);
		QualNameSPtr Append(const StdVector<StdString>& idens);
		QualNameSPtr Append(const StdVector<StdString>& idens, const StdVector<IdenGeneric>& generics);

		QualNameSPtr AppendLastIden(QualNameSPtr qualName);

		QualNameSPtr WithGenerics(const StdVector<IdenGeneric>& generics);

		StdString ToString() const;
		QualNameSPtr GetSubName(QualNameSPtr base);
		QualNameSPtr GetSubName(usize depth, usize numGenerics);

		QualNameSPtr GetBaseName(usize depth);
		//QualNameSPtr GetBaseName
		 
		bool IsBase();
		bool IsSubnameOf(QualNameSPtr base);

		TypeDisambiguationSPtr Disambiguation() { return m_Disambiguation; }
		StdString& LastIden() { return m_Idens.back(); }
		const StdString& LastIden() const { return m_Idens.back(); }
		const StdVector<StdString>& Idens() { return m_Idens; }
		usize Depth() { return m_Idens.size(); }
		
		StdVector<IdenGeneric>& Generics() { return m_Generics; }
		bool IsGeneric() const { return !m_Generics.empty(); }

		QualNameSPtr Base(usize numGenerics = 0);

		QualNameSPtr Fuzzy() { return m_Fuzzy.lock(); }

	private:
		QualName(QualNameSPtr base, StdString iden);
		QualName(TypeDisambiguationSPtr disambiguation);

		static bool CompareGenerics(const StdVector<IdenGeneric>& gens0, const StdVector<IdenGeneric>& gens1);
		static void FixupGenerics(StdVector<IdenGeneric>& idenGens);
		static QualNameSPtr CreateFuzzy(TypeDisambiguationSPtr disambig, const StdVector<StdString>& idens, const StdVector<IdenGeneric>& generics);
		
		TypeDisambiguationSPtr m_Disambiguation;
		StdVector<StdString> m_Idens;
		QualNameWPtr m_Fuzzy;

		StdVector<IdenGeneric> m_Generics;
		
		QualNameWPtr m_Self;
		QualNameWPtr m_NoGenSelf;
		QualNameSPtr m_Base;
		StdUnorderedMap<StdString, StdVector<QualNameSPtr>> m_Children;
		
		static StdUnorderedMap<StdString, StdVector<QualNameSPtr>> s_BaseNames;
		static StdUnorderedMap<TypeDisambiguationSPtr, QualNameSPtr> s_TypeDisambiguationBaseNames;
	};
	
}