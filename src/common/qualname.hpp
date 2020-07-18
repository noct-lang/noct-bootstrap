#pragma once
#include "common/defs.hpp"
#include "type.hpp"

namespace Noctis
{
	FWDECL_CLASS_SPTR(Iden);
	FWDECL_CLASS_SPTR(TypeDisambiguation);
	FWDECL_CLASS_SPTR(QualName);

	struct IdenGeneric
	{
		IdenGeneric();
		
		bool isType;
		bool isSpecialized;
		IdenSPtr iden;
		TypeHandle type;

		StdVector<TypeHandle> typeConstraints;
	};
	
	class Iden
	{
	public:

		static IdenSPtr Create(StdStringView name);
		static IdenSPtr Create(StdStringView name, u64 numGenerics);
		static IdenSPtr Create(StdStringView name, u64 numGenerics, const StdVector<StdString>& paramNames);
		static IdenSPtr Create(StdStringView name, const StdVector<IdenGeneric>& generics, TypeRegistry& typeReg);
		static IdenSPtr Create(StdStringView name, const StdVector<IdenGeneric>& generics, TypeRegistry& typeReg, const StdVector<StdString>& paramNames);

		const StdString& Name() { return m_Name; }
		StdVector<IdenGeneric>& Generics() { return m_Generics; }
		StdVector<StdString>& ParamNames() { return m_ParamNames; }

		StdString ToFuncSymName() const;
		StdString ToString() const;

	private:
		Iden(StdString name, u64 numGenerics);
		Iden(StdString name, StdVector<IdenGeneric> generics);
		
		StdString m_Name;
		StdVector<IdenGeneric> m_Generics;
		StdVector<StdString> m_ParamNames;

		static StdUnorderedMap<StdString, StdUnorderedMap<u64, StdVector<IdenSPtr>>> s_Idens;
		static StdString s_SearchString;
	};

	class TypeDisambiguation
	{
	public:
		static TypeDisambiguationSPtr Create(QualNameSPtr qualName, TypeHandle type);

		QualNameSPtr QualName() { return m_QualName; }
		TypeHandle Type() { return m_Type; }

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

		StdString ToString() const;
		StdVector<IdenSPtr> AllIdens();
		QualNameSPtr GetSubName(QualNameSPtr base);
		usize Depth();

		bool IsSubnameOf(QualNameSPtr base);

		IdenSPtr Iden() { return m_Iden; }
		const IdenSPtr Iden() const { return m_Iden; }
		QualNameSPtr Base() { return m_Base; }
		const QualNameSPtr Base() const { return m_Base; }
		TypeDisambiguationSPtr Disambiguation() { return m_Disambiguation; }
		
	private:
		QualName(QualNameSPtr base, IdenSPtr iden);
		QualName(TypeDisambiguationSPtr disambiguation);

		IdenSPtr m_Iden;
		QualNameSPtr m_Base;
		TypeDisambiguationSPtr m_Disambiguation;
		StdUnorderedMap<IdenSPtr, QualNameSPtr> m_Children;
		
		static StdUnorderedMap<IdenSPtr, QualNameSPtr> s_BaseNames;
		static StdUnorderedMap<TypeDisambiguationSPtr, QualNameSPtr> s_TypeDisambiguationBaseNames;
	};
	
}
