#pragma once
#include "defs.hpp"
#include "qualname.hpp"
#include "type.hpp"

namespace Noctis
{
	struct Context;
	FWDECL_STRUCT_SPTR(Symbol);
	FWDECL_CLASS_SPTR(QualName);
	FWDECL_CLASS_SPTR(TypeDisambiguation);	
}

namespace Noctis::NameMangling
{
	StdString Mangle(SymbolSPtr sym);

	StdString Mangle(QualNameSPtr qualName, BoundsInfo* pBoundsInfo);
	StdString Mangle(StdVector<IdenGeneric>& idenGens, BoundsInfo* pBoundsInfo);

	StdString Mangle(const StdString& iden);

	StdString Mangle(TypeHandle type);
	StdString Mangle(TypeSPtr type);


	QualNameSPtr DemangleQualName(StdStringView data, BoundsInfo* pBoundsInfo);
	QualNameSPtr DemangleQualName(StdStringView data, usize& idx, BoundsInfo* pBoundsInfo);
	TypeHandle DemangleType(StdStringView data);
	TypeHandle DemangleType(StdStringView data, usize& idx);

	StdVector<IdenGeneric> DemangledGenerics(StdStringView data, usize& idx, BoundsInfo* pBoundsInfo);

	usize DemangleUSize(StdStringView data, usize& idx);
	StdString DemangleLName(StdStringView data, usize& idx);
	
}