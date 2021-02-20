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
	StdString Mangle(Context* pCtx, SymbolSPtr sym);

	StdString Mangle(Context* pCtx, QualNameSPtr qualName, BoundsInfo* pBoundsInfo);
	StdString Mangle(Context* pCtx, StdVector<IdenGeneric>& idenGens, BoundsInfo* pBoundsInfo);

	StdString Mangle(const StdString& iden);

	StdString Mangle(Context* pCtx, TypeHandle type);
	StdString Mangle(Context* pCtx, TypeSPtr type);


	QualNameSPtr DemangleQualName(Context* pCtx, StdStringView data, BoundsInfo* pBoundsInfo);
	QualNameSPtr DemangleQualName(Context* pCtx, StdStringView data, usize& idx, BoundsInfo* pBoundsInfo);
	TypeHandle DemangleType(Context* pCtx, StdStringView data);
	TypeHandle DemangleType(Context* pCtx, StdStringView data, usize& idx);

	StdVector<IdenGeneric> DemangledGenerics(Context* pCtx, StdStringView data, usize& idx, BoundsInfo* pBoundsInfo);

	usize DemangleUSize(StdStringView data, usize& idx);
	StdString DemangleLName(StdStringView data, usize& idx);
	
}