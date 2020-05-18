#pragma once
#include "defs.hpp"
#include "type.hpp"

namespace Noctis
{
	struct Context;
	FWDECL_STRUCT_SPTR(Symbol);
	FWDECL_CLASS_SPTR(QualName);
	FWDECL_CLASS_SPTR(Iden);	
}

namespace Noctis::NameMangling
{
	StdString Mangle(Context* pCtx, SymbolSPtr sym);

	StdString Mangle(Context* pCtx, QualNameSPtr qualName);
	StdString Mangle(Context* pCtx, IdenSPtr iden);

	StdString Mangle(Context* pCtx, TypeHandle type);
	StdString Mangle(Context* pCtx, TypeSPtr type);


	QualNameSPtr DemangleQualName(Context* pCtx, StdStringView data);
	QualNameSPtr DemangleQualName(Context* pCtx, StdStringView data, usize& idx);
	IdenSPtr DemangleIden(Context* pCtx, StdStringView data, usize& idx);
	TypeHandle DemangleType(Context* pCtx, StdStringView data);
	TypeHandle DemangleType(Context* pCtx, StdStringView data, usize& idx);

	StdString DemangleLName(StdStringView data, usize& idx);
	
}