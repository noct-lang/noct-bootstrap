#pragma once
#include "semantic/semantic-pass.hpp"
#include "common/qualname.hpp"
#include "tokens/span.hpp"

namespace Noctis
{
	FWDECL_STRUCT_SPTR(Symbol);
	FWDECL_STRUCT_SPTR(ITrGenDecl);

	class TypealiasReplacing : public ITrSemanticPass
	{
	public:
		TypealiasReplacing(Context* pCtx);

		void Process(ITrModule& mod) override;
	};

	class InterfaceResolve : public ITrSemanticPass
	{
	public:
		InterfaceResolve(Context* pCtx);

		void Process(ITrModule& mod) override;

		IdenGeneric GetGeneric(ITrGenDeclSPtr genDecl, IdenGeneric origGeneric, StdUnorderedMap<StdString, TypeHandle>& genMapping);
	};
	
}
