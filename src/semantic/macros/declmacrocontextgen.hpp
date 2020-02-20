#pragma once
#include "semantic/semanticpass.hpp"

namespace Noctis
{

	class DeclMacroContextGen : public SemanticPass
	{
	public:
		DeclMacroContextGen(Context* pCtx);

		void Visit(AstDeclSPtr node) override;
	};
	
}
