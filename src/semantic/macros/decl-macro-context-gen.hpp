#pragma once
#include "semantic/semantic-pass.hpp"

namespace Noctis
{

	class DeclMacroContextGen : public SemanticPass
	{
	public:
		DeclMacroContextGen(Context* pCtx);

		void Visit(AstDeclSPtr node) override;
	};
	
}
