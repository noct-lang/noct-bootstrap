#pragma once
#include "semantic/semantic-pass.hpp"

namespace Noctis
{

	class DeclMacroContextGen : public AstSemanticPass
	{
	public:
		DeclMacroContextGen(Context* pCtx);

		void Visit(AstDeclSPtr& node) override;
	};
	
}
