#pragma once
#include "semantic/semantic-pass.hpp"

namespace Noctis
{

	class DeclMacroContextGen : public AstSemanticPass
	{
	public:
		DeclMacroContextGen();

		void Visit(AstDeclSPtr& node) override;
	};
	
}
