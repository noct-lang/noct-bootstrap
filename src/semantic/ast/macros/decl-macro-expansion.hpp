#pragma once
#include "semantic/semantic-pass.hpp"

namespace Noctis
{
	struct Context;

	class DeclMacroExpansion : public AstSemanticPass
	{
	public:
		DeclMacroExpansion();

		void Visit(AstMacroInstStmt& node) override;
		void Visit(AstMacroInstExpr& node) override;
		
	};
	
}
