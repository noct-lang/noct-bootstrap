#include "semantic-utils.hpp"
#include "semantic-pass.hpp"
#include "ast/ast.hpp"

namespace Noctis
{
	StdUnorderedSet<QualNameSPtr> ExtractImportModules(AstTree& tree)
	{
		class ImportExtractor : public AstSemanticPass
		{
		public:
			ImportExtractor()
				: AstSemanticPass("import extractor")
			{
			}
			
			void Visit(AstImportStmt& node) override
			{
				QualNameSPtr qualName = QualName::Create(node.moduleIdens);
				qualNames.insert(qualName);
			}

			StdUnorderedSet<QualNameSPtr> qualNames;
			
		} extractor;

		extractor.Process(tree);
		return extractor.qualNames;
	}
}
