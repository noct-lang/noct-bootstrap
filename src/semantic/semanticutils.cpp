#include "semanticutils.hpp"
#include "semanticpass.hpp"
#include "ast/ast.hpp"

namespace Noctis
{
	StdUnorderedSet<QualNameSPtr> ExtractImportModules(AstTree& tree, Context* pCtx)
	{
		class ImportExtractor : public SemanticPass
		{
		public:
			ImportExtractor(Context* pCtx)
				: SemanticPass("Import Extractor", pCtx)
			{
			}
			
			void Visit(AstImportStmt& node) override
			{
				QualNameSPtr qualName = QualName::Create(node.moduleIdens);
				qualNames.insert(qualName);
			}

			StdUnorderedSet<QualNameSPtr> qualNames;
			
		} extractor{ pCtx };

		extractor.Process(tree);
		return extractor.qualNames;
	}
}
