#include "semantic-utils.hpp"
#include "semantic-pass.hpp"
#include "ast/ast.hpp"

namespace Noctis
{
	StdUnorderedSet<QualNameSPtr> ExtractImportModules(AstTree& tree, Context* pCtx)
	{
		class ImportExtractor : public AstSemanticPass
		{
		public:
			ImportExtractor(Context* pCtx)
				: AstSemanticPass("import extractor", pCtx)
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
