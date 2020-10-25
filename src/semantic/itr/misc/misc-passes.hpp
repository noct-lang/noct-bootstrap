#pragma once
#include "semantic/semantic-pass.hpp"

namespace Noctis
{
	enum class Attribute : u8;
	FWDECL_STRUCT_SPTR(Symbol);
	
	class NameManglePass : public ITrSemanticPass
	{
	public:
		NameManglePass(Context* pCtx);
		
		void Process(ITrModule& mod) override;
	};

	class MarkingPass : public ITrSemanticPass
	{
	public:
		MarkingPass(Context* pCtx);

		void Process(ITrModule& mod) override;

		void Visit(ITrStruct& node) override;
		void Visit(ITrUnion& node) override;
		void Visit(ITrValEnum& node) override;
		void Visit(ITrAdtEnum& node) override;
		void Visit(ITrVar& node) override;
		void Visit(ITrFunc& node) override;
		
	private:

		void Mark(ITrDef& def, Attribute attribs);

		bool m_CompileTimeOnly;
		bool m_DependsOnValueGenerics;
	};
	
}
