#pragma once
#include "semantic/semantic-pass.hpp"
#include "tokens/span.hpp"

namespace Noctis
{
	enum class Attribute : u8;

	class SimpleAttributePass : public ITrSemanticPass
	{
	public:
		SimpleAttributePass();
		
		void Process(ITrModule& mod) override;
		
		void Visit(ITrLocalVar& node) override;
		void Visit(ITrType& node) override;

		void CheckAttribs(ITrDef& node, Attribute validMask, StdStringView defName);
		void CheckAttribs(Attribute attribs, Attribute validMask, SpanId spanId, StdStringView defName);
		void CheckSingleAttrib(Attribute attribs, Attribute singleMask, SpanId spanId, StdStringView defName);
	};

	/*class AttribSymAssignPass : public ITrSemanticPass
	{
	public:
		
	};*/
	
}
