// This file is part of AceWiki.
// Copyright 2008-2011, Tobias Kuhn.
// 
// AceWiki is free software: you can redistribute it and/or modify it under the terms of the GNU
// Lesser General Public License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
// 
// AceWiki is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
// even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General Public License along with AceWiki. If
// not, see http://www.gnu.org/licenses/.

package ch.uzh.ifi.attempto.acewiki.aceowl;

/**
 * This file transforms from OWL 1.1 into OWL 2. Furthermore, some OWL structures are rewritten.
 * This class becomes obsolete once the changes are implemented in the ACE parser.
 * 
 * @author Tobias Kuhn
 */
class OWLXMLTransformer {
	
	// no instances allowed
	private OWLXMLTransformer() {}
	
	/**
	 * Transforms the given input in OWL 1.1 XML format into OWL 2 and applies some rewriting
	 * rules.
	 * 
	 * @param in The input string in OWL 1.1 XML format.
	 * @return The output string in OWL 2 XML format.
	 */
	public static String transform(String in) {
		String out = in;
		
		// Transform from OWL 1.1 to OWL 2:
		out = out.replaceAll("http://www\\.w3\\.org/2006/12/owl11-xml#", "http://www.w3.org/2002/07/owl#");
		out = out.replaceAll("InverseObjectProperty>", "ObjectInverseOf>");
		out = out.replaceAll("SubObjectPropertyChain>", "ObjectPropertyChain>");
		out = out.replaceAll("ObjectExistsSelf>", "ObjectHasSelf>");
		out = out.replaceAll("(\\s)URI=\"", "$1IRI=\"");
		
		// Rewrite as ObjectPropertyDomain axiom when possible:
		out = out.replaceAll(
				"( *)<SubClassOf>\\s*" +
					"<ObjectIntersectionOf>\\s*" +
						"<Class\\s*IRI=\"http://www\\.w3\\.org/2002/07/owl#Thing\"/>\\s*" +
						"<ObjectSomeValuesFrom>\\s*" +
							"(<ObjectProperty\\s*IRI=\"[^\"]+\"/>)\\s*" +
							"<Class\\s*IRI=\"http://www\\.w3\\.org/2002/07/owl#Thing\"/>\\s*" +
						"</ObjectSomeValuesFrom>\\s*" +
					"</ObjectIntersectionOf>\\s*" +
					"(<Class\\s*IRI=\"[^\"]+\"/>)\\s*" +
				"</SubClassOf>"
				,
				"$1<ObjectPropertyDomain>\n" +
				"$1$1$2\n" +
				"$1$1$3\n" +
				"$1</ObjectPropertyDomain>"
			);

		// Rewrite as ObjectPropertyRange axiom when possible:
		out = out.replaceAll(
				"( *)<SubClassOf>\\s*" +
					"<ObjectIntersectionOf>\\s*" +
						"<Class\\s*IRI=\"http://www\\.w3\\.org/2002/07/owl#Thing\"/>\\s*" +
						"<ObjectSomeValuesFrom>\\s*" +
							"<ObjectInverseOf>\\s*" +
								"(<ObjectProperty\\s*IRI=\"[^\"]+\"/>)\\s*" +
							"</ObjectInverseOf>\\s*" +
							"<Class\\s*IRI=\"http://www\\.w3\\.org/2002/07/owl#Thing\"/>\\s*" +
						"</ObjectSomeValuesFrom>\\s*" +
					"</ObjectIntersectionOf>\\s*" +
					"(<Class\\s*IRI=\"[^\"]+\"/>)\\s*" +
				"</SubClassOf>"
				,
				"$1<ObjectPropertyRange>\n" +
				"$1$1$2\n" +
				"$1$1$3\n" +
				"$1</ObjectPropertyRange>"
			);
		
		return out;
	}

}
