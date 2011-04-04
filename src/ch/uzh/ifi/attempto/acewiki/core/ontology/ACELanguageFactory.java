package ch.uzh.ifi.attempto.acewiki.core.ontology;

public class ACELanguageFactory implements LanguageFactory {

	public OntologyElement createOntologyElement(String type) {
		if (type.equals("propername")) {
			return new Individual();
		} else if (type.equals("noun")) {
			return new NounConcept();
		} else if (type.equals("nounof")) {
			return new OfRole();
		} else if (type.equals("trverb")) {
			return new VerbRole();
		} else if (type.equals("tradj")) {
			return new TrAdjRole();
		}
		return null;
	}
	
	public Sentence createSentence(String text) {
		// remove leading and trailing blank spaces.
		text = text.replaceFirst("^\\s+", "").replaceFirst("\\s+$", "");
		if (text.substring(text.length()-1).equals("?")) {
			return new Question(text);
		} else {
			return new Declaration(text);
		}
	}

}
