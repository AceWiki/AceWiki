package ch.uzh.ifi.attempto.acewiki.core;

import java.util.List;

public abstract class AbstractModuleElement extends AbstractOntologyElement implements ModuleElement {

	private String mWord = "";

	public String[] getWords() {
		return new String[] {mWord};
	}

	public void setWords(String serializedWords) {
		String[] words = serializedWords.split(";");
		mWord = words[0];
	}

	public String serializeWords() {
		return mWord + ";";
	}

	public String getType() {
		return "Module";
	}

	public String getInternalType() {
		return "module";
	}

	// TODO: the following methods assume that a GF module is an article
	// which contains at most one Comment whose text is the module's GF source.
	public Comment getModuleContent() {
		List<Statement> statements = getArticle().getStatements();
		if (statements == null || statements.isEmpty() || ! (statements.get(0) instanceof Comment)) {
			return null;
		}
		return (Comment) statements.get(0);
	}

	public void replaceModuleContent(String newContent) {
		Statement newStatement = new Comment(newContent);
		newStatement.init(getOntology(), getArticle());
		List<Statement> statements = getArticle().getStatements();
		if (statements == null || statements.isEmpty()) {
			getArticle().add(null, newStatement);
		} else {
			getArticle().edit(statements.get(0), newStatement);
		}
	}

}
