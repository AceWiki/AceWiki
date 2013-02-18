package ch.uzh.ifi.attempto.acewiki.gf;

import com.google.common.base.Strings;

public class GfWikiEntry {

	final private String mLanguage;
	final private String mText;
	final private TreeList mTrees;

	public GfWikiEntry(String language, String text, TreeList trees) {
		mLanguage = Strings.emptyToNull(language);
		mText = Strings.emptyToNull(text);
		mTrees = trees;
	}

	public GfWikiEntry(TreeList trees) {
		this(null, null, trees);
	}

	public String getLanguage() {
		return mLanguage;
	}

	public String getText() {
		return mText;
	}

	public TreeList getTrees() {
		return mTrees;
	}

}