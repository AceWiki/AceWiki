// This file is part of AceWiki.
// Copyright 2008-2012, AceWiki developers.
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

package ch.uzh.ifi.attempto.acewiki.core;

import java.util.Locale;

import ch.uzh.ifi.attempto.base.LocaleResources;
import ch.uzh.ifi.attempto.echocomp.EchoThread;

/**
 * This class represents a topic ontology element, which cannot be part of ontological statements
 * but represents the content of an article. The main page is represented by a special topic type.
 * 
 * @author Tobias Kuhn
 * @author Kaarel Kaljurand
 */
public class GeneralTopic extends AbstractOntologyElement implements TopicElement {

	public static final String MAINPAGE_TYPE = "mainpage";
	public static final String NORMAL_TYPE = "article";

	private final String type;
	private String text;

	/**
	 * Creates a new main page topic.
	 * 
	 * @param text The text, for example "Main Page".
	 * @return The topic object.
	 */
	public static final GeneralTopic makeMain(String text) {
		return new GeneralTopic(MAINPAGE_TYPE, text);
	}

	/**
	 * Creates a general topic.
	 * 
	 * @param text The name of the topic.
	 * @return The topic object.
	 */
	public static final GeneralTopic makeNormal(String text) {
		return new GeneralTopic(NORMAL_TYPE, text);
	}

	/**
	 * Creates a new topic ontology element.
	 * 
	 * @param type The internal type.
	 * @param text The text or text key.
	 */
	private GeneralTopic(String type, String text) {
		this.type = type;
		this.text = text;
	}
	
	public String[] getWords() {
		return new String[] { text };
	}
	
	public String[] getHeadwords() {
		return new String[] { getLocalized(text) };
	}

	public void setWords(String serializedWords) {
		String[] words = serializedWords.split(";");
		text = words[0];
	}

	public String serializeWords() {
		return text + ";";
	}
	
	public String getType() {
		return "Topic";
	}
	
	public String getInternalType() {
		return type;
	}

	private String getLocalized(String s) {
		Locale locale = EchoThread.getActiveApplication().getLocale();
		String l = LocaleResources.getString(locale, s);
		if (l == null) l = s;
		return l;
	}

}
