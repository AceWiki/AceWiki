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

package ch.uzh.ifi.attempto.acewiki.gf;

import java.io.InputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.grammaticalframework.Linearizer;
import org.grammaticalframework.PGF;
import org.grammaticalframework.PGFBuilder;
import org.grammaticalframework.Parser;
import org.grammaticalframework.parser.ParseState;

/**
 * This class wraps GF features of a particular GF grammar.
 * 
 * @author Tobias Kuhn
 */
public class GFGrammar {
	
	private String fileName;
	private String serializationLanguage;
	private PGF pgf;
	private Map<String, Parser> parsers = new HashMap<String, Parser>();
	private Map<String, Linearizer> linearizers = new HashMap<String, Linearizer>();
	
	/**
	 * Creates a new GF grammar object.
	 * 
	 * @param pgfFile Path and name of the pgf file.
	 * @param serializationLanguage The language used for serialization.
	 */
	public GFGrammar(String pgfFile, String serializationLanguage) {
		if (!pgfFile.matches("^.*/[^\\/]+.pgf$")) {
			throw new RuntimeException("Illegal pgf filename");
		}
		fileName = pgfFile.replaceFirst("^.*/([^\\/]+).pgf$", "$1");
		this.serializationLanguage = serializationLanguage;
		try {
			ClassLoader cl = Thread.currentThread().getContextClassLoader();
			InputStream in = cl.getResourceAsStream(pgfFile);
			pgf = PGFBuilder.fromInputStream(in);
		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}
	
	private Parser getGFParser(String language) {
		Parser p = parsers.get(language);
		if (p == null) {
			try {
				p = new Parser(pgf, fileName + language);
			} catch (Exception ex) {
				ex.printStackTrace();
			}
			parsers.put(language, p);
		}
		return p;
	}
	
	/**
	 * Parses the given text in the given language.
	 * 
	 * @param text The text.
	 * @param language The language.
	 * @return The parse result.
	 */
	public ParseState parse(String text, String language) {
		return getGFParser(language).parse(text);
	}

	/**
	 * Parses the given tokens in the given language.
	 * 
	 * @param tokens The tokens.
	 * @param language The language.
	 * @return The parse result.
	 */
	public ParseState parse(String[] tokens, String language) {
		return getGFParser(language).parse(tokens);
	}
	
	/**
	 * Deserializes a serialized representation into a parse state.
	 * 
	 * @param serialized The serialized representation
	 * @return The parse state.
	 */
	public ParseState deserialize(String serialized) {
		return parse(serialized, serializationLanguage);
	}
	
	private Linearizer getGFLinearizer(String language) {
		Linearizer l = linearizers.get(language);
		if (l == null) {
			try {
				l = new Linearizer(pgf, fileName + language);
			} catch (Exception ex) {
				ex.printStackTrace();
			}
			linearizers.put(language, l);
		}
		return l;
	}
	
	/**
	 * Linearizes a parse state in the given language.
	 * 
	 * @param parseState The parse state.
	 * @param language The language.
	 * @return The linearization as a string.
	 */
	public String linearizeAsString(ParseState parseState, String language) {
		try {
			return getGFLinearizer(language).linearizeString(parseState.getTrees()[0]);
		} catch (Exception ex) {
			ex.printStackTrace();
		}
		return null;
	}

	/**
	 * Linearizes a parse state in the given language.
	 * 
	 * @param parseState The parse state.
	 * @param language The language.
	 * @return The linearization as a list of tokens.
	 */
	public List<String> linearizeAsTokens(ParseState parseState, String language) {
		try {
			return getGFLinearizer(language).linearizeTokens(parseState.getTrees()[0]);
		} catch (Exception ex) {
			ex.printStackTrace();
		}
		return null;
	}
	
	/**
	 * Serializes a given parse state.
	 * 
	 * @param parseState The parse state.
	 * @return The serialization.
	 */
	public String serialize(ParseState parseState) {
		return linearizeAsString(parseState, serializationLanguage);
	}

}
