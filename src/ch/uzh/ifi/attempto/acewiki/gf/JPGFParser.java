// This file is part of AceWiki.
// Copyright 2008-2011, AceWiki developers.
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
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.grammaticalframework.PGF;
import org.grammaticalframework.PGFBuilder;
import org.grammaticalframework.Parser;
import org.grammaticalframework.parser.ParseState;

import ch.uzh.ifi.attempto.base.ConcreteOption;
import ch.uzh.ifi.attempto.base.NextTokenOptions;
import ch.uzh.ifi.attempto.base.PredictiveParser;
import ch.uzh.ifi.attempto.base.SimpleConcreteOption;
import ch.uzh.ifi.attempto.base.SimpleNextTokenOptions;

/**
 * This is a predictive parser connecting to the JPGF implementation of GF.
 * 
 * @author Tobias Kuhn
 */
public class JPGFParser implements PredictiveParser {
	
	private List<String> tokens = new ArrayList<String>();
	private ParseState parseState;
	private NextTokenOptions nextTokenOptions;
	private Parser gfParser;
//	private Linearizer gfLinearizer;
	
	/**
	 * Creates a new parser object for the given pgf file and language.
	 * 
	 * @param pgfFile The pgf file containing the language definition.
	 * @param language The language to be parsed as defined in the pgf file.
	 */
	public JPGFParser(String pgfFile, String language) {
		try {
			ClassLoader cl = Thread.currentThread().getContextClassLoader();
			InputStream in = cl.getResourceAsStream(pgfFile);
			PGF pgf = PGFBuilder.fromInputStream(in);
			gfParser = new Parser(pgf, language);
//			gfLinearizer = new Linearizer(pgf, language);
		} catch (Exception ex) {
			ex.printStackTrace();
		}
		update();
	}
	
	private ParseState getParseState() {
		if (parseState == null) {
			parseState = gfParser.parse(getTokens().toArray(new String[] {}));
		}
		return parseState;
	}
	
	private void update() {
		// lazy parsing
		parseState = null;
		nextTokenOptions = null;
	}
	
	public void addToken(String token) {
		tokens.add(token);
		update();
	}
	
	public void addTokens(List<String> tokens) {
		this.tokens.addAll(tokens);
		update();
	}
	
	public void removeToken() {
		tokens.remove(tokens.size()-1);
		update();
	}
	
	public void removeAllTokens() {
		tokens.clear();
		update();
	}
	
	public void setTokens(List<String> tokens) {
		this.tokens.clear();
		this.tokens.addAll(tokens);
		update();
	}
	
	public List<String> getTokens() {
		return tokens;
	}
	
	public int getTokenCount() {
		return tokens.size();
	}
	
	public NextTokenOptions getNextTokenOptions() {
		if (nextTokenOptions == null) {
			Set<ConcreteOption> options = new HashSet<ConcreteOption>();
			for (String s : getParseState().predict()) {
				options.add(new SimpleConcreteOption(s));
			}
			nextTokenOptions = new SimpleNextTokenOptions(options);
		}
		return nextTokenOptions;
	}
	
	public boolean isPossibleNextToken(String token) {
		return getNextTokenOptions().containsToken(token);
	}
	
	public boolean isComplete() {
		//System.err.println(getLin());
		return getParseState().getTrees().length > 0;
	}
	
//	private String getLin() {
//		String s = null;
//		try {
//			s = gfLinearizer.linearizeString(getParseState().getTrees()[0]);
//		} catch (LinearizerException ex) {
//			ex.printStackTrace();
//		}
//		return s;
//	}
	
	public int getReference() {
		return -1;
	}

}
