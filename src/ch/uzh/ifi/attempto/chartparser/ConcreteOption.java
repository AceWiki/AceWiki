package ch.uzh.ifi.attempto.chartparser;

import java.util.ArrayList;
import java.util.List;

/**
 * This class represents an option (in a concrete way) how a partial sentence can be continued
 * according to given grammar. Such a concrete option consists of a word in the form of a
 * terminal category standing for a possible next token, and optionally of a pre-terminal category
 * from which the terminal category was derived.
 * 
 * @see AbstractOption
 * @see NextTokenOptions
 * @author Tobias Kuhn
 */
public class ConcreteOption {
	
	private final Terminal word;
	private final Preterminal category;
	private final String identifier;
	
	ConcreteOption(Grammar grammar, Terminal word, Preterminal category) {
		this.word = word;
		this.category = category;
		identifier = calculateIdentifier(grammar.getUsedFeatureNames());
	}
	
	ConcreteOption(Grammar grammar, LexicalRule lexRule) {
		this(grammar, lexRule.getWord(), lexRule.getCategory());
	}
	
	/**
	 * Returns the word of this concrete option.
	 * 
	 * @return The word in the form of a terminal category.
	 */
	public Terminal getWord() {
		return word;
	}
	
	/**
	 * Returns the pre-terminal category of this concrete option, or null if no pre-terminal
	 * category was involved.
	 * 
	 * @return The pre-terminal category.
	 */
	public Preterminal getCategory() {
		return category;
	}
	
	String calculateIdentifier(String[] usedFeatureNames) {
		if (category == null) {
			return word + " <-";
		} else {
			List<Integer> vars = new ArrayList<Integer>();
			List<Integer> mvars = new ArrayList<Integer>();
			
			vars.clear();
			mvars.clear();
			category.collectVars(vars, mvars);
			return word + " <- " + category.getIdentifier(mvars, usedFeatureNames);
		}
	}
	
	public boolean equals(Object obj) {
		if (!(obj instanceof ConcreteOption)) return false;
		ConcreteOption other = (ConcreteOption) obj;
		return this.identifier.equals(other.identifier);
	}
	
	public int hashCode() {
		return identifier.hashCode();
	}
	
	public String toString() {
		if (category == null) {
			return word + " <-";
		} else {
			return word + " <- " + category;
		}
	}

}
