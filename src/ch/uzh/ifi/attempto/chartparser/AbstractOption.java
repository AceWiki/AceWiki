package ch.uzh.ifi.attempto.chartparser;

import java.util.ArrayList;
import java.util.List;

/**
 * This class represents an option (in an abstract way) how a partial sentence can be continued
 * according to given grammar. Such an abstract option consists of a category that represents
 * possible next tokens and of a set of zero or more exceptions, also in the form of categories.
 * The categories can be terminal or pre-terminal, but not non-terminal.
 * 
 * @see NextTokenOptions
 * @see ConcreteOption
 * @author Tobias Kuhn
 */
public class AbstractOption {
	
	private final Category category;
	private final Category[] exceptions;
	private final String identifier;
	
	AbstractOption(Grammar grammar, Category category, Category[] exceptions) {
		this.category = category;
		if (exceptions != null) {
			this.exceptions = exceptions;
		} else {
			this.exceptions = new Category[] {};
		}
		identifier = calculateIdentifier(grammar.getUsedFeatureNames());
	}
	
	AbstractOption(Grammar grammar, Category category, List<Category> exceptions) {
		this(grammar, category, exceptions.toArray(new Category[] {}));
	}
	
	AbstractOption(Grammar grammar, Category category) {
		this(grammar, category, (Category[]) null);
	}
	
	/**
	 * Returns the terminal or pre-terminal category that represents possible next tokens.
	 * 
	 * @return The terminal or pre-terminal category.
	 */
	public Category getCategory() {
		return category;
	}
	
	/**
	 * This method returns the exceptions.
	 * 
	 * @return The exceptions in the form of categories.
	 */
	public Category[] getExceptions() {
		return exceptions;
	}
	
	/**
	 * Returns true if the given category is fulfilled by this option.
	 * 
	 * @param c The category.
	 * @return true if it is fulfilled by this option.
	 */
	public boolean isFulfilledBy(Category c) {
		if (!category.subsumes(c)) return false;
		if (exceptions == null) return true;
		for (Category x : exceptions) {
			if (x.subsumes(c)) return false;
		}
		return true;
	}
	
	String calculateIdentifier(String[] usedFeatureNames) {
		List<Integer> vars = new ArrayList<Integer>();
		List<Integer> mvars = new ArrayList<Integer>();
		String id = "";
		
		vars.clear();
		mvars.clear();
		category.collectVars(vars, mvars);
		id += category.getIdentifier(mvars, usedFeatureNames) + " / ";
		
		if (exceptions != null) {
			for (Category x : exceptions) {
				vars.clear();
				mvars.clear();
				x.collectVars(vars, mvars);
				id += x.getIdentifier(mvars, usedFeatureNames) + " ";
			}
		}
		return id;
	}
	
	public boolean equals(Object obj) {
		if (!(obj instanceof AbstractOption)) return false;
		AbstractOption other = (AbstractOption) obj;
		return this.identifier.equals(other.identifier);
	}
	
	public int hashCode() {
		return identifier.hashCode();
	}
	
	public String toString() {
		String s = "";
		s += category + "";
		if (exceptions.length > 0) {
			s += " except";
			for (Category x : exceptions) {
				s += " " + x;
			}
		}
		return s;
	}

}
