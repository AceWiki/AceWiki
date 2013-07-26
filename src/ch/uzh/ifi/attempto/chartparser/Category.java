// This file is part of AceWiki.
// Copyright 2008-2013, AceWiki developers.
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

package ch.uzh.ifi.attempto.chartparser;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang.ArrayUtils;

/**
 * This class represents a grammatical category.
 * 
 * @author Tobias Kuhn
 */
public abstract class Category {
	
	/**
	 * This static array contains all category names that denote special categories. These special
	 * category names are ">" and ">>" for forward references, "<" and "/<" for backward references,
	 * "//" for scope openers, and "#" for position operators.
	 */
	public static final String[] specialCategories = new String[] {">", ">>", "<", "/<", "//", "#"};
	
	/**
	 * The name of the category.
	 */
	protected String name;
	
	/**
	 * The feature map of the category. Backward references do not use this field, since they can
	 * have multiple feature maps.
	 */
	protected FeatureMap featureMap;
	
	/**
	 * This method returns the type of the category. For example, "term" is returned for terminal
	 * categories and "nonterm" for non-terminal ones.
	 * 
	 * @return The type of the category.
	 */
	protected abstract String getType();
	
	/**
	 * Returns the name of the category.
	 * 
	 * @return The name of the category.
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * Returns true if this category is a special category. Special categories are references, scope
	 * openers, and position operators.
	 * 
	 * @return true if this category is a special category.
	 */
	public boolean isSpecialCategory() {
		return ArrayUtils.contains(specialCategories, name);
	}
	
	/**
	 * Returns the map of features of this category. It returns null in the case of backward
	 * references, where the methods for positive and negative feature maps have to be used.
	 * 
	 * @return The feature map.
	 */
	public FeatureMap getFeatureMap() {
		return featureMap;
	}
	
	/**
	 * This method returns the list of positive feature maps for backward references, or null
	 * for all other categories.
	 * 
	 * @return The list of positive feature maps in the case of backward references.
	 */
	public List<FeatureMap> getPosFeatureMaps() {
		return null;
	}
	
	/**
	 * This method returns the list of negative feature maps for backward references, or null
	 * for all other categories.
	 * 
	 * @return The list of negative feature maps in the case of backward references.
	 */
	public List<FeatureMap> getNegFeatureMaps() {
		return null;
	}
	
	/**
	 * Adds a positive feature map in the case of backward references, or does nothing for all
	 * other categories.
	 * 
	 * @param fm The positive feature map to be added.
	 */
	public void addPosFeatureMap(FeatureMap fm) {
	}
	
	/**
	 * Adds a negative feature map in the case of backward references, or does nothing for all
	 * other categories.
	 * 
	 * @param fm The negative feature map to be added.
	 */
	public void addNegFeatureMap(FeatureMap fm) {
	}
	
	/**
	 * Sets a feature. This method cannot be used for backward references, which can have more than
	 * one feature map.
	 * 
	 * @param featureName The feature name
	 * @param featureValue The string reference that points to the value of the feature.
	 */
	public void setFeature(String featureName, StringRef featureValue) {
		featureMap.setFeature(featureName, featureValue);
	}
	
	/**
	 * Sets a feature. This method cannot be used for backward references, which can have more than
	 * one feature map.
	 * 
	 * @param featureName The feature name
	 * @param featureValue The value of the feature.
	 */
	public void setFeature(String featureName, String featureValue) {
		featureMap.setFeature(featureName, new StringRef(featureValue));
	}
	
	/**
	 * Returns a feature value. This method cannot be used for backward references, which can have
	 * more than one feature map.
	 * 
	 * @param featureName The name of the feature.
	 * @return The value of the feature.
	 */
	public StringRef getFeature(String featureName) {
		return featureMap.getFeature(featureName);
	}
	
	/**
	 * Unifies this category with another category. Two categories can unify if and only if they have
	 * the same names and the same types and they have no features with conflicting values. If the
	 * unification fails, a UnificationFailedException is thrown. In this case, the two categories
	 * remain partly unified, i.e. no backtracking is done. Thus, this operation should be perfomed
	 * only if it is certain that the unification succeeds, or if the operation is performed on
	 * copies of objects that are not used anymore afterwards.
	 * 
	 * @param c The category to be unified with this category.
	 * @throws UnificationFailedException If unification fails.
	 */
	public void unify(Category c) throws UnificationFailedException {
		if (!name.equals(c.name) || !getType().equals(c.getType())) {
			throw new UnificationFailedException();
		}
		featureMap.unify(c.featureMap);
	}
	
	/**
	 * Tries to unify this category with another category. If unification is not possible, an exception
	 * is thrown. In the case unification would be possible, the unification is not performed completely.
	 * In any case the two categories remain in an unconsistent state afterwards. Thus, this operation
	 * should be performed only on copies of objects that are not used anymore afterwards.
	 * 
	 * @param c The category to be unified with this category.
	 * @throws UnificationFailedException If unification fails.
	 */
	public void tryToUnify(Category c) throws UnificationFailedException {
		if (!name.equals(c.name) || !getType().equals(c.getType())) {
			throw new UnificationFailedException();
		}
		featureMap.tryToUnify(c.featureMap);
	}
	
	/**
	 * This method detects whether this category can unify with the given category. Neither of the two
	 * categories are changed.
	 * 
	 * @param category The category for the unification check.
	 * @return true if the two categories can unify.
	 */
	public boolean canUnify(Category category) {
		if (!isSimilar(category)) return false;
		Category thisC = deepCopy();
		Category otherC = category.deepCopy();
		try {
			thisC.tryToUnify(otherC);
		} catch (UnificationFailedException ex) {
			return false;
		}
		return true;
	}
	
	/**
	 * This methods checks whether two categories are similar. Two categories are similar if and
	 * only if the categories have the same name and the same type and no feature with the same
	 * name is present in both categories with values that do not unify locally. Two categories
	 * that are unifiable are always similar, but not necessarily vice versa. This check for
	 * similarity is computationally less expensive than the check for unification.
	 * 
	 * @param c The category for which similarity with this category should be checked.
	 * @return true if the two categories are similar.
	 */
	public boolean isSimilar(Category c) {
		if (!name.equals(c.name) || !getType().equals(c.getType())) return false;
		if (c.featureMap == null) return false;
		if (featureMap.getFeatureNames().isEmpty()) return true;
		if (c.featureMap.getFeatureNames().isEmpty()) return true;
		return featureMap.isSimilar(c.featureMap);
	}
	
	/**
	 * This method returns true if this category subsumes (in other words "is more general than")
	 * the given category, or false otherwise.
	 * 
	 * @param c The category for which it is checked whether this category subsumes it.
	 * @return true if this category subsumes the given category.
	 */
	public boolean subsumes(Category c) {
		if (!name.equals(c.name) || !getType().equals(c.getType())) return false;
		
		// Some quick checks before doing the expensive copying of the categories:
		if (c.featureMap == null) return false;
		if (featureMap.getFeatureNames().isEmpty()) return true;
		if (!featureMap.isSimilar(c.featureMap)) return false;
		
		// Both categories are copied to keep the existing categories untouched:
		Category category1C = deepCopy();
		Category category2C = c.deepCopy();
		
		// Category 1 subsumes category 2 iff 1 unifies with 2 after the skolemization of 2.
		category2C.skolemize();
		try {
			category1C.tryToUnify(category2C);
			return true;
		} catch (UnificationFailedException ex) {
			return false;
		}
	}
	
	/**
	 * Skolemizes the feature values of this category.
	 */
	public void skolemize() {
		featureMap.skolemize();
	}
	
	/**
	 * Returns the used feature names within the feature map.
	 * 
	 * @return The used feature names.
	 */
	public Set<String> getFeatureNames() {
		return featureMap.getFeatureNames();
	}
	
	/**
	 * Returns the used feature values within the feature map.
	 * 
	 * @return The used feature values.
	 */
	public Collection<StringRef> getFeatureValues() {
		return featureMap.getFeatureValues();
	}
	
	void collectVars(List<Integer> vars, List<Integer> mvars) {
		if (this instanceof Terminal) return;
		Collection<StringRef> fvalues = getFeatureValues();
		if (fvalues == null) return;
		for (StringRef v : fvalues) {
			if (v.getString() == null) {
				int id = v.getID();
				if (vars.contains(id)) {
					if (!mvars.contains(id)) {
						mvars.add(id);
					}
				} else {
					vars.add(id);
				}
			}
		}
	}
	
	String getIdentifier(List<Integer> mvars, String[] usedFeatureNames) {
		return getName() + featureMap.getIdentifier(mvars, usedFeatureNames);
	}
	
	/**
	 * Creates a deep copy of this category.
	 * 
	 * @return A deep copy.
	 */
	public Category deepCopy() {
		return deepCopy(new HashMap<Integer, StringObject>());
	}
	
	/**
	 * Creates a deep copy of this category using the given string objects. This method is
	 * usually called form another deepCopy-method.
	 * 
	 * @param stringObjs The string objects to be used.
	 * @return A deep copy.
	 */
	Category deepCopy(HashMap<Integer, StringObject> stringObjs) {
		Category c;
		if (this instanceof Terminal) {
			c = new Terminal(name);
		} else if (this instanceof Preterminal) {
			c = new Preterminal(name);
		} else if (this instanceof Nonterminal) {
			c = new Nonterminal(name);
		} else {
			throw new RuntimeException("Unknown category type: " + this.getClass().toString());
		}
		c.featureMap = featureMap.deepCopy(stringObjs);
		return c;
	}
	
	public boolean equals(Object obj) {
		if (!(obj instanceof Category)) return false;
		if (!getType().equals(((Category) obj).getType())) return false;
		return toString().equals(obj.toString());
	}
	
	public abstract String toString();

}
