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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * This class stands for backward refernces. In contrast to all other categories, backward
 * references can have multiple feature maps. They have one or more "positive" feature
 * maps and zero or more "negative" feature maps.
 * 
 * @author Tobias Kuhn
 */
public class BackrefCategory extends Nonterminal {
	
	/**
	 * The positive feature maps of the backward reference.
	 */
	protected List<FeatureMap> posFeatureMaps;
	
	/**
	 * The negative feature maps of the backward reference.
	 */
	protected List<FeatureMap> negFeatureMaps;
	
	/**
	 * Creates a new backward reference. Positive and negative feature maps have to be added by
	 * using the respective methods.
	 */
	public BackrefCategory() {
		name = "<";
		posFeatureMaps = new ArrayList<FeatureMap>();
		negFeatureMaps = new ArrayList<FeatureMap>();
	}
	
	public List<FeatureMap> getPosFeatureMaps() {
		return posFeatureMaps;
	}
	
	public List<FeatureMap> getNegFeatureMaps() {
		return negFeatureMaps;
	}
	
	public void addPosFeatureMap(FeatureMap fm) {
		posFeatureMaps.add(fm);
	}
	
	public void addNegFeatureMap(FeatureMap fm) {
		negFeatureMaps.add(fm);
	}
	
	public void unify(Category c) throws UnificationFailedException {
		if (!(c instanceof BackrefCategory)) {
			throw new UnificationFailedException();
		}
		BackrefCategory b = (BackrefCategory) c;
		if (b.posFeatureMaps == null) throw new UnificationFailedException();
		if (b.posFeatureMaps.size() != posFeatureMaps.size()) throw new UnificationFailedException();
		for (int i = 0 ; i < posFeatureMaps.size() ; i++) {
			posFeatureMaps.get(i).unify(b.posFeatureMaps.get(i));
		}
		if (b.negFeatureMaps.size() != negFeatureMaps.size()) throw new UnificationFailedException();
		for (int i = 0 ; i < negFeatureMaps.size() ; i++) {
			negFeatureMaps.get(i).unify(b.negFeatureMaps.get(i));
		}
	}
	
	public void tryToUnify(Category c) throws UnificationFailedException {
		if (!(c instanceof BackrefCategory)) {
			throw new UnificationFailedException();
		}
		BackrefCategory b = (BackrefCategory) c;
		if (b.posFeatureMaps == null) throw new UnificationFailedException();
		if (b.posFeatureMaps.size() != posFeatureMaps.size()) throw new UnificationFailedException();
		for (int i = 0 ; i < posFeatureMaps.size() ; i++) {
			posFeatureMaps.get(i).tryToUnify(b.posFeatureMaps.get(i));
		}
		if (b.negFeatureMaps.size() != negFeatureMaps.size()) throw new UnificationFailedException();
		for (int i = 0 ; i < negFeatureMaps.size() ; i++) {
			negFeatureMaps.get(i).tryToUnify(b.negFeatureMaps.get(i));
		}
	}
	
	public boolean isSimilar(Category c) {
		if (!(c instanceof BackrefCategory)) return false;
		BackrefCategory b = (BackrefCategory) c;
		if (posFeatureMaps.size() == b.posFeatureMaps.size()) return true;
		return false;
	}
	
	public boolean subsumes(Category c) {
		if (!(c instanceof BackrefCategory)) return false;
		
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
	
	public void skolemize() {
		for (FeatureMap fm : posFeatureMaps) {
			fm.skolemize();
		}
		for (FeatureMap fm : negFeatureMaps) {
			fm.skolemize();
		}
	}
	
	public Set<String> getFeatureNames() {
		Set<String> featureNames = new HashSet<String>();
		for (FeatureMap fm : posFeatureMaps) {
			featureNames.addAll(fm.getFeatureNames());
		}
		for (FeatureMap fm : negFeatureMaps) {
			featureNames.addAll(fm.getFeatureNames());
		}
		return featureNames;
	}
	
	public Collection<StringRef> getFeatureValues() {
		Set<StringRef> featureNames = new HashSet<StringRef>();
		for (FeatureMap fm : posFeatureMaps) {
			featureNames.addAll(fm.getFeatureValues());
		}
		for (FeatureMap fm : negFeatureMaps) {
			featureNames.addAll(fm.getFeatureValues());
		}
		return featureNames;
	}
	
	String getIdentifier(List<Integer> mvars, String[] usedFeatureNames) {
		String s = getName();
		s += "+";
		for (FeatureMap fm : getPosFeatureMaps()) {
			s += fm.getIdentifier(mvars, usedFeatureNames);
		}
		s += "-";
		for (FeatureMap fm : getNegFeatureMaps()) {
			s += fm.getIdentifier(mvars, usedFeatureNames);
		}
		return s;
	}
	
	Category deepCopy(HashMap<Integer, StringObject> stringObjs) {
		Category c;
		c = new BackrefCategory();
		for (FeatureMap fm : posFeatureMaps) {
			c.addPosFeatureMap(fm.deepCopy(stringObjs));
		}
		for (FeatureMap fm : negFeatureMaps) {
			c.addNegFeatureMap(fm.deepCopy(stringObjs));
		}
		return c;
	}
	
	protected String getType() {
		return "bwref";
	}
	
	public String toString() {
		String s = "<+";
		for (FeatureMap fm : posFeatureMaps) {
			s += fm.toString();
		}
		s += "-";
		for (FeatureMap fm : negFeatureMaps) {
			s += fm.toString();
		}
		return s;
	}

}
