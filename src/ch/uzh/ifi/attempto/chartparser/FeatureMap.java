// This file is part of AceWiki.
// Copyright 2008-2010, Tobias Kuhn.
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
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

/**
 * This class represents a set of features consisting of name/value pairs.
 * 
 * @author Tobias Kuhn
 */
public class FeatureMap {
	
	/**
	 * This counter is used for skolemization:
	 */
	private static int skNumber = 0;
	
	private Map<String, StringRef> features = new HashMap<String, StringRef>();
	
	/**
	 * Creates an empty feature map.
	 */
	public FeatureMap() {
	}
	
	/**
	 * Sets a feature. If the feature is already present, the feature value is overridden and not
	 * unified.
	 * 
	 * @param featureName The name of the feature to set.
	 * @param featureValue The feature value.
	 */
	public void setFeature(String featureName, StringRef featureValue) {
		features.put(featureName, featureValue);
	}
	
	/**
	 * Returns a feature value.
	 * 
	 * @param featureName The name of the feature.
	 * @return The value of the feature.
	 */
	public StringRef getFeature(String featureName) {
		StringRef featureValue = features.get(featureName);
		if (featureValue == null) {
			featureValue = new StringRef();
			features.put(featureName, featureValue);
		}
		return featureValue;
	}
	
	/**
	 * Unifies this feature map with another feature map. Two feature maps can unify if and only if
	 * all values of common feature name unify. If the unification fails, a
	 * UnificationFailedException is thrown. In this case, the two feature maps remain partly unified,
	 * i.e. no backtracking is done. Thus, this operation should be perfomed only if it is certain that
	 * the unification succeeds, or if the operation is performed on copies of objects that are not
	 * used anymore afterwards.
	 * 
	 * @param featureMap The feature map to be unified with this feature map.
	 * @throws UnificationFailedException If unification fails.
	 */
	public void unify(FeatureMap featureMap) throws UnificationFailedException {
		for (String f : features.keySet()) {
			getFeature(f).unify(featureMap.getFeature(f));
		}
		for (String f : featureMap.features.keySet()) {
			getFeature(f).unify(featureMap.getFeature(f));
		}
	}
	
	/**
	 * Tries to unify this feature map with another feature map. If unification is not possible, an exception
	 * is thrown. In the case unification would be possible, the unification is not performed completely.
	 * In any case the two feature maps remain in an unconsistent state afterwards. Thus, this operation should
	 * be performed only on copies of objects that are not used anymore afterwards.
	 * 
	 * @param featureMap The feature map to be unified with this feature map.
	 * @throws UnificationFailedException If unification fails.
	 */
	public void tryToUnify(FeatureMap featureMap) throws UnificationFailedException {
		if (featureMap == null) {
			throw new UnificationFailedException();
		}
		for (String f : features.keySet()) {
			features.get(f).unify(featureMap.getFeature(f));
		}
	}
	
	/**
	 * This method detects whether this feature map can unify with the given feature map. Neither of the two
	 * feature maps are changed.
	 * 
	 * @param featureMap The feature map for the unification check.
	 * @return true if the two feature map can unify.
	 */
	public boolean canUnify(FeatureMap featureMap) {
		if (!isSimilar(featureMap)) return false;
		FeatureMap thisC = deepCopy();
		FeatureMap otherC = featureMap.deepCopy();
		try {
			thisC.tryToUnify(otherC);
		} catch (UnificationFailedException ex) {
			return false;
		}
		return true;
	}
	
	/**
	 * Skolemizes the feature values of this feature map.
	 */
	public void skolemize() {
		for (String feature : features.keySet()) {
			StringRef s = features.get(feature);
			if (s.getString() == null) {
				try {
					s.unify(new StringRef("$SK" + skNumber++));
				} catch (UnificationFailedException ex) {}
			}
		}
	}
	
	/**
	 * This methods checks whether two feature maps are similar. Two feature maps are similar
	 * if and only if they do not share a feature with the same name but with values that do
	 * not unify locally (i.e. without considering the unifications entailed by other features).
	 * 
	 * @param fm The category for which similarity with this category should be checked.
	 * @return true if the two categories are similar.
	 */
	public boolean isSimilar(FeatureMap fm) {
		if (fm == null) return false;
		for (String v : features.keySet()) {
			String s1 = features.get(v).getString();
			String s2 = null;
			StringRef sr2 = fm.features.get(v);
			if (sr2 != null) s2 = sr2.getString();
			if (s1 != null && s2 != null && !s1.equals(s2)) return false;
		}
		return true;
	}
	
	/**
	 * Creates a deep copy of this feature map.
	 * 
	 * @return A deep copy.
	 */
	public FeatureMap deepCopy() {
		return deepCopy(new HashMap<Integer, StringObject>());
	}
	
	/**
	 * Creates a deep copy of this feature map using the given string objects. This method is
	 * usually called form another deepCopy-method.
	 * 
	 * @param stringObjs The string objects to be used.
	 * @return A deep copy.
	 */
	FeatureMap deepCopy(HashMap<Integer, StringObject> stringObjs) {
		FeatureMap fm = new FeatureMap();
		for (String feature : features.keySet()) {
			StringRef s = features.get(feature);
			StringObject se = stringObjs.get(s.getID());
			if (se != null) {
				fm.setFeature(feature, se.newStringRef());
			} else {
				StringRef sr = new StringRef(s.getString());
				fm.setFeature(feature, sr);
				stringObjs.put(s.getID(), sr.getStringObject());
			}
		}
		return fm;
	}
	
	/**
	 * Returns the used feature names.
	 * 
	 * @return The used feature names.
	 */
	public Set<String> getFeatureNames() {
		return features.keySet();
	}
	
	/**
	 * Returns the used feature values.
	 * 
	 * @return The used feature values.
	 */
	public Collection<StringRef> getFeatureValues() {
		return features.values();
	}
	
	String getIdentifier(List<Integer> mvars, String[] usedFeatureNames) {
		String s = "(";
		int i = 0;
		for (String n : usedFeatureNames) {
			i++;
			if (!getFeatureNames().contains(n)) continue;
			StringRef v = getFeature(n);
			if (v.getString() == null) {
				if (mvars.contains(v.getID())) {
					s += i + ":" + mvars.indexOf(v.getID()) + ",";
				}
			} else {
				s += i + ":" + v.getString() + ",";
			}
		}
		return s + ")";
	}
	
	public String toString() {
		String s = "";
		Set<String> featureKeys = features.keySet();
		if (featureKeys.size() > 0) s += "(";
		for (String feature : new TreeSet<String>(features.keySet())) {
			// traverse the feature names in alphabetical order
			StringRef sr = features.get(feature);
			s += feature + ":";
			if (sr.getString() == null) {
				s += sr.getID();
			} else {
				s += sr.getString();
			}
			s += ",";
		}
		if (featureKeys.size() > 0) {
			s = s.substring(0, s.length()-1) + ")";
		}
		return s;
	}

}
