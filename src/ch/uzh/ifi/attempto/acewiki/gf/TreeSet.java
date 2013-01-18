package ch.uzh.ifi.attempto.acewiki.gf;

import java.util.Set;

import com.google.common.collect.ImmutableSet;

public class TreeSet {

	private final ImmutableSet<String> mTrees;

	public TreeSet(Iterable<String> trees) {
		mTrees = ImmutableSet.copyOf(trees);
	}

	public TreeSet(String tree) {
		mTrees = ImmutableSet.of(tree);
	}

	public Set<String> getTrees() {
		return mTrees;
	}

	/**
	 * @return the single member if the set is singleton, otherwise return {@code null}
	 */
	public String getTree() {
		if (size() == 1) {
			return mTrees.iterator().next();
		}
		return null;
	}

	public int size() {
		return mTrees.size();
	}

	public String toString() {
		return mTrees.toString();
	}

}