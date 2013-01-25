package ch.uzh.ifi.attempto.acewiki.gf;

import java.util.List;

import com.google.common.collect.ImmutableList;

public class TreeList {

	private final ImmutableList<String> mTrees;

	public TreeList(Iterable<String> trees) {
		mTrees = ImmutableList.copyOf(trees);
	}

	public TreeList(String tree) {
		mTrees = ImmutableList.of(tree);
	}

	public List<String> getTrees() {
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