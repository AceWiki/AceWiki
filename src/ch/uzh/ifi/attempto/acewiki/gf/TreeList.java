package ch.uzh.ifi.attempto.acewiki.gf;

import java.util.List;

import com.google.common.collect.ImmutableList;

/**
 * Non-empty ordered set of Strings.
 */
public class TreeList {

	private final ImmutableList<String> mTrees;

	public TreeList(Iterable<String> trees) {
		mTrees = ImmutableList.copyOf(trees);
		if (mTrees.isEmpty()) {
			throw new IllegalArgumentException("Empty tree set");
		}
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
		if (mTrees.size() == 1) {
			return mTrees.get(0);
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