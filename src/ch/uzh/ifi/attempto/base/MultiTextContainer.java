package ch.uzh.ifi.attempto.base;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import com.google.common.collect.ImmutableList;

public class MultiTextContainer implements Iterable<TextContainer> {

	private final List<TextContainer> mTextContainerList;

	public MultiTextContainer(TextContainer... textContainers) {
		mTextContainerList = ImmutableList.copyOf(textContainers);
		if (mTextContainerList.size() == 0) {
			throw new RuntimeException("Empty MultiTextContainer");
		}
	}

	public MultiTextContainer(Collection<TextContainer> textContainers) {
		mTextContainerList = ImmutableList.copyOf(textContainers);
		if (mTextContainerList.size() == 0) {
			throw new RuntimeException("Empty MultiTextContainer");
		}
	}

	/**
	 * Returns the text elements of the first text container.
	 * 
	 * @return The text elements.
	 */
	public List<TextElement> getTextElements() {
		return get(0).getTextElements();
	}

	public TextContainer get(int index) {
		return mTextContainerList.get(index);
	}

	public int size() {
		return mTextContainerList.size();
	}

	@Override
	public Iterator<TextContainer> iterator() {
		return mTextContainerList.iterator();
	}

}