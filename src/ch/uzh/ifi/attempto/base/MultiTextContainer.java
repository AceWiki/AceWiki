// This file is part of AceWiki.
// Copyright 2008-2012, AceWiki developers.
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

package ch.uzh.ifi.attempto.base;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import com.google.common.collect.ImmutableList;

/**
 * This class represents a text that can have multiple variants (all in the same language).
 * Multiple variants can arise when the internal representation is ambiguous.
 * 
 * @author Kaarel Kaljurand
 * @author Tobias Kuhn
 */
public class MultiTextContainer implements Iterable<TextContainer> {

	private final List<TextContainer> mTextContainerList;

	/**
	 * Generates a new multi-text container.
	 * 
	 * @param textContainers The text containers, each representing a variant.
	 */
	public MultiTextContainer(TextContainer... textContainers) {
		mTextContainerList = ImmutableList.copyOf(textContainers);
		if (mTextContainerList.size() == 0) {
			throw new RuntimeException("Empty MultiTextContainer");
		}
	}

	/**
	 * Generates a new multi-text container.
	 * 
	 * @param textContainers The text containers, each representing a variant.
	 */
	public MultiTextContainer(Collection<TextContainer> textContainers) {
		mTextContainerList = ImmutableList.copyOf(textContainers);
		if (mTextContainerList.size() == 0) {
			throw new RuntimeException("Empty MultiTextContainer");
		}
	}

	/**
	 * Returns the text elements of the first variant.
	 * 
	 * @return The text elements.
	 */
	public List<TextElement> getTextElements() {
		return get(0).getTextElements();
	}

	/**
	 * Returns the text container for the given variant.
	 * 
	 * @param index The index of the variant.
	 * @return The text container for the variant.
	 */
	public TextContainer get(int index) {
		return mTextContainerList.get(index);
	}

	/**
	 * Returns the number of variants.
	 * 
	 * @return The number of variants.
	 */
	public int size() {
		return mTextContainerList.size();
	}

	@Override
	public Iterator<TextContainer> iterator() {
		return mTextContainerList.iterator();
	}

}