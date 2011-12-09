// This file is part of AceWiki.
// Copyright 2008-2011, AceWiki developers.
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

package ch.uzh.ifi.attempto.acewiki.core;

/**
 * This class shows a simple progress bar on the standard error channel.
 */
class ConsoleProgressBar {
	
	private long maxPoints;
	private long progress = 0;
	private int steps = 0;
	
	/**
	 * Creates and starts a new progress bar.
	 * 
	 * @param maxPoints The number of points representing 100%.
	 */
	public ConsoleProgressBar(long maxPoints) {
		this.maxPoints = maxPoints;
		System.err.print("0%");
	}
	
	/**
	 * Creates and starts a new progress bar.
	 * 
	 * @param maxPoints The number of points representing 100%.
	 */
	public ConsoleProgressBar(int maxPoints) {
		this((long) maxPoints);
	}
	
	/**
	 * Increases the progress bar by one point.
	 */
	public void addOne() {
		add(1);
	}
	
	/**
	 * Increases the progress bar by the specified amount of points.
	 * 
	 * @param points The number of points to be added.
	 */
	public void add(long points) {
		if (points < 0) return;
		progress += points;
		int s;
		if (maxPoints == 0) {
			s = 40;
		} else {
			s = (int) ((40 * progress) / maxPoints);
		}
		for (int i = steps+1 ; i <= s ; i++) {
			if (i % 4 == 0) {
				System.err.print((int) (i * 2.5) + "%");
			} else {
				System.err.print(".");
			}
		}
		steps = s;
	}
	
	/**
	 * Increases the progress bar by the specified amount of points.
	 * 
	 * @param points The number of points to be added.
	 */
	public void add(int points) {
		add((long) points);
	}
	
	/**
	 * Completes the progress bar.
	 */
	public void complete() {
		add(maxPoints - progress);
		System.err.print("\n");
	}

}
