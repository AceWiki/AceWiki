package ch.uzh.ifi.attempto.acewiki.core.ontology;

/**
 * This class shows a simple progress bar on the standard error channel.
 */
class ConsoleProgressBar {
	
	private int maxPoints;
	private int progress = 0;
	private int percentage = 0;
	
	/**
	 * Creates and starts a new progress bar.
	 * 
	 * @param maxPoints The number of points representing 100%.
	 */
	public ConsoleProgressBar(int maxPoints) {
		this.maxPoints = maxPoints;
		System.err.print("0%");
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
	 * @param addPoints The number of points to be added.
	 */
	public void add(int points) {
		if (points < 0) return;
		progress += points;
		int newPercentage;
		if (maxPoints == 0) {
			newPercentage = 100;
		} else {
			newPercentage = (100 * progress) / maxPoints;
		}
		for (int i = percentage+1 ; i <= newPercentage ; i++) {
			if (i % 10 == 0) {
				System.err.print(i + "%");
			} else if (i % 2 == 0) {
				System.err.print(".");
			}
		}
		percentage = newPercentage;
	}
	
	/**
	 * Completes the progress bar.
	 */
	public void complete() {
		add(maxPoints - progress);
		System.err.print("\n");
	}

}
