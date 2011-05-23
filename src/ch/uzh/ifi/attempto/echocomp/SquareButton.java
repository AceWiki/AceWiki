package ch.uzh.ifi.attempto.echocomp;

import nextapp.echo.app.Button;
import nextapp.echo.app.event.ActionListener;

/**
 * This class represents a small square button with one of the following predefined icons: right,
 * down, plus, diamond.
 * 
 * @author Tobias Kuhn
 */
public class SquareButton extends Button {
	
	private static final long serialVersionUID = -3555338733759467195L;
	
	private static final String imgpath = "ch/uzh/ifi/attempto/echocomp/style/";

	/**
	 * Creates a new square button.
	 * 
	 * @param iconName The name of the icon button.
	 * @param tooltip The tooltip to be shown.
	 * @param actionListener The action listener.
	 */
	public SquareButton(String iconName, String tooltip, ActionListener actionListener) {
		setRolloverEnabled(true);
		setRolloverBackground(Style.lightBackground);
		if (tooltip != null) {
			setToolTipText(tooltip);
		}
		addActionListener(actionListener);
		setIconName(iconName);
	}
	
	/**
	 * Creates a new square button.
	 * 
	 * @param iconName The name of the icon button.
	 * @param actionListener The action listener.
	 */
	public SquareButton(String iconName, ActionListener actionListener) {
		this(iconName, null, actionListener);
	}
	
	/**
	 * Changes the icon name.
	 * 
	 * @param iconName The new icon name.
	 */
	public void setIconName(String iconName) {
		setIcon(Style.getImage(imgpath + iconName + ".png"));
	}

}
