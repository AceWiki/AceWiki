package ch.uzh.ifi.attempto.acewiki.gui;

import nextapp.echo.app.Button;
import nextapp.echo.app.Extent;
import nextapp.echo.app.ResourceImageReference;
import nextapp.echo.app.event.ActionListener;

/**
 * This class represents a button consisting of an icon, for example forward and backward arrows to
 * navigate through the wiki history.
 * 
 * @author Tobias Kuhn
 */
public class IconButton extends Button {
	
	private static final long serialVersionUID = 9007778082893227249L;

	/**
	 * Creates a new icon button. The name has to correspond to the name of the icon image files
	 * and is also shown as a tooltip.
	 * 
	 * @param name The name of the icon button.
	 * @param actionListener The action listener for this button.
	 */
	public IconButton(String name, ActionListener actionListener) {
		setRolloverEnabled(true);
		setWidth(new Extent(25));
		setHeight(new Extent(25));
		String filename = name.toLowerCase();
		String path = "ch/uzh/ifi/attempto/acewiki/gui/img/";
		setIcon(new ResourceImageReference(path + filename + ".png"));
		setRolloverIcon(new ResourceImageReference(path + filename + "h.png"));
		setDisabledIcon(new ResourceImageReference(path + filename + "i.png"));
		setActionCommand(name);
		setToolTipText(name);
		addActionListener(actionListener);
	}

}
