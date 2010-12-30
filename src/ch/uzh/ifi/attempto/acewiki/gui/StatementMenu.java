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

package ch.uzh.ifi.attempto.acewiki.gui;

import nextapp.echo.app.Border;
import nextapp.echo.app.Color;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import nextapp.echo.app.ResourceImageReference;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import nextapp.echo.extras.app.DropDownMenu;
import nextapp.echo.extras.app.menu.DefaultMenuModel;
import nextapp.echo.extras.app.menu.DefaultOptionModel;
import nextapp.echo.extras.app.menu.SeparatorModel;
import ch.uzh.ifi.attempto.echocomp.Style;

/**
 * This class represents drop down menus that are shown in front of the ACE sentences as small
 * triangles. Such drop down menus can have different appearances based on different types.
 * 
 * @author Tobias Kuhn
 */
public class StatementMenu extends DropDownMenu implements ActionListener {

	private static final long serialVersionUID = 3646511994109953476L;
	
	/**
	 * This type is for sentences used for reasoning.
	 */
	public static final int REASONING_TYPE = 0;
	
	/**
	 * This type is for sentences that are not used for reasoning.
	 */
	public static final int NOREASONING_TYPE = 1;
	
	/**
	 * This type is for questions.
	 */
	public static final int QUESTION_TYPE = 2;
	
	/**
	 * This type is for sentences that are automatically inferred.
	 */
	public static final int INFERRED_TYPE = 3;
	
	/**
	 * This type is for comments.
	 */
	public static final int COMMENT_TYPE = 4;
	
	/**
	 * This type is for drop down menus with no statement.
	 */
	public static final int EMPTY_TYPE = 5;
	
	private DefaultMenuModel menuModel = new DefaultMenuModel();
	private ActionListener actionListener;
	
	/**
	 * Creates a empty drop down menu with an icon of the given color.
	 * 
	 * @param type The type of the statement.
	 * @param actionListener The action-listener.
	 */
	public StatementMenu(int type, ActionListener actionListener) {
		this.actionListener = actionListener;
		setModel(menuModel);
		
		setWidth(new Extent(15));
		String img1 = null;
		//String img2 = null;
		// TODO: reactivate tooltips
		if (type == REASONING_TYPE ) {
			img1 = "ch/uzh/ifi/attempto/acewiki/gui/img/DropDown1.png";
			//img2 = "ch/uzh/ifi/attempto/acewiki/gui/img/DropDown2.png";
			//setToolTipText("Blue triangle: This statement is considered for reasoning.");
		} else if (type == QUESTION_TYPE) {
			img1 = "ch/uzh/ifi/attempto/acewiki/gui/img/DropDown1.png";
			//img2 = "ch/uzh/ifi/attempto/acewiki/gui/img/DropDown2.png";
			//setToolTipText("This is a question that is automatically answered.");
		} else if (type == NOREASONING_TYPE) {
			img1 = "ch/uzh/ifi/attempto/acewiki/gui/img/DropDownRed1.png";
			//img2 = "ch/uzh/ifi/attempto/acewiki/gui/img/DropDownRed2.png";
			//setToolTipText("Red triangle: This statement is too complex to be considered for reasoning.");
		} else if (type == INFERRED_TYPE) {
			img1 = "ch/uzh/ifi/attempto/acewiki/gui/img/DropDownLBlue1.png";
			//img2 = "ch/uzh/ifi/attempto/acewiki/gui/img/DropDownLBlue2.png";
			//setToolTipText("White triangle: This statement is automatically inferred.");
		} else if (type == COMMENT_TYPE) {
			img1 = "ch/uzh/ifi/attempto/acewiki/gui/img/DropDownGray1.png";
			//img2 = "ch/uzh/ifi/attempto/acewiki/gui/img/DropDownGray2.png";
			//setToolTipText("Gray triangle: This statement is an informal comment.");
		} else if (type == EMPTY_TYPE) {
			img1 = "ch/uzh/ifi/attempto/acewiki/gui/img/DropDownGray1.png";
			//img2 = "ch/uzh/ifi/attempto/acewiki/gui/img/DropDownGray2.png";
			//setToolTipText("Click here to add a sentence or comment.");
		}
		setBackground(Color.WHITE);
		setInsets(new Insets(0));
		setExpandIcon(new ResourceImageReference(img1));
		//setMenuExpandIcon(new ResourceImageReference(img2));
		//setDisabledExpandIcon(new ResourceImageReference(img2));
		// TODO: reactivate effects for rollover and pressed
		//setsetToggleRolloverIcon(new ResourceImageReference(img2));
		//setTogglePressedIcon(new ResourceImageReference(img2));
		setBorder(new Border(0, Color.BLACK, Border.STYLE_SOLID));
		//setRolloverBorder(new Border(0, Color.BLACK, Border.STYLE_SOLID));
		
		setMenuBackground(Style.mediumBackground);
		setMenuForeground(Style.darkForeground);
		setMenuWidth(new Extent(140));
		setMenuBorder(new Border(1, Color.BLACK, Border.STYLE_OUTSET));
		setSelectionBackground(Style.darkBackground);
		setSelectionForeground(Style.lightForeground);
		setMenuFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(11)));
		addActionListener(this);
	}
	
	/**
	 * Adds a menu entry to the drop down menu.
	 * 
	 * @param text The text of the menu entry.
	 * @param tooltip The tool tip text of the menu entry.
	 */
	public void addMenuEntry(String text, String tooltip) {
		menuModel.addItem(new DefaultOptionModel(text, text, null));
	}
	
	/**
	 * Adds a separator to the menu.
	 */
	public void addMenuSeparator() {
		menuModel.addItem(new SeparatorModel());
	}
	
	public void actionPerformed(ActionEvent e) {
		actionListener.actionPerformed(new ActionEvent(this, e.getActionCommand()));
	}

}
