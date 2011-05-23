// This file is part of AceWiki.
// Copyright 2008-2011, Tobias Kuhn.
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

import nextapp.echo.app.Alignment;
import nextapp.echo.app.Border;
import nextapp.echo.app.Button;
import nextapp.echo.app.Color;
import nextapp.echo.app.Column;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import nextapp.echo.app.ResourceImageReference;
import nextapp.echo.app.Row;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import nextapp.echo.app.layout.RowLayoutData;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.echocomp.Style;
import echopoint.ContainerEx;
import echopoint.able.Positionable;

/**
 * This class represents drop down menus that are shown in front of the ACE sentences as small
 * triangles. Such drop down menus can have different appearances based on different types.
 * 
 * @author Tobias Kuhn
 */
public class StatementMenu extends Row implements ActionListener {

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
	
	private Button toggle = new Button();
	private ContainerEx popup;
	private Button overlayLockButton;
	
	private int type;
	
	private Column menuColumn = new Column();
	private ActionListener actionListener;
	private Column menuSeparator = null;
	private Wiki wiki;
	private RowLayoutData layout;
	
	/**
	 * Creates a empty drop down menu with an icon of the given color.
	 * 
	 * @param type The type of the statement.
	 * @param wiki The wiki object.
	 * @param actionListener The action-listener.
	 */
	public StatementMenu(int type, Wiki wiki, ActionListener actionListener) {
		this.type = type;
		this.wiki = wiki;
		this.actionListener = actionListener;
		
		layout = new RowLayoutData();
		layout.setAlignment(new Alignment(Alignment.LEFT, Alignment.TOP));
		
		toggle.setWidth(new Extent(13));
		toggle.setHeight(new Extent(13));
		toggle.setInsets(new Insets(1));
		toggle.setLayoutData(layout);
		toggle.setIcon(getNormalIcon());
		toggle.setRolloverEnabled(true);
		toggle.setRolloverIcon(getActiveIcon());
		toggle.setPressedIcon(getActiveIcon());
		toggle.setToolTipText(getTooltipText());
		toggle.addActionListener(this);
		add(toggle);
	}
	
	/**
	 * Adds a menu entry to the drop down menu.
	 * 
	 * @param text The text of the menu entry.
	 * @param tooltip The tool tip text of the menu entry.
	 */
	public void addMenuEntry(String text, String tooltip) {
		if (menuSeparator != null) {
			menuColumn.add(menuSeparator);
			menuSeparator = null;
		}
		Button menuEntry = new Button(text);
		menuEntry.setActionCommand(text);
		menuEntry.setHeight(new Extent(16));
		menuEntry.setWidth(new Extent(140));
		menuEntry.setBackground(Style.mediumBackground);
		menuEntry.setForeground(Style.darkForeground);
		menuEntry.setRolloverEnabled(true);
		menuEntry.setRolloverForeground(Style.lightForeground);
		menuEntry.setRolloverBackground(Style.darkBackground);
		menuEntry.setInsets(new Insets(2, 0));
		menuEntry.setAlignment(new Alignment(Alignment.LEFT, Alignment.CENTER));
		menuEntry.setTextAlignment(new Alignment(Alignment.LEFT, Alignment.CENTER));
		menuEntry.setFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(13)));
		menuEntry.setToolTipText(tooltip);
		menuEntry.addActionListener(this);
		menuColumn.add(menuEntry);
	}
	
	/**
	 * Adds a separator to the menu.
	 */
	public void addMenuSeparator() {
		if (menuColumn.getComponentCount() == 0) return;
		menuSeparator = new Column();
		menuSeparator.setInsets(new Insets(2, 0, 2, 0));
		menuSeparator.setBackground(Style.mediumBackground);
		Column line = new Column();
		line.setBackground(Style.darkBackground);
		line.setInsets(new Insets(0, 1, 0, 0));
		menuSeparator.add(line);
	}
	
	public void actionPerformed(ActionEvent e) {
		Object src = e.getSource();
		
		if (src == toggle) {
			setExpanded(true);
		} else if (src == wiki) {
			setExpanded(false);
		} else if (src == overlayLockButton) {
			setExpanded(false);
		} else {
			setExpanded(false);
			actionListener.actionPerformed(new ActionEvent(this, e.getActionCommand()));
		}
	}
	
	/**
	 * This method shows or hides the popup part of the menu.
	 * 
	 * @param expanded true to show the popup; false to hide it.
	 */
	public void setExpanded(boolean expanded) {
		if (expanded) {
			if (isExpanded()) return;
			if (menuColumn.getComponentCount() == 0) return;
			wiki.lock(this);
			toggle.setIcon(getActiveIcon());
			if (popup == null) {
				popup = new ContainerEx();
				popup.setPosition(Positionable.ABSOLUTE);
				popup.add(menuColumn);
				popup.setBorder(new Border(1, Color.BLACK, Border.STYLE_OUTSET));
				popup.setZIndex(Integer.MAX_VALUE);
				popup.setLayoutData(layout);
				add(popup);
				
				ContainerEx overlayLockComponent = new ContainerEx();
				overlayLockComponent.setPosition(Positionable.FIXED);
				overlayLockComponent.setTop(new Extent(0));
				overlayLockComponent.setLeft(new Extent(0));
				overlayLockButton = new Button();
				overlayLockButton.setWidth(new Extent(10000));
				overlayLockButton.setHeight(new Extent(10000));
				overlayLockButton.addActionListener(this);
				overlayLockComponent.add(overlayLockButton);
				add(overlayLockComponent);
			} else {
				popup.setVisible(true);
				overlayLockButton.setVisible(true);
			}
		} else {
			if (!isExpanded()) return;
			wiki.unlock();
			toggle.setIcon(getNormalIcon());
			popup.setVisible(false);
			overlayLockButton.setVisible(false);
		}
	}
	
	/**
	 * Returns whether the menu is expanded.
	 * 
	 * @return true if the menu is expanded.
	 */
	public boolean isExpanded() {
		return popup != null && popup.isVisible();
	}
	
	private ResourceImageReference getNormalIcon() {
		return Wiki.getImage("tri" + getIconType() + ".png");
	}
	
	private ResourceImageReference getActiveIcon() {
		return Wiki.getImage("tri" + getIconType() + "l.png");
	}
	
	private String getIconType() {
		if (type == REASONING_TYPE ) {
			return "blue";
		} else if (type == QUESTION_TYPE) {
			return "blue";
		} else if (type == NOREASONING_TYPE) {
			return "red";
		} else if (type == INFERRED_TYPE) {
			return "white";
		} else if (type == COMMENT_TYPE) {
			return "gray";
		} else if (type == EMPTY_TYPE) {
			return "gray";
		}
		return "";
	}
	
	private String getTooltipText() {
		String t = null;
		if (type == REASONING_TYPE ) {
			t = "Blue triangle: This statement is considered for reasoning.";
		} else if (type == QUESTION_TYPE) {
			t = "This is a question that is automatically answered.";
		} else if (type == NOREASONING_TYPE) {
			t = "Red triangle: This statement is too complex to be considered for reasoning.";
		} else if (type == INFERRED_TYPE) {
			t = "White triangle: This statement is automatically inferred.";
		} else if (type == COMMENT_TYPE) {
			t = "Gray triangle: This statement is an informal comment.";
		} else if (type == EMPTY_TYPE) {
			t = "Click here to add a sentence or comment.";
		}
		return t;
	}

}
