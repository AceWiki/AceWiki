// This file is part of AceWiki.
// Copyright 2008-2013, AceWiki developers.
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
import nextapp.echo.app.Button;
import nextapp.echo.app.Color;
import nextapp.echo.app.Component;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import nextapp.echo.app.Row;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import nextapp.echo.app.layout.RowLayoutData;
import ch.uzh.ifi.attempto.echocomp.Label;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import ch.uzh.ifi.attempto.echocomp.Style;

/**
 * This class represents a title label which is used for article titles.
 * 
 * @author Tobias Kuhn
 */
// TODO improve this class
public class Title extends Row implements ActionListener {
	
	private static final long serialVersionUID = 7797492687936611323L;
	
	private Component titleComp;
	private SolidLabel postTitleLabel;
	private ActionListener actionListener;
	
	/**
	 * Creates a new clickable title which has two parts. The second part is shown in gray.
	 * 
	 * @param text The text of the main part of the title.
	 * @param postTitle The text of the second part of the title in gray.
	 * @param tooltip The tooltip text.
	 * @param actionListener The action listener.
	 */
	public Title(String text, String postTitle, String tooltip, ActionListener actionListener) {
		this.actionListener = actionListener;
		setInsets(new Insets(10, 5, 10, 5));
		Row row = new Row();
		row.setCellSpacing(new Extent(5));
		if (actionListener == null) {
			Label l = new Label(text);
			if (tooltip != null) {
				l.setToolTipText(tooltip);
			}
			titleComp = l;
		} else {
			Button b = new Button(text);
			b.setInsets(new Insets(0, 0, 0, 0));
			b.setRolloverEnabled(true);
			b.setRolloverForeground(Color.BLUE);
			b.addActionListener(this);
			if (tooltip != null) {
				b.setToolTipText(tooltip);
			}
			titleComp = b;
		}
		titleComp.setFont(new Font(Style.fontTypeface, Font.PLAIN, new Extent(20)));
		row.add(titleComp);
		postTitleLabel = new SolidLabel(postTitle);
		postTitleLabel.setFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(20)));
		postTitleLabel.setForeground(Color.DARKGRAY);
		RowLayoutData layout = new RowLayoutData();
		layout.setAlignment(new Alignment(Alignment.CENTER, Alignment.BOTTOM));
		postTitleLabel.setLayoutData(layout);
		row.add(postTitleLabel);
		add(row);
	}
	/**
	 * Creates a new non-clickable title which has two parts. The second part is shown in gray.
	 * 
	 * @param text The text of the main part of the title.
	 * @param postTitle The text of the second part of the title in gray.
	 */
	public Title(String text, String postTitle) {
		this(text, postTitle, null, null);
	}
	
	/**
	 * Creates a new clickable title.
	 * 
	 * @param text The text of the title.
	 * @param italic Defines whether the title text should be displayed in italics.
	 * @param tooltip The tooltip text.
	 * @param actionListener The action listener.
	 */
	public Title(String text, boolean italic, String tooltip, ActionListener actionListener) {
		this.actionListener = actionListener;
		setInsets(new Insets(10, 5, 5, 5));
		if (actionListener == null) {
			Label l = new Label(text);
			if (tooltip != null) {
				l.setToolTipText(tooltip);
			}
			titleComp = l;
		} else {
			Button b = new Button(text);
			b.setInsets(new Insets(0, 0, 0, 0));
			b.setRolloverEnabled(true);
			b.setRolloverForeground(Color.BLUE);
			b.addActionListener(this);
			if (tooltip != null) {
				b.setToolTipText(tooltip);
			}
			titleComp = b;
		}
		if (italic) {
			titleComp.setFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(20)));
		} else {
			titleComp.setFont(new Font(Style.fontTypeface, Font.PLAIN, new Extent(20)));
		}
		add(titleComp);
	}
	
	/**
	 * Creates a new non-clickable title.
	 * 
	 * @param text The text of the title.
	 * @param tooltip The tooltip text.
	 * @param actionListener The action listener.
	 */
	public Title(String text, String tooltip, ActionListener actionListener) {
		this(text, false, tooltip, actionListener);
	}
	
	/**
	 * Creates a new clickable title.
	 * 
	 * @param text The text of the title.
	 * @param italic Defines whether the title text should be displayed in italics.
	 */
	public Title(String text, boolean italic) {
		this(text, italic, null, null);
	}
	
	/**
	 * Creates a new non-clickable title.
	 * 
	 * @param text The title text.
	 */
	public Title(String text) {
		this(text, false, null, null);
	}
	
	/**
	 * Resets the title text.
	 * 
	 * @param text The title text.
	 */
	public void setText(String text) {
		if (titleComp instanceof Label) {
			((Label) titleComp).setText(text);
		} else if (titleComp instanceof Button) {
			((Button) titleComp).setText(text);
		}
	}

	/**
	 * Sets the text of the second part of the title in gray.
	 * 
	 * @param text The new text.
	 */
	public void setPostTitle(String text) {
		if (postTitleLabel != null) {
			postTitleLabel.setText(text);
		}
	}

	/**
	 * Sets the tooltip text for the title.
	 * 
	 * @param tooltip The new text.
	 */
	public void setTooltip(String tooltip) {
		if (titleComp instanceof Label) {
			((Label) titleComp).setToolTipText(tooltip);
		} else if (titleComp instanceof Button) {
			((Button) titleComp).setToolTipText(tooltip);
		}
	}
	
	/**
	 * Sets the foreground color of the title text.
	 * 
	 * @param color The foreground color.
	 */
	public void setColor(Color color) {
		titleComp.setForeground(color);
	}
	
	public void actionPerformed(ActionEvent e) {
		actionListener.actionPerformed(new ActionEvent(this, "Title clicked"));
	}
	
}
