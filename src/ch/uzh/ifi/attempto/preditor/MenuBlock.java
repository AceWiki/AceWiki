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

package ch.uzh.ifi.attempto.preditor;

import java.util.List;

import nextapp.echo.app.Alignment;
import nextapp.echo.app.ApplicationInstance;
import nextapp.echo.app.Border;
import nextapp.echo.app.Button;
import nextapp.echo.app.Color;
import nextapp.echo.app.Column;
import nextapp.echo.app.Component;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import nextapp.echo.app.Row;
import nextapp.echo.app.TaskQueueHandle;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.echocomp.Style;
import ch.uzh.ifi.attempto.echocomp.WindowPane;
import echopoint.ContainerEx;
import echopoint.able.Scrollable;

/**
 * This class represents a menu block of the predictive editor. A menu block consists of a list of
 * menu items.
 * 
 * @author Tobias Kuhn
 */
class MenuBlock extends Column implements ActionListener {
	
	private static final long serialVersionUID = -5856577034761259001L;
	
	private ActionListener actionListener;
	private MenuBlockContent content;
	private ContainerEx container;
	private Column menuColumn = new Column();
	private Button label = new Button("...");
	private Button enlargeButton = new Button();

	private int state = 0;
	private List<MenuItem> items;
	private int width;
	private int progress;
	private final ApplicationInstance app;
	private final TaskQueueHandle taskQueue;
	
	private final int lazyEvalStep = 25;
	
	/**
	 * Creates a new menu block.
	 * 
	 * @param actionListener The action listener.
	 * @param parent The parent window.
	 */
	public MenuBlock(ActionListener actionListener, WindowPane parent) {
		this.app = ApplicationInstance.getActive();
		this.taskQueue = app.createTaskQueue();
		this.actionListener = actionListener;
		
		Row labelRow = new Row();
		label.setEnabled(false);
		label.setHeight(new Extent(16));
		label.setWidth(new Extent(100));
		label.setDisabledBackground(Color.WHITE);
		label.setDisabledForeground(Color.BLACK);
		label.setDisabledFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(11)));
		label.setRolloverEnabled(false);
		label.setLineWrap(false);
		label.setAlignment(new Alignment(Alignment.LEFT, Alignment.BOTTOM));
		label.setInsets(new Insets(1, 0, 0, 0));
		labelRow.add(label);
		enlargeButton.setHeight(new Extent(15));
		enlargeButton.setWidth(new Extent(10));
		enlargeButton.setFont(new Font(Style.fontTypeface, Font.BOLD, new Extent(11)));
		enlargeButton.setForeground(Style.lightBackground);
		enlargeButton.setRolloverEnabled(true);
		enlargeButton.setRolloverForeground(Color.BLACK);
		enlargeButton.addActionListener(this);
		setEnlarged(false);
		labelRow.add(enlargeButton);
		add(labelRow);
		
		container = new ContainerEx();
		container.setBorder(new Border(1, Color.BLACK, Border.STYLE_INSET));
		container.setBackground(Style.lightBackground);
		container.setScrollBarPolicy(Scrollable.AUTO);
		add(container);
		
		Column menuBaseColumn = new Column();
		menuBaseColumn.setBackground(Style.mediumBackground);
		menuBaseColumn.add(menuColumn);
		container.add(menuBaseColumn);
	}
	
	public void setWidth(int width) {
		container.setWidth(new Extent(width - 2));
	}
	
	public void setHeight(int height) {
		container.setHeight(new Extent(height - 16));
	}
	
	/**
	 * Sets the content and the size of this menu block.
	 * 
	 * @param content The content to be shown in this menu block.
	 * @param width The width of this menu block in pixels.
	 * @param pageSize The vertical page size in number of items.
	 */
	public synchronized void setContent(MenuBlockContent content, final int width, final int pageSize) {
		this.content = content;
		state++;
		label.setText(content.getName());
		label.setWidth(new Extent(width - 13));
		menuColumn.removeAll();
		this.width = width;
		
		items = content.getItems();
		progress = 0;
		
		if (items.size() > pageSize) {
			menuColumn.add(createNextMenuComponent());
			calculateRest(state);
		} else {
			for (MenuItem m : items) {
				menuColumn.add(m);
				m.setWidth(new Extent(width - 7));
			}
			progress = items.size();
		}
	}
	
	/**
	 * Returns the current content of this menu block.
	 * 
	 * @return The content.
	 */
	public MenuBlockContent getContent() {
		return content;
	}
	
	/**
	 * Clears the content and removes all menu items.
	 */
	public void clear() {
		state++;
		content = null;
		menuColumn.removeAll();
	}
	
	private synchronized void calculateRest(final int state) {
		if (progress >= items.size()) return;
		if (this.state != state) return;

		Thread thread = new Thread() {
			public void run() {
				final Component c = createNextMenuComponent();
				
				app.enqueueTask(
					taskQueue,
					new Runnable() {
						public void run() {
							addMenuComponent(c, state);
						}
					}
				);
			}
		};
		thread.start();
	}
	
	private synchronized void addMenuComponent(Component c, int state) {
		if (this.state != state) return;
		menuColumn.add(c);
		calculateRest(state);
	}
	
	private synchronized Component createNextMenuComponent() {
		Column c = new Column();
		int endPos = progress + lazyEvalStep;
		if (endPos > items.size()) endPos = items.size();
		for (int i = progress ; i < endPos ; i++) {
			MenuItem m = items.get(i);
			m.setWidth(new Extent(width - 24));
			c.add(m);
		}
		progress = endPos;
		return c;
	}
	
	public void setVisible(boolean visible) {
		state++;
		super.setVisible(visible);
	}
	
	/**
	 * This method is used to switch from normal to enlarged mode and back.
	 * 
	 * @param enlarged true to switch to enlarged mode; false to switch back.
	 */
	public void setEnlarged(boolean enlarged) {
		state++;
		if (enlarged) {
			enlargeButton.setText("–");
			enlargeButton.setToolTipText("back to normal size");
			enlargeButton.setActionCommand("downsize");
		} else {
			enlargeButton.setText("+");
			enlargeButton.setToolTipText("enlarge this box");
			enlargeButton.setActionCommand("enlarge");
		}
	}
	
	public void actionPerformed(ActionEvent e) {
		String c = e.getActionCommand();
		if (actionListener != null) {
			actionListener.actionPerformed(new ActionEvent(this, c));
		}
	}

}
