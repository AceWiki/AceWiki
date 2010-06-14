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

import nextapp.echo2.app.Alignment;
import nextapp.echo2.app.ApplicationInstance;
import nextapp.echo2.app.Button;
import nextapp.echo2.app.Color;
import nextapp.echo2.app.Column;
import nextapp.echo2.app.Component;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.SplitPane;
import nextapp.echo2.app.TaskQueueHandle;
import nextapp.echo2.app.event.ActionListener;
import ch.uzh.ifi.attempto.echocomp.Label;
import ch.uzh.ifi.attempto.echocomp.Style;
import ch.uzh.ifi.attempto.echocomp.WindowPane;

/**
 * This class represents a menu block of the predictive editor. A menu block consists of a list of
 * menu items.
 * 
 * @author Tobias Kuhn
 */
class MenuBlock extends SplitPane {
	
	private static final long serialVersionUID = -5856577034761259001L;
	
	private ActionListener actionListener;
	private Column menuColumn = new Column();
	private Button label = new Button("...");

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
		super(ORIENTATION_VERTICAL_TOP_BOTTOM, new Extent(16));
		this.app = ApplicationInstance.getActive();
		this.taskQueue = app.createTaskQueue();
		this.actionListener = actionListener;
		
		label.setEnabled(false);
		label.setHeight(new Extent(15));
		label.setWidth(new Extent(100));
		label.setDisabledBackground(Color.WHITE);
		label.setDisabledForeground(Color.BLACK);
		label.setDisabledFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(11)));
		label.setRolloverEnabled(false);
		label.setLineWrap(false);
		label.setAlignment(new Alignment(Alignment.LEFT, Alignment.BOTTOM));
		label.setInsets(new Insets(1, 0, 0, 0));
		add(label);
		
		SplitPane spLeft = new SplitPane(SplitPane.ORIENTATION_HORIZONTAL_LEFT_RIGHT);
		spLeft.setSeparatorColor(Color.BLACK);
		spLeft.setSeparatorWidth(new Extent(1));
		spLeft.setSeparatorPosition(new Extent(0));
		spLeft.add(new Label());
		add(spLeft);
		
		SplitPane spTop = new SplitPane(SplitPane.ORIENTATION_VERTICAL_TOP_BOTTOM);
		spTop.setSeparatorColor(Color.BLACK);
		spTop.setSeparatorHeight(new Extent(1));
		spTop.setSeparatorPosition(new Extent(0));
		spTop.add(new Label());
		spLeft.add(spTop);
		
		SplitPane spRight = new SplitPane(SplitPane.ORIENTATION_HORIZONTAL_RIGHT_LEFT);
		spRight.setSeparatorColor(Color.BLACK);
		spRight.setSeparatorWidth(new Extent(1));
		spRight.setSeparatorPosition(new Extent(0));
		spRight.add(new Label());
		spTop.add(spRight);
		
		SplitPane spBottom = new SplitPane(SplitPane.ORIENTATION_VERTICAL_BOTTOM_TOP);
		spBottom.setSeparatorColor(Color.BLACK);
		spBottom.setSeparatorHeight(new Extent(1));
		spBottom.setSeparatorPosition(new Extent(0));
		spBottom.add(new Label());
		spBottom.setBackground(Style.lightBackground);
		spRight.add(spBottom);
		
		Column menuBaseColumn = new Column();
		menuBaseColumn.setBackground(Style.mediumBackground);
		menuBaseColumn.add(menuColumn);
		spBottom.add(menuBaseColumn);
	}
	
	/**
	 * Sets the content and the size of this menu block.
	 * 
	 * @param content The content to be shown in this menu block.
	 * @param width The width of this menu block in pixels.
	 * @param pageSize The vertical page size in number of items.
	 */
	public synchronized void setContent(final MenuBlockContent content, final int width, final int pageSize) {
		state++;
		label.setText(content.getName());
		label.setWidth(new Extent(width - 3));
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
				m.addActionListener(actionListener);
			}
			progress = items.size();
		}
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
			m.addActionListener(actionListener);
			c.add(m);
		}
		progress = endPos;
		return c;
	}
	
	public void setVisible(boolean visible) {
		state++;
		super.setVisible(visible);
	}

}
