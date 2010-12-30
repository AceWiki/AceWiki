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

package ch.uzh.ifi.attempto.aceeditor;

import nextapp.echo.app.Color;
import nextapp.echo.extras.app.MenuBarPane;
import nextapp.echo.extras.app.menu.DefaultMenuModel;
import nextapp.echo.extras.app.menu.DefaultMenuStateModel;
import nextapp.echo.extras.app.menu.DefaultOptionModel;
import nextapp.echo.extras.app.menu.DefaultToggleOptionModel;
import nextapp.echo.extras.app.menu.MenuModel;
import nextapp.echo.extras.app.menu.MenuStateModel;
import nextapp.echo.extras.app.menu.SeparatorModel;

/**
 * This class represent the menu bar for the ACE Editor.
 * 
 * @author Tobias Kuhn
 */
class MenuBar {

	private MenuBarPane menuBarPane;

	/**
	 * Creates a new menu bar object.
	 * 
	 * @param editor The ACE Editor object.
	 */
	public MenuBar(ACEEditor editor) {
		menuBarPane = new MenuBarPane();
		menuBarPane.addActionListener(editor);
		menuBarPane.setBackground(new Color(230, 230, 230));
		menuBarPane.setForeground(Color.BLACK);
		menuBarPane.setSelectionBackground(new Color(100, 100, 200));
		DefaultMenuModel menuBar = new DefaultMenuModel();

		DefaultMenuModel firstMenu = new DefaultMenuModel("ACE Editor", "ACE Editor");
		firstMenu.addItem(new DefaultOptionModel("About", "About", null));
		firstMenu.addItem(new DefaultOptionModel("Attempto Website", "Attempto Website", null));
		menuBar.addItem(firstMenu);

		DefaultMenuModel fileMenu = new DefaultMenuModel("File", "File");
		fileMenu.addItem(new DefaultOptionModel("Open Text...", "Open Text...", null));
		fileMenu.addItem(new DefaultOptionModel("Save Text...", "Save Text...", null));
		if (!editor.isLexiconImmutable()) {
			fileMenu.addItem(new SeparatorModel());
			fileMenu.addItem(new DefaultOptionModel("Load Lexicon...", "Load Lexicon...", null));
			fileMenu.addItem(new DefaultOptionModel("Replace Lexicon...", "Replace Lexicon...", null));
			fileMenu.addItem(new DefaultOptionModel("Save Lexicon...", "Save Lexicon...", null));
		}
		menuBar.addItem(fileMenu);

		DefaultMenuModel editMenu = new DefaultMenuModel("Edit", "Edit");
		editMenu.addItem(new DefaultOptionModel("Add...", "Add...", null));
		editMenu.addItem(new DefaultOptionModel("Add Comment...", "Add Comment...", null));
		editMenu.addItem(new DefaultOptionModel("Add Separator", "Add Separator", null));
		editMenu.addItem(new DefaultOptionModel("Modify...", "Modify...", null));
		editMenu.addItem(new DefaultOptionModel("Delete", "Delete", null));
		editMenu.addItem(new SeparatorModel());
		editMenu.addItem(new DefaultOptionModel("Cut", "Cut", null));
		editMenu.addItem(new DefaultOptionModel("Copy", "Copy", null));
		editMenu.addItem(new DefaultOptionModel("Paste", "Paste", null));
		menuBar.addItem(editMenu);

		DefaultMenuModel viewMenu = new DefaultMenuModel("View", "View");
		DefaultMenuModel defaultViewMenu = new DefaultMenuModel("Defaults", "Defaults");
		defaultViewMenu.addItem(new DefaultToggleOptionModel("Default Expanded", "Expanded"));
		defaultViewMenu.addItem(new SeparatorModel());
		for (String s : ResultItem.TYPES) {
			defaultViewMenu.addItem(new DefaultToggleOptionModel("Default " + s, s));
		}
		viewMenu.addItem(defaultViewMenu);
		viewMenu.addItem(new SeparatorModel());
		viewMenu.addItem(new DefaultToggleOptionModel("Expanded", "Expanded"));
		viewMenu.addItem(new DefaultOptionModel("Expand All", "Expand All", null));
		viewMenu.addItem(new DefaultOptionModel("Collapse All", "Collapse All", null));
		viewMenu.addItem(new SeparatorModel());
		for (String s : ResultItem.TYPES) {
			viewMenu.addItem(new DefaultToggleOptionModel("Show " + s, s));
		}
		menuBar.addItem(viewMenu);

		menuBarPane.setModel(menuBar);
	}
	
	public MenuBarPane getContent() {
		return menuBarPane;
	}

	public boolean isSelected(String id) {
		return menuBarPane.getStateModel().isSelected(id);
	}

	public void setSelected(String id, boolean selected) {
		menuBarPane.getStateModel().setSelected(id, selected);
	}

	public boolean isEnabled(String id) {
		return menuBarPane.getStateModel().isEnabled(id);
	}

	public void setEnabled(String id, boolean enabled) {
		menuBarPane.getStateModel().setEnabled(id, enabled);
	}

	public void update() {
		MenuModel m = menuBarPane.getModel();
		menuBarPane.setModel(new DefaultMenuModel());
		menuBarPane.setModel(m);
		MenuStateModel s = menuBarPane.getStateModel();
		menuBarPane.setStateModel(new DefaultMenuStateModel());
		menuBarPane.setStateModel(s);
	}

}
