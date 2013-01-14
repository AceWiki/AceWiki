// This file is part of AceWiki.
// Copyright 2008-2012, AceWiki developers.
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

import java.util.ArrayList;
import java.util.List;

import nextapp.echo.app.Alignment;
import nextapp.echo.app.ApplicationInstance;
import nextapp.echo.app.Border;
import nextapp.echo.app.Color;
import nextapp.echo.app.Column;
import nextapp.echo.app.Component;
import nextapp.echo.app.ContentPane;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Grid;
import nextapp.echo.app.Insets;
import nextapp.echo.app.Row;
import nextapp.echo.app.WindowPane;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import nextapp.echo.app.layout.GridLayoutData;
import ch.uzh.ifi.attempto.echocomp.GeneralButton;
import ch.uzh.ifi.attempto.echocomp.Label;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import ch.uzh.ifi.attempto.echocomp.TextField;

/**
 * This class represents a form that appears within a tab of the word editor window. Such a form
 * contains the form elements for a certain class of words.
 * 
 * @author Tobias Kuhn
 */
public class WordEditorForm extends ContentPane implements ActionListener {

	private static final long serialVersionUID = 5886665203518065212L;
	
	private ActionListener actionListener;
	private WindowPane parentWindow;
	private Row buttonBar;
	private String title = "";
	private Column column;
	private Row explanationRow;
	private List<Component> formElements = new ArrayList<Component>();
	private List<Component> requiredFormElements = new ArrayList<Component>();
	
	/**
	 * Creates a new word editor form.
	 * 
	 * @param parentWindow The parent window.
	 * @param actionListener The action-listener.
	 */
	public WordEditorForm(WindowPane parentWindow, ActionListener actionListener) {
		this.parentWindow = parentWindow;
		this.actionListener = actionListener;
		
		Column borderCol = new Column();
		borderCol.setBorder(new Border(1, Color.BLACK, Border.STYLE_INSET));
		
		Grid grid = new Grid(1);
		grid.setRowHeight(0, new Extent(parentWindow.getHeight().getValue()-160));
		grid.setRowHeight(1, new Extent(30));
		
		column = new Column();
		column.setInsets(new Insets(10, 20, 0, 0));
		column.setCellSpacing(new Extent(10));
		GridLayoutData gridLayout = new GridLayoutData();
		gridLayout.setAlignment(new Alignment(Alignment.LEFT, Alignment.TOP));
		column.setLayoutData(gridLayout);
		grid.add(column);
		
		explanationRow = new Row();
		column.add(explanationRow);
		
		Row footerRow = new Row();
		footerRow.setInsets(new Insets(10, 0, 0, 0));
		footerRow.add(new Label("* required field", Font.ITALIC, 11));
		grid.add(footerRow);
		
		buttonBar = new Row();
		buttonBar.setAlignment(new Alignment(Alignment.RIGHT, Alignment.CENTER));
		buttonBar.setInsets(new Insets(10, 10, 10, 10));
		buttonBar.setCellSpacing(new Extent(5));
		grid.add(buttonBar);
		
		addButton("OK");
		addButton("Cancel");
		
		borderCol.add(grid);
		add(borderCol);
	}
	
	/**
	 * Creates a new word editor form.
	 * 
	 * @param title The title of the word editor form.
	 * @param parentWindow The parent window.
	 * @param actionListener The action-listener.
	 */
	public WordEditorForm(String title, WindowPane parentWindow, ActionListener actionListener) {
		this(parentWindow, actionListener);
		setTitle(title);
	}
	
	/**
	 * Sets the title of this form.
	 * 
	 * @param title The title.
	 */
	public void setTitle(String title) {
		this.title = title;
	}
	
	/**
	 * Returns the title of the word editor form.
	 * 
	 * @return The title.
	 */
	public String getTitle() {
		return title;
	}
	
	/**
	 * Return the parent window of this form.
	 * 
	 * @return The parent window.
	 */
	public WindowPane getParentWindow() {
		return parentWindow;
	}
	
	/**
	 * Returns the action-listener.
	 * 
	 * @return The action-listener.
	 */
	public ActionListener getActionListener() {
		return actionListener;
	}
	
	/**
	 * Adds a button to the button bar.
	 * 
	 * @param buttonText The text of the button.
	 */
	public void addButton(String buttonText) {
		buttonBar.add(new GeneralButton(buttonText, this, 80));
	}
	
	/**
	 * Removes all existing buttons and adds the given buttons to the button bar.
	 * 
	 * @param buttonTexts The texts for the buttons.
	 */
	public void setButtons(String... buttonTexts) {
		buttonBar.removeAll();
		for (String t : buttonTexts) {
			addButton(t);
		}
	}
	
	/**
	 * Adds a new row to the form.
	 * 
	 * @param labelText The text for the label shown on the left hand side of the component.
	 * @param formElement The component, i.e. a text field.
	 * @param explanation An explanation text shown under the component.
	 * @param required Defines whether the component should be marked as required.
	 */
	public void addRow(String labelText, Component formElement, String explanation,
			boolean required) {
		Grid grid = new Grid(3);
		grid.setInsets(new Insets(0, 0, 5, 0));
		grid.setColumnWidth(0, new Extent(140));
		grid.add(new SolidLabel(labelText, Font.ITALIC));
		formElements.add(formElement);
		if (formElement instanceof TextField) {
			TextField tf = (TextField) formElement;
			tf.setWidth(new Extent(parentWindow.getWidth().getValue()-223));
			tf.setActionCommand("OK");
		}
		grid.add(formElement);
		if (required) {
			requiredFormElements.add(formElement);
			grid.add(new Label("*", Font.ITALIC, 11));
		} else {
			grid.add(new Label());
		}
		grid.add(new Label());
		grid.add(new Label(explanation, Font.ITALIC, 11));
		column.add(grid);
	}

	/**
	 * Sets the explanation component. The explanation component is shown above the form elements
	 * and should explain how these form elements have to be used.
	 * 
	 * @param comp The graphical component.
	 */
	public void setExplanationComponent(Component comp) {
		explanationRow.removeAll();
		explanationRow.add(comp);
	}
	
	/**
	 * Returns all form elements.
	 * 
	 * @return A list of all form elements.
	 */
	public List<Component> getFormElements() {
		return formElements;
	}
	
	/**
	 * Returns all form elements that are marked as required.
	 * 
	 * @return A list of all required form elements.
	 */
	public List<Component> getRequiredFormElements() {
		return requiredFormElements;
	}
	
	/**
	 * Returns the content of all form elements that are text fields.
	 * 
	 * @return A list of the content of all form elements that are text fields.
	 */
	public List<String> getRequiredTextFieldContents() {
		List<String> textContents = new ArrayList<String>();
		for (Component comp : requiredFormElements) {
			if (comp instanceof TextField) {
				textContents.add(((TextField) comp).getText());
			}
		}
		return textContents;
	}
	
	/**
	 * This method sets the focus on the first enabled text field of this form.
	 */
	protected void doFocus() {
		doFocus(this);
	}
	
	private boolean doFocus(Component c) {
		if (c instanceof TextField) {
			TextField tf = (TextField) c;
			if (tf.isEnabled()) {
				ApplicationInstance.getActive().setFocusedComponent(tf);
				return true;
			} else {
				return false;
			}
		} else {
			for (Component child : c.getComponents()) {
				boolean b = doFocus(child);
				if (b) return true;
			}
		}
		return false;
	}
	
	public void actionPerformed(ActionEvent e) {
		actionListener.actionPerformed(new ActionEvent(parentWindow, e.getActionCommand()));
	}

}