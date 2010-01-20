package ch.uzh.ifi.attempto.preditor;

import java.util.ArrayList;
import java.util.List;

import nextapp.echo2.app.Alignment;
import nextapp.echo2.app.Column;
import nextapp.echo2.app.Component;
import nextapp.echo2.app.ContentPane;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.Grid;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.Row;
import nextapp.echo2.app.event.ActionEvent;
import nextapp.echo2.app.event.ActionListener;
import nextapp.echo2.app.layout.GridLayoutData;
import ch.uzh.ifi.attempto.echocomp.GeneralButton;
import ch.uzh.ifi.attempto.echocomp.Label;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import ch.uzh.ifi.attempto.echocomp.TextField;
import ch.uzh.ifi.attempto.echocomp.WindowPane;

/**
 * This class represents a form that appears within a tab of the word editor window. Such a form
 * contains the form elements for a certain class of words.
 * 
 * @author Tobias Kuhn
 */
public class WordEditorForm extends ContentPane implements ActionListener {

	private static final long serialVersionUID = 5886665203518065212L;
	
	/**
	 * The action-listener.
	 */
	protected ActionListener actionListener;
	
	/**
	 * The parent window.
	 */
	protected WindowPane parent;
	
	/**
	 * The "OK" button.
	 */
	protected GeneralButton okButton;
	
	/**
	 * The "Cancel" button.
	 */
	protected GeneralButton cancelButton;
	
	private String title;
	private Column column;
	private Row explanationRow;
	private List<Component> formElements = new ArrayList<Component>();
	private List<Component> requiredFormElements = new ArrayList<Component>();
	
	/**
	 * Creates a new word editor form.
	 * 
	 * @param title The title of the word editor form.
	 * @param parent The parent window.
	 * @param actionListener The action-listener.
	 */
	public WordEditorForm(String title, WindowPane parent, ActionListener actionListener) {
		this.title = title;
		this.parent = parent;
		this.actionListener = actionListener;
		
		Grid grid = new Grid(1);
		grid.setRowHeight(0, new Extent(parent.getHeight().getValue()-173));
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
		
		Row buttonBar = new Row();
		buttonBar.setAlignment(new Alignment(Alignment.RIGHT, Alignment.CENTER));
		buttonBar.setInsets(new Insets(10, 10, 10, 10));
		buttonBar.setCellSpacing(new Extent(5));
		okButton = new GeneralButton("OK", 70, this);
		buttonBar.add(okButton);
		cancelButton = new GeneralButton("Cancel", 70, this);
		buttonBar.add(cancelButton);
		grid.add(buttonBar);
		
		add(grid);
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
	 * Adds a new row to the form.
	 * 
	 * @param labelText The text for the label shown on the left hand side of the component.
	 * @param formElement The component, i.e. a text field.
	 * @param explanation An explanation text shown under the component.
	 * @param required Defines whether the component should be marked as required.
	 */
	public void addRow(String labelText, Component formElement, String explanation, boolean required) {
		Grid grid = new Grid(3);
		grid.setInsets(new Insets(0, 0, 5, 0));
		grid.setColumnWidth(0, new Extent(140));
		grid.add(new SolidLabel(labelText, Font.ITALIC));
		formElements.add(formElement);
		if (formElement instanceof TextField) {
			((TextField) formElement).setWidth(new Extent(parent.getWidth().getValue()-223));
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
	
	public void actionPerformed(ActionEvent e) {
		actionListener.actionPerformed(new ActionEvent(parent, e.getActionCommand()));
	}

}