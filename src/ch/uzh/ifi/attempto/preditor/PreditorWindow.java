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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nextapp.echo.app.Alignment;
import nextapp.echo.app.ApplicationInstance;
import nextapp.echo.app.Border;
import nextapp.echo.app.Button;
import nextapp.echo.app.Color;
import nextapp.echo.app.Column;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Grid;
import nextapp.echo.app.Insets;
import nextapp.echo.app.Row;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import nextapp.echo.app.event.WindowPaneEvent;
import nextapp.echo.app.event.WindowPaneListener;
import nextapp.echo.app.layout.GridLayoutData;
import ch.uzh.ifi.attempto.chartparser.ChartParser;
import ch.uzh.ifi.attempto.chartparser.ConcreteOption;
import ch.uzh.ifi.attempto.chartparser.DynamicLexicon;
import ch.uzh.ifi.attempto.chartparser.FeatureMap;
import ch.uzh.ifi.attempto.chartparser.Grammar;
import ch.uzh.ifi.attempto.chartparser.NextTokenOptions;
import ch.uzh.ifi.attempto.chartparser.Nonterminal;
import ch.uzh.ifi.attempto.chartparser.ParseTree;
import ch.uzh.ifi.attempto.echocomp.GeneralButton;
import ch.uzh.ifi.attempto.echocomp.Label;
import ch.uzh.ifi.attempto.echocomp.Logger;
import ch.uzh.ifi.attempto.echocomp.Style;
import ch.uzh.ifi.attempto.echocomp.TabSensitiveTextField;
import ch.uzh.ifi.attempto.echocomp.TextField;
import echopoint.DirectHtml;

//import static ch.uzh.ifi.attempto.echocomp.KeyStrokes.*;

/**
 * This class represents a predictive editor window. The predictive editor enables easy creation of texts that
 * comply with a certain grammar. The users can create such a text word-by-word by clicking on one of different
 * menu items. The menu items are structured into menu blocks each of which has a name that is displayed above
 * the menu block.
 * 
 * @author Tobias Kuhn
 */
public class PreditorWindow extends nextapp.echo.app.WindowPane implements ActionListener, WindowPaneListener {
	
	private static final long serialVersionUID = -7815494421993305554L;
	
	private final TextContainer textContainer = new TextContainer();
	private MenuCreator menuCreator;
	private TextOperator textOperator;
	private ChartParser parser;
	private List<ActionListener> actionListeners = new ArrayList<ActionListener>();
	private Logger logger;
	
	private MenuBlockManager menuBlockManager;
	private MenuBlock enlargedMenuBlock;
	
	private DirectHtml textArea = new DirectHtml();
	private TabSensitiveTextField textField;
	private TextField dummyTextField;
	private Column menuBlockArea;
	private GeneralButton deleteButton = new GeneralButton("< Delete", 70, this);
	private GeneralButton clearButton = new GeneralButton("Clear", 70, this);
	private Button okButton = new GeneralButton("OK", 70, this);
	private Button cancelButton = new GeneralButton("Cancel", 70, this);
	
	// TODO: reactive key combinations
//	private KeyStrokeListener keyStrokeListener = new KeyStrokeListener();

	private boolean isInitialized = false;
	
	/**
	 * Creates a new predictive editor window for the given grammar.
	 * 
	 * @param title The title of the window.
	 * @param grammar The grammar to be used.
	 * @param startCategoryName The name of the start category.
	 * @param context A list of forward references and scope openers that define the context.
	 */
	public PreditorWindow(String title, Grammar grammar, String startCategoryName,
			List<Nonterminal> context) {
		this.parser = new ChartParser(grammar, startCategoryName, context);
		
		this.menuBlockManager = new MenuBlockManager(this);
		
		addWindowPaneListener(this);
		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
		setModal(true);
		setTitle(title);
		setTitleFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(13)));
		setWidth(new Extent(753));
		setHeight(new Extent(523));
		setResizable(false);
		setTitleBackground(Style.windowTitleBackground);
		setStyleName("Default");
		
		Grid grid = new Grid(1);
		grid.setColumnWidth(0, new Extent(730));
		add(grid);
		
		GridLayoutData layout = new GridLayoutData();
		layout.setAlignment(new Alignment(Alignment.LEFT, Alignment.TOP));
		
		Column textAreaColumn = new Column();
		textAreaColumn.setInsets(new Insets(10, 10, 10, 0));
		textAreaColumn.add(textArea);
		textAreaColumn.setLayoutData(layout);
		grid.setRowHeight(0, new Extent(68));
		grid.add(textAreaColumn);
		
		Column textColumn = new Column();
		textColumn.setInsets(new Insets(10, 10, 0, 0));
		textColumn.setCellSpacing(new Extent(10));
		
		Row textAreaButtonBar = new Row();
		textAreaButtonBar.setAlignment(new Alignment(Alignment.RIGHT, Alignment.CENTER));
		textAreaButtonBar.setInsets(new Insets(0, 5, 10, 0));
		textAreaButtonBar.setCellSpacing(new Extent(5));
		clearButton.setVisible(false);
		textAreaButtonBar.add(clearButton);
		textAreaButtonBar.add(deleteButton);
		textColumn.add(textAreaButtonBar);
		
		Column textFieldColumn = new Column();
		textFieldColumn.setCellSpacing(new Extent(1));
		Label textFieldLabel = new Label("text", Font.ITALIC, 11);
		textFieldColumn.add(textFieldLabel);
		
		textField = new TabSensitiveTextField(this);
		textField.setWidth(new Extent(708));
		textField.setDisabledBackground(Style.lightDisabled);
		Row textFieldRow = new Row();
		textFieldRow.add(textField);
		dummyTextField = new TextField();
		dummyTextField.setWidth(new Extent(1));
		dummyTextField.setBorder(new Border(0, Color.BLACK, 0));
		dummyTextField.setBackground(Color.WHITE);
		textFieldRow.add(dummyTextField);
		textFieldColumn.add(textFieldRow);
		
//		keyStrokeListener.addKeyCombination(VK_TAB, "Tab");
//		keyStrokeListener.addKeyCombination(VK_ESCAPE, "Esc");
//		keyStrokeListener.addKeyCombination(VK_BACK_SPACE | CONTROL_MASK, "Ctrl-Backspace");
//		keyStrokeListener.addActionListener(this);
//		textFieldColumn.add(keyStrokeListener);
		
		textColumn.add(textFieldColumn);
		grid.setRowHeight(1, new Extent(88));
		grid.add(textColumn);
		
		menuBlockArea = new Column();
		menuBlockArea.setInsets(new Insets(10, 15, 0, 0));
		grid.setRowHeight(2, new Extent(265));
		grid.add(menuBlockArea);
		
		Row buttonBar = new Row();
		buttonBar.setAlignment(new Alignment(Alignment.RIGHT, Alignment.TOP));
		buttonBar.setInsets(new Insets(10, 12, 10, 0));
		buttonBar.setCellSpacing(new Extent(5));
		buttonBar.add(okButton);
		buttonBar.add(cancelButton);
		grid.setRowHeight(3, new Extent(30));
		grid.add(buttonBar);
		
		update();
	}
	
	/**
	 * Creates a new predictive editor window for the given grammar.
	 * 
	 * @param title The title of the window.
	 * @param grammar The grammar to be used.
	 * @param startCategoryName The name of the start category.
	 */
	public PreditorWindow(String title, Grammar grammar, String startCategoryName) {
		this(title, grammar, startCategoryName, null);
	}
	
	/**
	 * Sets the menu creator. {@link DefaultMenuCreator} is used by default.
	 * 
	 * @param menuCreator The menu creator.
	 */
	public void setMenuCreator(MenuCreator menuCreator) {
		this.menuCreator = menuCreator;
		update();
	}
	
	/**
	 * Returns the menu creator.
	 * 
	 * @return The menu creator.
	 */
	public MenuCreator getMenuCreator() {
		if (menuCreator == null) {
			setMenuCreator(new DefaultMenuCreator());
		}
		return menuCreator;
	}
	
	/**
	 * Sets the text operator. {@link DefaultTextOperator} is used by default.
	 * 
	 * @param textOperator The text operator.
	 */
	public void setTextOperator(TextOperator textOperator) {
		this.textOperator = textOperator;
		textContainer.setTextOperator(textOperator);
	}
	
	/**
	 * Returns the text operator.
	 * 
	 * @return The text operator.
	 */
	public TextOperator getTextOperator() {
		if (textOperator == null) {
			setTextOperator(new DefaultTextOperator());
		}
		return textOperator;
	}
	
	/**
	 * Sets the dynamic lexicon.
	 * 
	 * @param dynLexicon The dynamic lexicon.
	 */
	public void setDynamicLexicon(DynamicLexicon dynLexicon) {
		parser.setDynamicLexicon(dynLexicon);
		update();
	}
	
	/**
	 * Shows or hides the "clear" button.
	 * 
	 * @param visible true to show the "clear" button; false to hide it.
	 */
	public void setClearButtonVisible(boolean visible) {
		clearButton.setVisible(visible);
	}
	
	/**
	 * Returns a copy of the text container object that contains the (partial) text that has been
	 * entered.
	 * 
	 * @return A copy of the text container object.
	 */
	public TextContainer getTextContainer() {
		return textContainer.clone();
	}
	
	/**
	 * Returns the number of tokens of the current (partial) text.
	 * 
	 * @return The number of tokens.
	 */
	public int getTokenCount() {
		return parser.getTokenCount();
	}
	
	/**
	 * Returns whether the given token is a possible next token.
	 * 
	 * @param token The token.
	 * @return true if it is a possible next token.
	 */
	public boolean isPossibleNextToken(String token) {
		return parser.isPossibleNextToken(token);
	}
	
	/**
	 * Adds the text element to the end of the text.
	 * 
	 * @param te The text element to be added.
	 */
	public void addTextElement(TextElement te) {
		textElementSelected(te);
		textField.setText("");
		update();
	}
	
	/**
	 * Reads the text and adds it to the end of the current text as far as possible.
	 * 
	 * @param text The text to be added.
	 */
	public void addText(String text) {
		handleTextInput(text, true);
		update();
	}
	
	private void textElementSelected(TextElement te) {
		textContainer.addElement(te);
		parser.addToken(te.getOriginalText());
		
		log("words added: " + te);
	}
	
	private void update() {
		if (!isInitialized) return;
		updateMenuBlockContents();
		menuBlockArea.removeAll();
		menuBlockManager.setFilter(textField.getText());
		if (enlargedMenuBlock != null) {
			// One enlarged menu block
			MenuBlockContent mbc = enlargedMenuBlock.getContent();
			int cs = menuCreator.getColorShift(mbc.getName());
			enlargedMenuBlock = new MenuBlock(708, 240, cs, this);
			enlargedMenuBlock.setContent(mbc);
			enlargedMenuBlock.setEnlarged(true);
			menuBlockArea.add(enlargedMenuBlock);
		} else {
			menuBlockArea.add(menuBlockManager.createGUI());
		}
		textField.setEnabled(menuBlockManager.getMenuBlockCount() > 0 || !textField.getText().equals(""));
		ApplicationInstance.getActive().setFocusedComponent(textField);
		clearButton.setEnabled(getTokenCount() > 0);
		deleteButton.setEnabled(getTokenCount() > 0);
	}
	
	private void updateMenuBlockContents() {
		boolean[] r = new boolean[getTokenCount()];
		for (FeatureMap f : parser.getBackwardReferences()) {
			String s = f.getFeature("*pos").getString();
			if (s != null) {
				int i = new Integer(s) - 1;
				if (i > -1) {
					r[i] = true;
					r[getTokenCount()-1] = true;
				}
			}
		}
		String t = "";
		for (int i = 0; i < getTokenCount() ; i++) {
			TextElement te = textContainer.getTextElement(i);
			if (te.getText().matches("[.?!]")) {
				t += te.getText();
			} else if (r[i]) {
				t += " <u>" + te.getText() + "</u>";
			} else {
				t += " " + te.getText();
			}
		}
		if (t.startsWith(" ")) t = t.substring(1);
		textArea.setText("<div style=\"font-family: Verdana,Arial,Helvetica,Sans-Serif; font-size: 12px\">" +
				t + "<span style=\"color: rgb(150, 150, 150)\"> ...</span></div>");
		
		menuBlockManager.clear();
		
		NextTokenOptions options = parser.getNextTokenOptions();
		HashMap<String, MenuBlockContent> contentsMap = new HashMap<String, MenuBlockContent>();
		for (MenuItem m : getMenuCreator().createSpecialMenuItems(options)) {
			addMenuItem(m, contentsMap);
		}
		for (ConcreteOption o : options.getConcreteOptions()) {
			addMenuItem(getMenuCreator().createMenuEntry(o), contentsMap);
		}
		
		for (String mg : getMenuCreator().getMenuGroupOrdering()) {
			if (contentsMap.containsKey(mg)) {
				menuBlockManager.addMenuBlockContent(contentsMap.get(mg));
			}
		}
		for (String mg : contentsMap.keySet()) {
			if (!getMenuCreator().getMenuGroupOrdering().contains(mg)) {
				menuBlockManager.addMenuBlockContent(contentsMap.get(mg));
			}
		}
	}
	
	private void addMenuItem(MenuItem menuItem, Map<String, MenuBlockContent> contentsMap) {
		String menuGroup = menuItem.getMenuGroup();
		MenuBlockContent mbc;
		if (contentsMap.containsKey(menuGroup)) {
			mbc = contentsMap.get(menuGroup);
		} else {
			mbc = new MenuBlockContent(menuGroup);
			mbc.setComparator(getMenuCreator().getMenuItemComparator());
			mbc.setActionListener(this);
			contentsMap.put(menuGroup, mbc);
		}
		mbc.addItem(menuItem);
	}
	
	private void handleTextInput(boolean enterPressed) {
		handleTextInput(textField.getText(), enterPressed);
	}
	
	private void handleTextInput(String text, boolean enterPressed) {
		List<String> subtokens = getTextOperator().splitIntoTokens(text);
		boolean force = enterPressed && (text.equals(menuBlockManager.getFilter()) || text.endsWith(" "));
		handleTokenInput(subtokens, force, true);
	}
	
	private void handleTokenInput(List<String> subtokens, boolean force, boolean caseSensitive) {
		if (subtokens.size() == 0) {
			textField.setText("");
			return;
		}
		
		String filter = "";
		for (String s : subtokens) filter += s + " ";
		menuBlockManager.setFilter(filter);
		
		String text = "";
		TextElement textElement = null;
		List<String> rest = null;
		List<String> s = new ArrayList<String>(subtokens);
		while (s.size() > 0) {
			if (text.length() > 0) text += " ";
			text += s.remove(0);
			TextElement te = null;
			if (caseSensitive) {
				te = getTextOperator().createTextElement(text);
			} else {
				String t = proposeToken(text);
				if (t != null) {
					te = getTextOperator().createTextElement(t);
				}
			}
			if (te != null && parser.isPossibleNextToken(te.getOriginalText())) {
				textElement = te;
				rest = new ArrayList<String>(s);
			}
		}
		if (textElement != null) {
			if ((rest.isEmpty() && force) || (!rest.isEmpty() && menuBlockManager.getMenuEntryCount() == 0)) {
				textContainer.addElement(textElement);
				parser.addToken(textElement.getOriginalText());
				updateMenuBlockContents();
				handleTokenInput(rest, force, caseSensitive);
				return;
			}
		}
		if (caseSensitive) {
			handleTokenInput(subtokens, force, false);
		} else {
			textField.setText(text);
		}
	}
	
	private String proposeToken(String text) {
		text = text.toLowerCase().replaceAll("\\s+", "_");
		for (ConcreteOption o : parser.getConcreteOptions()) {
			String t = o.getWord().toLowerCase().replaceAll("\\s+", "_");
			if (t.equals(text)) {
				return o.getWord();
			}
		}
		return null;
	}
	
	/**
	 * Adds a new action-listener.
	 * 
	 * @param actionListener The new action-listener.
	 */
	public void addActionListener(ActionListener actionListener) {
		actionListeners.add(actionListener);
	}
	
	/**
	 * Removes the action-listener.
	 * 
	 * @param actionListener The action-listener to be removed.
	 */
	public void removeActionListener(ActionListener actionListener) {
		actionListeners.remove(actionListener);
	}
	
	/**
	 * Removes all action-listeners.
	 */
	public void removeAllActionListeners() {
		actionListeners.clear();
	}
	
	private void notifyActionListeners(ActionEvent event) {
		for (ActionListener al : actionListeners) {
			al.actionPerformed(event);
		}
	}
	
	public void actionPerformed(ActionEvent e) {
		Object src = e.getSource();
		String c = e.getActionCommand();

		if (enlargedMenuBlock != null) {
			enlargedMenuBlock.setEnlarged(false);
			enlargedMenuBlock = null;
		}
		
		boolean tabKeyPressed = false;
		
		if (src == cancelButton) {
			log("pressed: cancel");
			notifyActionListeners(new ActionEvent(this, "Cancel"));
			return;
		} else if (src == okButton) {
			log("pressed: ok");
			handleTextInput(true);
			update();
			notifyActionListeners(new ActionEvent(this, "OK"));
			return;
		} else if (src == deleteButton) {
			log("pressed: < delete");
			removeLastToken();
		} else if (src == clearButton) {
			log("pressed: clear");
			clearTokens();
		} else if (src == textField) {
			if (getApplicationInstance().getFocusedComponent() == dummyTextField) {
				log("pressed: tab-key");
				handleTextInput(false);
				tabKeyPressed = true;
			} else {
				log("pressed: enter-key");
				if (textField.getText().equals("") && menuBlockManager.getFilter().equals("")) {
					notifyActionListeners(new ActionEvent(this, "Enter"));
					return;
				} else {
					handleTextInput(true);
				}
			}
		} else if (src instanceof MenuEntry) {
			TextElement te = ((MenuEntry) e.getSource()).getTextElement();
			log("pressed: menu-entry " + te.getText());
			textElementSelected(te);
			textField.setText("");
		} else if ("enlarge".equals(c) && src instanceof MenuBlock) {
			enlargedMenuBlock = (MenuBlock) src;
		} else if ("Esc".equals(c)) {
			log("pressed: escape key");
			notifyActionListeners(new ActionEvent(this, "Escape"));
			return;
		} else if ("Ctrl-Backspace".equals(c)) {
			log("pressed: ctrl-backspace");
			if (getTokenCount() > 0) {
				textContainer.removeLastElement();
				parser.removeToken();
				textField.setText("");
			}
		}
		
		update();
		
		if (tabKeyPressed) {
			String s = menuBlockManager.getStartString();
			if (s != null) textField.setText(s);
		}
	}
	
	/**
	 * Removes the last token.
	 */
	public void removeLastToken() {
		if (getTokenCount() > 0) {
			textContainer.removeLastElement();
			parser.removeToken();
			textField.setText("");
		}
	}
	
	/**
	 * Removes all tokens.
	 */
	public void clearTokens() {
		textContainer.removeAllElements();
		parser.removeAllTokens();
		textField.setText("");
	}
	
	/**
	 * Returns true if the current text is a complete statement.
	 * 
	 * @return true if the current text is a complete statement.
	 */
	public boolean isTextComplete() {
		return parser.isComplete();
	}
	
	/**
	 * Returns the parse tree of the text for the given category if a complete parse tree exists.
	 * Null is returned for uncomplete texts.
	 * 
	 * @param categoryName The category name.
	 * @return The parse tree.
	 */
	public ParseTree getParseTree(String categoryName) {
		return parser.getParseTree(categoryName);
	}
	
	/**
	 * Returns the parse tree of the text for the start category if a complete parse tree exists.
	 * Null is returned for uncomplete texts.
	 * 
	 * @return The parse tree.
	 */
	public ParseTree getParseTree() {
		return parser.getParseTree();
	}
	
	public void windowPaneClosing(WindowPaneEvent e) {
		log("pressed: close window");
		notifyActionListeners(new ActionEvent(this, "Close"));
	}
	
	public void init() {
		isInitialized = true;
		update();
		super.init();
	}

	/**
	 * Sets the logger.
	 * 
	 * @param logger The logger object or null.
	 */
	public void setLogger(Logger logger) {
		this.logger = logger;
	}
	
	private void log(String text) {
		if (logger != null) {
			logger.log("pred", text);
		}
	}
	
	public String toString() {
		return "sentence: " + textContainer.getText();
	}
	
}
