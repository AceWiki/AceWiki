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

import nextapp.echo2.app.Alignment;
import nextapp.echo2.app.ApplicationInstance;
import nextapp.echo2.app.Border;
import nextapp.echo2.app.Button;
import nextapp.echo2.app.Color;
import nextapp.echo2.app.Column;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.Row;
import nextapp.echo2.app.SplitPane;
import nextapp.echo2.app.event.ActionEvent;
import nextapp.echo2.app.event.ActionListener;
import nextapp.echo2.app.event.WindowPaneEvent;
import nextapp.echo2.app.event.WindowPaneListener;
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
import ch.uzh.ifi.attempto.echocomp.TextField;
import ch.uzh.ifi.attempto.echocomp.WindowPane;
import echopointng.DirectHtml;
import echopointng.KeyStrokeListener;
import echopointng.KeyStrokes;

/**
 * This class represents a predictive editor window. The predictive editor enables easy creation of texts that
 * comply with a certain grammar. The users can create such a text word-by-word by clicking on one of different
 * menu items. The menu items are structured into menu blocks each of which has a name that is displayed above
 * the menu block.
 * 
 * @author Tobias Kuhn
 */
public class PreditorWindow extends WindowPane implements ActionListener, WindowPaneListener,
		KeyStrokes {
	
	private static final long serialVersionUID = -7815494421993305554L;
	
	private final TextContainer textContainer = new TextContainer();
	private MenuCreator menuCreator;
	private TextOperator textOperator;
	private ChartParser parser;
	private String filter = "";
	private List<ActionListener> actionListeners = new ArrayList<ActionListener>();
	private Logger logger;
	
	private List<MenuBlockContent> menuBlockContents = new ArrayList<MenuBlockContent>();
	private List<MenuBlock> menuBlocksTop = new ArrayList<MenuBlock>();
	private List<MenuBlock> menuBlocksBottom = new ArrayList<MenuBlock>();
	private List<SplitPane> menuSplitPanesTop = new ArrayList<SplitPane>();
	private List<SplitPane> menuSplitPanesBottom = new ArrayList<SplitPane>();
	private MenuBlock enlargedMenuBlock;
	
	private DirectHtml textArea = new DirectHtml();
	private TextField textField;
	private SplitPane menuBlockPane;
	private SplitPane doubleColumnMenuPane;
	private GeneralButton deleteButton = new GeneralButton("< Delete", 70, this);
	private GeneralButton clearButton = new GeneralButton("Clear", 70, this);
	private Button okButton = new GeneralButton("OK", 70, this);
	private Button cancelButton = new GeneralButton("Cancel", 70, this);
	private KeyStrokeListener keyStrokeListener = new KeyStrokeListener();

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
		
		Row buttonBar = new Row();
		buttonBar.setAlignment(new Alignment(Alignment.RIGHT, Alignment.CENTER));
		buttonBar.setInsets(new Insets(10, 17, 10, 10));
		buttonBar.setCellSpacing(new Extent(5));
		buttonBar.add(okButton);
		buttonBar.add(cancelButton);
		
		SplitPane splitPane = new SplitPane(SplitPane.ORIENTATION_VERTICAL_BOTTOM_TOP, new Extent(47));
		splitPane.add(buttonBar);
		add(splitPane);
		
		SplitPane editorPane = new SplitPane(SplitPane.ORIENTATION_VERTICAL_TOP_BOTTOM, new Extent(168));
		SplitPane textPane = new SplitPane(SplitPane.ORIENTATION_VERTICAL_TOP_BOTTOM, new Extent(70));
		
		textArea.setFocusTraversalParticipant(false);
		SplitPane textAreaBorderTop = new SplitPane(SplitPane.ORIENTATION_VERTICAL_TOP_BOTTOM, new Extent(10));
		SplitPane textAreaBorderLeft = new SplitPane(SplitPane.ORIENTATION_HORIZONTAL_LEFT_RIGHT, new Extent(10));
		SplitPane textAreaBorderRight = new SplitPane(SplitPane.ORIENTATION_HORIZONTAL_RIGHT_LEFT, new Extent(10));
		textAreaBorderTop.add(new Label());
		textAreaBorderTop.add(textAreaBorderLeft);
		textAreaBorderLeft.add(new Label());
		textAreaBorderLeft.add(textAreaBorderRight);
		textAreaBorderRight.add(new Label());
		textAreaBorderRight.add(textArea);
		textPane.add(textAreaBorderTop);
		
		Column textColumn = new Column();
		textColumn.setInsets(new Insets(10, 10, 0, 0));
		textColumn.setCellSpacing(new Extent(10));
		
		Row textAreaButtonBar = new Row();
		textAreaButtonBar.setAlignment(new Alignment(Alignment.RIGHT, Alignment.CENTER));
		textAreaButtonBar.setInsets(new Insets(0, 5, 10, 0));
		textAreaButtonBar.setCellSpacing(new Extent(5));
		clearButton.setVisible(false);
		clearButton.setFocusTraversalParticipant(false);
		deleteButton.setFocusTraversalParticipant(false);
		textAreaButtonBar.add(clearButton);
		textAreaButtonBar.add(deleteButton);
		textColumn.add(textAreaButtonBar);
		
		Column textFieldColumn = new Column();
		textFieldColumn.setCellSpacing(new Extent(1));
		Label textFieldLabel = new Label("text", Font.ITALIC, 11);
		textFieldColumn.add(textFieldLabel);
		
		textField = new TextField(this);
		textField.setWidth(new Extent(708));
		textField.setFocusTraversalParticipant(true);
		textField.setFocusTraversalIndex(0);
		textField.setDisabledBackground(Style.lightDisabled);
		Row textFieldRow = new Row();
		textFieldRow.add(textField);
		TextField dummyTextField = new TextField();
		dummyTextField.setWidth(new Extent(1));
		dummyTextField.setBorder(new Border(0, null, 0));
		dummyTextField.setBackground(Color.WHITE);
		textFieldRow.add(dummyTextField);
		textFieldColumn.add(textFieldRow);
		
		keyStrokeListener.addKeyCombination(VK_TAB, "Tab");
		keyStrokeListener.addKeyCombination(VK_ESCAPE, "Esc");
		keyStrokeListener.addKeyCombination(VK_BACK_SPACE | CONTROL_MASK, "Ctrl-Backspace");
		keyStrokeListener.addActionListener(this);
		textFieldColumn.add(keyStrokeListener);
		
		textColumn.add(textFieldColumn);
		textPane.add(textColumn);
		editorPane.add(textPane);
		
		menuBlockPane = new SplitPane(SplitPane.ORIENTATION_HORIZONTAL_LEFT_RIGHT, new Extent(0));
		menuBlockPane.setSeparatorWidth(new Extent(10));
		menuBlockPane.setSeparatorColor(Color.WHITE);
		menuBlockPane.add(new Label());
		
		doubleColumnMenuPane =  new SplitPane(SplitPane.ORIENTATION_VERTICAL_TOP_BOTTOM, new Extent(258));
		SplitPane parentSplitPane = doubleColumnMenuPane;
		for (int i=0; i<10; i++) {
			MenuBlock menuBlock = new MenuBlock(this, this);
			menuBlocksTop.add(menuBlock);
			SplitPane menuSplitPane = new SplitPane(SplitPane.ORIENTATION_HORIZONTAL_LEFT_RIGHT);
			menuSplitPane.setSeparatorWidth(new Extent(10));
			menuSplitPane.setSeparatorColor(Color.WHITE);
			menuSplitPane.setVisible(false);
			menuSplitPane.add(menuBlock);
			menuSplitPanesTop.add(menuSplitPane);
			parentSplitPane.add(menuSplitPane);
			parentSplitPane = menuSplitPane;
		}
		
		parentSplitPane = doubleColumnMenuPane;
		for (int i=0; i<10; i++) {
			MenuBlock menuBlock = new MenuBlock(this, this);
			menuBlocksBottom.add(menuBlock);
			SplitPane menuSplitPane = new SplitPane(SplitPane.ORIENTATION_HORIZONTAL_LEFT_RIGHT);
			menuSplitPane.setSeparatorWidth(new Extent(10));
			menuSplitPane.setSeparatorColor(Color.WHITE);
			menuSplitPane.setVisible(false);
			menuSplitPane.add(menuBlock);
			menuSplitPanesBottom.add(menuSplitPane);
			parentSplitPane.add(menuSplitPane);
			parentSplitPane = menuSplitPane;
		}
		
		doubleColumnMenuPane.setSeparatorHeight(new Extent(12));
		doubleColumnMenuPane.setSeparatorColor(Color.WHITE);
		menuBlockPane.add(doubleColumnMenuPane);
		editorPane.add(menuBlockPane);
		splitPane.add(editorPane);
		
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
	 * Returns the (partial) text that has been entered .
	 * 
	 * @return The (partial) text in the form of a text container.
	 */
	public TextContainer getTextContainer() {
		return textContainer;
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
	
	private int getEntryCount() {
		int c = 0;
		for (MenuBlockContent mc : menuBlockContents) {
			c += mc.getEntryCount();
		}
		return c;
	}
	
	private String getStartString() {
		String startString = "";
		List<String> blockStartStrings = new ArrayList<String>();
		
		for (MenuBlockContent mc : menuBlockContents) {
			String s = mc.getStartString();
			if (s != null) {
				blockStartStrings.add(s);
			}
		}
		
		if (blockStartStrings.isEmpty()) return null;
		
		String first = blockStartStrings.get(0);
		blockStartStrings.remove(0);
		
		if (blockStartStrings.isEmpty()) return first;
		
		for (int i = 0; i < first.length(); i++) {
			char c = first.charAt(i);
			boolean stop = false;
			for (String s : blockStartStrings) {
				if (s.length() <= i || s.charAt(i) != c) stop = true;
			}
			if (stop) break;
			startString += c;
		}
		
		return startString;
	}
	
	private void update() {
		if (!isInitialized) return;
		updateMenuBlockContents();
		setFilter(textField.getText());
		int mbCount = menuBlockContents.size();
		if (enlargedMenuBlock != null) {
			// One enlarged menu block
			MenuBlockContent mbc = enlargedMenuBlock.getContent();
			clearMenuBlocks();
			enlargedMenuBlock = menuBlocksTop.get(0);
			enlargedMenuBlock.setContent(mbc, 710, 16);
			enlargedMenuBlock.setEnlarged(true);
			menuSplitPanesTop.get(0).setSeparatorPosition(new Extent(710));
			menuSplitPanesTop.get(0).setVisible(true);
			for (int i=1; i < menuBlocksTop.size(); i++) {
				menuSplitPanesTop.get(i).setVisible(false);
			}
			for (int i=0; i < menuBlocksBottom.size(); i++) {
				menuSplitPanesBottom.get(i).setVisible(false);
			}
			doubleColumnMenuPane.setSeparatorPosition(new Extent(258));
		} else if (mbCount < 5) {
			// Menu blocks on one row
			int width = ( 720 / ( mbCount > 3 ? mbCount : 3 ) ) - 10;
			for (int i=0; i < menuBlocksTop.size(); i++) {
				if (menuBlockContents.size() > i) {
					menuBlocksTop.get(i).setContent(menuBlockContents.get(i), width, 16);
					menuSplitPanesTop.get(i).setSeparatorPosition(new Extent(width));
					menuSplitPanesTop.get(i).setVisible(true);
				} else {
					menuSplitPanesTop.get(i).setVisible(false);
				}
			}
			for (int i=0; i < menuBlocksBottom.size(); i++) {
				menuSplitPanesBottom.get(i).setVisible(false);
			}
			doubleColumnMenuPane.setSeparatorPosition(new Extent(258));
		} else {
			// Menu blocks on two rows
			int firstRowCount = (mbCount + 1) / 2;
			int width = ( 720 / firstRowCount ) - 10;
			for (int i=0; i < menuBlocksTop.size(); i++) {
				if (i < firstRowCount) {
					menuBlocksTop.get(i).setContent(menuBlockContents.get(i), width, 7);
					menuSplitPanesTop.get(i).setSeparatorPosition(new Extent(width));
					menuSplitPanesTop.get(i).setVisible(true);
				} else {
					menuSplitPanesTop.get(i).setVisible(false);
				}
			}
			for (int i=0; i < menuBlocksBottom.size(); i++) {
				if (firstRowCount + i < mbCount) {
					menuBlocksBottom.get(i).setContent(menuBlockContents.get(firstRowCount + i), width, 7);
					menuSplitPanesBottom.get(i).setSeparatorPosition(new Extent(width));
					menuSplitPanesBottom.get(i).setVisible(true);
				} else {
					menuSplitPanesBottom.get(i).setVisible(false);
				}
			}
			doubleColumnMenuPane.setSeparatorPosition(new Extent(123));
		}
		textField.setEnabled(menuBlockContents.size() > 0 || !textField.getText().equals(""));
		ApplicationInstance.getActive().setFocusedComponent(textField);
		clearButton.setEnabled(getTokenCount() > 0);
		deleteButton.setEnabled(getTokenCount() > 0);
	}
	
	private void clearMenuBlocks() {
		for (MenuBlock mb : menuBlocksTop) {
			mb.clear();
		}
		for (MenuBlock mb : menuBlocksBottom) {
			mb.clear();
		}
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
		
		menuBlockContents.clear();
		
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
				addMenuBlockContent(mg, contentsMap);
			}
		}
		for (String mg : contentsMap.keySet()) {
			if (!getMenuCreator().getMenuGroupOrdering().contains(mg)) {
				addMenuBlockContent(mg, contentsMap);
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
	
	private void addMenuBlockContent(String menuGroup, Map<String, MenuBlockContent> contentsMap) {
		MenuBlockContent m = contentsMap.get(menuGroup);
		if (!m.isEmpty()) {
			menuBlockContents.add(m);
		}
	}
	
	private void setFilter(String filter) {
		if (filter == null) filter = "";
		filter = filter.replaceFirst("^\\s*", "").replaceFirst("\\s*$", "");
		
		for (MenuBlockContent c : menuBlockContents) {
			c.setFilter(filter);
		}
		this.filter = filter;
	}
	
	private void setFilter(List<String> subtokens) {
		String filter = "";
		for (String s : subtokens) {
			filter += s + " ";
		}
		setFilter(filter);
	}
	
	private void handleTextInput(boolean enterPressed) {
		handleTextInput(textField.getText(), enterPressed);
	}
	
	private void handleTextInput(String text, boolean enterPressed) {
		List<String> subtokens = getTextOperator().splitIntoTokens(text);
		boolean force = enterPressed && (text.equals(filter) || text.endsWith(" "));
		handleTokenInput(subtokens, force, true);
	}
	
	private void handleTokenInput(List<String> subtokens, boolean force, boolean caseSensitive) {
		if (subtokens.size() == 0) {
			textField.setText("");
			return;
		}
		setFilter(subtokens);
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
			if ((rest.isEmpty() && force) || (!rest.isEmpty() && getEntryCount() == 0)) {
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
			log("pressed: enter-key");
			if (textField.getText().equals("") && filter.equals("")) {
				notifyActionListeners(new ActionEvent(this, "Enter"));
				return;
			} else {
				handleTextInput(true);
			}
		} else if (src instanceof MenuEntry) {
			TextElement te = ((MenuEntry) e.getSource()).getTextElement();
			log("pressed: menu-entry " + te.getText());
			textElementSelected(te);
			textField.setText("");
		} else if ("enlarge".equals(c) && src instanceof MenuBlock) {
			enlargedMenuBlock = (MenuBlock) src;
		} else if ("Tab".equals(c)) {
			log("pressed: tab-key");
			handleTextInput(false);
			tabKeyPressed = true;
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
			String s = getStartString();
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
