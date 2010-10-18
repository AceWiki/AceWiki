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
import java.util.Arrays;
import java.util.List;

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
import ch.uzh.ifi.attempto.chartparser.FeatureMap;
import ch.uzh.ifi.attempto.chartparser.Grammar;
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
	
	private TextContainer textContainer = new TextContainer();
	private MenuCreator menuCreator;
	private ChartParser parser;
	private String filter = "";
	private List<ActionListener> actionListeners = new ArrayList<ActionListener>();
	private Logger logger;
	
	private List<MenuBlockContent> menuBlockContents = new ArrayList<MenuBlockContent>();
	private List<MenuBlock> menuBlocksTop = new ArrayList<MenuBlock>();
	private List<MenuBlock> menuBlocksBottom = new ArrayList<MenuBlock>();
	private List<SplitPane> menuSplitPanesTop = new ArrayList<SplitPane>();
	private List<SplitPane> menuSplitPanesBottom = new ArrayList<SplitPane>();
	
	private DirectHtml textArea = new DirectHtml();
	private TextField textField;
	private SplitPane menuBlockPane;
	private SplitPane doubleColumnMenuPane;
	private GeneralButton deleteButton = new GeneralButton("< Delete", 70, this);
	private GeneralButton clearButton = new GeneralButton("Clear", 70, this);
	private Button okButton = new GeneralButton("OK", 70, this);
	private Button cancelButton = new GeneralButton("Cancel", 70, this);
	private KeyStrokeListener keyStrokeListener = new KeyStrokeListener();
	
	/**
	 * Creates a new predictive editor window for the given grammar using the given menu creator.
	 * 
	 * @param title The title of the window.
	 * @param grammar The grammar to be used.
	 * @param startCategoryName The name of the start category.
	 * @param context A list of forward references and scope openers that define the context.
	 * @param menuCreator The menu creator to be used.
	 */
	public PreditorWindow(String title, Grammar grammar, String startCategoryName, 
			List<Nonterminal> context, MenuCreator menuCreator) {
		this.parser = new ChartParser(grammar, startCategoryName, context);
		this.menuCreator = menuCreator;
		
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
	 * Creates a new predictive editor window for the given grammar using the given menu creator.
	 * 
	 * @param title The title of the window.
	 * @param grammar The grammar to be used.
	 * @param startCategoryName The name of the start category.
	 * @param menuCreator The menu creator to be used.
	 */
	public PreditorWindow(String title, Grammar grammar, String startCategoryName, 
			MenuCreator menuCreator) {
		this(title, grammar, startCategoryName, null, menuCreator);
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
	 * Sets the context checker.
	 * 
	 * @param contextChecker The context checker.
	 */
	public void setContextChecker(ContextChecker contextChecker) {
		textContainer.setContextChecker(contextChecker);
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
	 * Returns a list of text elements that contain one of the given texts and that are
	 * possible next tokens.
	 * 
	 * @param text The content of the text elements to search for.
	 * @return The list of text elements.
	 */
	public List<TextElement> getPossibleNextTokens(String... text) {
		List<TextElement> l = new ArrayList<TextElement>();
		for (MenuBlockContent m : menuBlockContents) {
			for (String s : text) {
				MenuEntry e = m.getEntry(s);
				if (e != null) l.add(e.getTextElement());
			}
		}
		return l;
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
		handleTextInput(tokenize(text));
		update();
	}
	
	private void textElementSelected(TextElement te) {
		ArrayList<TextElement> l = new ArrayList<TextElement>();
		l.add(te);
		textElementSelected(l);
	}
	
	private void textElementSelected(List<TextElement> textElements) {
		if (textElements.isEmpty()) return;
		TextElement te = textElements.get(0);
		for (int i = 1 ; i < textElements.size() ; i++) {
			te.include(textElements.get(i));
		}
		
		textContainer.addElement(te);
		parser.addToken(te.getOriginalText(), te.getCategories());
		
		log("words added: " + te);
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
		updateMenuBlockContents();
		setFilter(textField.getText());
		int mbCount = menuBlockContents.size();
		if (mbCount < 5) {
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
		List<MenuBlockContent> newContents = menuCreator.createMenu(parser.getNextTokenOptions());
		for (MenuBlockContent c : newContents) {
			if (!c.isEmpty()) {
				menuBlockContents.add(c);
			}
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
	
	private void handleTextInput() {
		handleTextInput(tokenize(textField.getText()));
	}
	
	private void handleTextInput(List<String> textList) {
		
		List<TextElement> recognizedElements = null;
		int recognizedElementLength = 0;
		String text = "";
		
		for (int pos = 0 ; pos < textList.size() ; pos++) {
			if (text.length() > 0) text += " ";
			text += textList.get(pos);
			
			setFilter(text);
			List<TextElement> potentialElements = getPossibleNextTokens(text);
			if (potentialElements.isEmpty()) {
				potentialElements = null;
			}
			
			// Counting how many different menu entries are available under the current filter.
			// elCount=2 means two or more entries.
			int elCount = 0;
			String firstElementText = null;
			for (MenuBlockContent m : menuBlockContents) {
				for (MenuEntry e : m.getEntries()) {
					String thisElementText = e.getTextElement().getText();
					if (firstElementText == null) {
						firstElementText = thisElementText;
						elCount = 1;
					} else if (!firstElementText.equals(thisElementText)) {
						elCount = 2;
						break;
					}
				}
				if (elCount == 2) break;
			}
			
			boolean atLastPosition = pos == textList.size()-1;
			
			if (potentialElements != null && !atLastPosition) {
				recognizedElements = potentialElements;
				recognizedElementLength = pos + 1;
				if (elCount == 1) break;
			}
			if (elCount == 0) {
				break;
			} else if (atLastPosition) {
				recognizedElements = null;
				break;
			}
		}
		
		setFilter(null);
		
		if (recognizedElements != null) {
			textElementSelected(recognizedElements);
			updateMenuBlockContents();
			for (int i = 0; i < recognizedElementLength; i++) {
				textList.remove(0);
			}
			handleTextInput(textList);
		} else {
			text = "";
			for (String textPart : textList) {
				if (text.length() > 0) text += " ";
				text += textPart;
			}
			textField.setText(text);
		}
	}
	
	private ArrayList<String> tokenize(String text) {
		text = text.replaceAll("\\.\\s", " . ");
		text = text.replaceAll("\\.$", " .");
		text = text.replaceAll("\\?\\s", " ? ");
		text = text.replaceAll("\\?$", " ?");
		text = text.replaceAll("\\!\\s", " ! ");
		text = text.replaceAll("\\!$", " !");
		
		ArrayList<String> tokens = new ArrayList<String>(Arrays.asList(text.split(" ")));
		
		while (tokens.contains("")) {
			tokens.remove("");
		}
		
		if (text.endsWith(" ")) {
			tokens.add("");
		}
		
		return tokens;
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
		if (e.getSource() == cancelButton) {
			log("pressed: cancel");
			notifyActionListeners(new ActionEvent(this, "Cancel"));
			return;
		} else if (e.getSource() == okButton) {
			log("pressed: ok");
			handleTextInput();
			update();
			notifyActionListeners(new ActionEvent(this, "OK"));
			return;
		} else if (e.getSource() == deleteButton) {
			log("pressed: < delete");
			removeLastToken();
		} else if (e.getSource() == clearButton) {
			log("pressed: clear");
			clearTokens();
		} else if (e.getSource() instanceof MenuEntry) {
			TextElement te = ((MenuEntry) e.getSource()).getTextElement();
			log("pressed: menu-entry " + te.getText());
			textElementSelected(te);
			textField.setText("");
		} else if (e.getSource() == textField) {
			log("pressed: enter-key");
			if (textField.getText().equals("") && filter.equals("")) {
				notifyActionListeners(new ActionEvent(this, "OK"));
				return;
			} else {
				handleTextInput();
				List<TextElement> te = getPossibleNextTokens(textField.getText());
				if (!te.isEmpty()) {
					textElementSelected(te);
					textField.setText("");
				}
			}
		} else if ("Tab".equals(e.getActionCommand())) {
			log("pressed: tab-key");
			handleTextInput();
		} else if ("Esc".equals(e.getActionCommand())) {
			log("pressed: escape key");
			notifyActionListeners(new ActionEvent(this, "Cancel"));
			return;
		} else if ("Ctrl-Backspace".equals(e.getActionCommand())) {
			log("pressed: ctrl-backspace");
			if (getTokenCount() > 0) {
				textContainer.removeLastElement();
				parser.removeToken();
				textField.setText("");
			}
		}
		
		update();
		
		if ("Tab".equals(e.getActionCommand())) {
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
	 * Returns the parse tree of the text if it is complete. Null is returned for uncomplete texts.
	 * 
	 * @return The parse tree.
	 */
	public ParseTree getParseTree() {
		return parser.getParseTree();
	}
	
	public void windowPaneClosing(WindowPaneEvent e) {
		log("pressed: close window");
		notifyActionListeners(new ActionEvent(this, "Cancel"));
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
