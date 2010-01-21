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

package ch.uzh.ifi.attempto.acewiki;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Random;
import java.util.Stack;

import nextapp.echo2.app.Alignment;
import nextapp.echo2.app.ApplicationInstance;
import nextapp.echo2.app.Color;
import nextapp.echo2.app.Column;
import nextapp.echo2.app.ContentPane;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.ResourceImageReference;
import nextapp.echo2.app.Row;
import nextapp.echo2.app.SplitPane;
import nextapp.echo2.app.TaskQueueHandle;
import nextapp.echo2.app.event.ActionEvent;
import nextapp.echo2.app.event.ActionListener;
import nextapp.echo2.webcontainer.ContainerContext;
import ch.uzh.ifi.attempto.acewiki.core.AceWikiGrammar;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Individual;
import ch.uzh.ifi.attempto.acewiki.core.ontology.NounConcept;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OfRole;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OntologyTextElement;
import ch.uzh.ifi.attempto.acewiki.core.ontology.TrAdjRole;
import ch.uzh.ifi.attempto.acewiki.core.ontology.VerbRole;
import ch.uzh.ifi.attempto.acewiki.gui.ExportWindow;
import ch.uzh.ifi.attempto.acewiki.gui.ListItem;
import ch.uzh.ifi.attempto.acewiki.gui.editor.NounForm;
import ch.uzh.ifi.attempto.acewiki.gui.editor.NounOfForm;
import ch.uzh.ifi.attempto.acewiki.gui.editor.ProperNameForm;
import ch.uzh.ifi.attempto.acewiki.gui.editor.TrAdjForm;
import ch.uzh.ifi.attempto.acewiki.gui.editor.VerbForm;
import ch.uzh.ifi.attempto.acewiki.gui.page.ArticlePage;
import ch.uzh.ifi.attempto.acewiki.gui.page.IndexPage;
import ch.uzh.ifi.attempto.acewiki.gui.page.SearchPage;
import ch.uzh.ifi.attempto.acewiki.gui.page.StartPage;
import ch.uzh.ifi.attempto.acewiki.gui.page.WikiPage;
import ch.uzh.ifi.attempto.chartparser.Grammar;
import ch.uzh.ifi.attempto.echocomp.GeneralButton;
import ch.uzh.ifi.attempto.echocomp.Label;
import ch.uzh.ifi.attempto.echocomp.Logger;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;
import ch.uzh.ifi.attempto.echocomp.SmallButton;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import ch.uzh.ifi.attempto.echocomp.Style;
import ch.uzh.ifi.attempto.echocomp.TextAreaWindow;
import ch.uzh.ifi.attempto.echocomp.TextField;
import ch.uzh.ifi.attempto.echocomp.VSpace;
import ch.uzh.ifi.attempto.echocomp.WindowPane;
import ch.uzh.ifi.attempto.preditor.PreditorWindow;
import ch.uzh.ifi.attempto.preditor.WordEditorWindow;
import echopointng.ExternalEventMonitor;
import echopointng.externalevent.ExternalEvent;
import echopointng.externalevent.ExternalEventListener;

/**
 * This class represents an AceWiki wiki instance (including its graphical user interface). There
 * is such a wiki object for every wiki user.
 * 
 * @author Tobias Kuhn
 */
public class Wiki implements ActionListener, ExternalEventListener {
	
	private static final long serialVersionUID = 2777443689044226043L;
	
	private Map<String, String> parameters;

	private final Ontology ontology;
	
	private WikiPage currentPage;
	private ContentPane mainPane = new ContentPane();
	private ContentPane contentPane = new ContentPane();
	private Row navigationButtons = new Row();
	private Logger logger;
	
	private GeneralButton backButton = new GeneralButton("<Back", this);
	private GeneralButton forwardButton = new GeneralButton("Forward>", this);
	private GeneralButton refreshButton = new GeneralButton("Refresh", this);
	
	private SmallButton indexButton = new SmallButton("Index", this, 12);
	private SmallButton homeButton = new SmallButton("Main Page", this, 12);
	private SmallButton randomButton = new SmallButton("Random Article", this, 12);
	private SmallButton searchButton = new SmallButton("Search:", this, 12);
	private TextField searchTextField = new TextField(110, this);
	private SmallButton newButton = new SmallButton("New Word...", this, 12);
	private SmallButton exportButton = new SmallButton("Export...", this, 12);
	private SmallButton logoutButton = new SmallButton("Logout", this, 12);
	private ListItem logoutListItem;
	private Label logo;
	
	private StartPage startPage;
	
	private Stack<WikiPage> history = new Stack<WikiPage>();
	private Stack<WikiPage> forward = new Stack<WikiPage>();
	
	private List<WindowPane> windows = new ArrayList<WindowPane>();
	
	private Grammar grammar = new AceWikiGrammar();
	
	private TaskQueueHandle taskQueue;
	private MessageWindow waitWindow;
	private List<Task> strongTasks = new ArrayList<Task>();
	private List<Task> weakTasks = new ArrayList<Task>();
	
	private ApplicationInstance application;
	
	private static Properties properties;
	
	/**
	 * Creates a new wiki instance.
	 * 
	 * @param parameters A set of parameters in the form of name/value pairs.
	 * @param sessionID The session id.
	 */
	Wiki(Map<String, String> parameters, int sessionID) {
		this.parameters = parameters;
		
		ontology = Ontology.loadOntology(getParameter("ontology"), getParameter("baseuri"));
		logger = new Logger(ontology.getName(), "anon", sessionID);
		application = ApplicationInstance.getActive();
		taskQueue = application.createTaskQueue();
		
		SplitPane splitPane1 = new SplitPane(SplitPane.ORIENTATION_VERTICAL_TOP_BOTTOM);
		splitPane1.setSeparatorPosition(new Extent(50));
		splitPane1.setSeparatorHeight(new Extent(0));
		
		navigationButtons.setInsets(new Insets(5, 5, 5, 26));
		navigationButtons.setCellSpacing(new Extent(5));
		navigationButtons.setBackground(new Color(230, 230, 230));
		
		navigationButtons.add(backButton);
		navigationButtons.add(forwardButton);
		navigationButtons.add(refreshButton);
		
		ContentPane menuBar = new ContentPane();
		menuBar.add(navigationButtons);
		
		SplitPane splitPane2 = new SplitPane(
				SplitPane.ORIENTATION_HORIZONTAL_LEFT_RIGHT, 
				new Extent(145)
			);
		splitPane2.setSeparatorHeight(new Extent(0));
		
		ContentPane sideBar = new ContentPane();
		sideBar.setBackground(new Color(230, 230, 230));
		Column sideCol = new Column();
		sideCol.setInsets(new Insets(10, 10));
		sideCol.setCellSpacing(new Extent(1));
		
		logo = new Label(new ResourceImageReference(
				"ch/uzh/ifi/attempto/acewiki/gui/img/AceWikiLogoSmall.png"
			));
		sideCol.add(logo);
		
		sideCol.add(new VSpace(30));
		
		SolidLabel label1 = new SolidLabel("Navigation:", Font.ITALIC);
		label1.setFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(10)));
		sideCol.add(label1);
		sideCol.add(new ListItem(homeButton));
		sideCol.add(new ListItem(indexButton));
		sideCol.add(new ListItem(randomButton));
		sideCol.add(new ListItem(searchButton, null, searchTextField));
		
		sideCol.add(new VSpace(10));

		SolidLabel label2 = new SolidLabel("Actions:", Font.ITALIC);
		label2.setFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(10)));
		sideCol.add(label2);
		sideCol.add(new ListItem(newButton));
		sideCol.add(new ListItem(exportButton));
		logoutListItem = new ListItem(logoutButton);
		logoutButton.setWidth(new Extent(110));
		logoutButton.setAlignment(new Alignment(Alignment.LEFT, Alignment.CENTER));
		logoutListItem.setVisible(false);
		sideCol.add(logoutListItem);
		
		ExternalEventMonitor externalEventMonitor = new ExternalEventMonitor();
		externalEventMonitor.addExternalEventListener(this);
		sideCol.add(externalEventMonitor);
		
		//sideCol.add(new VSpace(20));
		//sideCol.add(new ItalicLabel("Session ID: " + sessionID));
		
		sideBar.add(sideCol);
		
		SplitPane splitPane3 = new SplitPane(SplitPane.ORIENTATION_HORIZONTAL_LEFT_RIGHT);
		splitPane3.setSeparatorWidth(new Extent(1));
		splitPane3.setSeparatorColor(Color.BLACK);
		splitPane3.setSeparatorPosition(new Extent(0));
		splitPane3.add(new Label());
		
		SplitPane splitPane4 = new SplitPane(SplitPane.ORIENTATION_VERTICAL_TOP_BOTTOM);
		splitPane4.setSeparatorHeight(new Extent(1));
		splitPane4.setSeparatorColor(Color.BLACK);
		splitPane4.setSeparatorPosition(new Extent(0));
		splitPane4.add(new Label());

		splitPane3.add(splitPane4);
		splitPane4.add(mainPane);
		
		splitPane1.add(menuBar);
		splitPane1.add(splitPane3);
		
		splitPane2.add(sideBar);
		splitPane2.add(splitPane1);
		
		contentPane.add(splitPane2);
		
		startPage = new StartPage(this, getParameter("title"), getParameter("description"));
		
		ContainerContext cc = (ContainerContext) application.
				getContextProperty(ContainerContext.CONTEXT_PROPERTY_NAME);
		String p = null;
		try {
			p = ((String[]) cc.getInitialRequestParameterMap().get("showpage"))[0];
		} catch (Exception ex) {}
		
		if (p != null && ontology.get(p) != null) {
			setCurrentPage(ArticlePage.create(ontology.get(p), this));
		} else {
			setCurrentPage(startPage);
		}
		
		// This thread checks regularly for pending tasks and executes them. Strong tasks take
		// precedence over weak ones.
		Thread asyncThread = new Thread() {
			
			public void run() {
				while (true) {
					try {
						sleep(500);
					} catch (InterruptedException ex) {}
					
					Task task = null;
					if (strongTasks.size() > 0) {
						task = strongTasks.remove(0);
					} else if (weakTasks.size() > 0) {
						task = weakTasks.remove(0);
					}
					
					final Task fTask = task;
					if (fTask != null) {
						task.run();
						application.enqueueTask(taskQueue, new Runnable() {
							public synchronized void run() {
								fTask.updateGUI();
								if (waitWindow != null) {
									waitWindow.setVisible(false);
									waitWindow = null;
								}
							}
						});
					}
				}
			}
			
		};
		asyncThread.setPriority(Thread.MIN_PRIORITY);
		asyncThread.start();
		
		update();
	}
	
	/**
	 * Returns the content pane containing the wiki GUI.
	 * 
	 * @return The content pane.
	 */
	public ContentPane getContentPane() {
		return contentPane;
	}
	
	/**
	 * Returns the application instance object of this wiki.
	 * 
	 * @return The application instance.
	 */
	public ApplicationInstance getApplication() {
		return application;
	}
	
	/**
	 * Returns the value of the given parameter. These parameters are defined in the web.xml file
	 * of the web application.
	 * 
	 * @param paramName The parameter name.
	 * @return The value of the parameter.
	 */
	public String getParameter(String paramName) {
		return parameters.get(paramName);
	}
	
	/**
	 * Displays the window in the wiki.
	 * 
	 * @param window The window to be shown.
	 */
	public void showWindow(final WindowPane window) {
		if (window instanceof WordEditorWindow ||
				window instanceof PreditorWindow ||
				window instanceof TextAreaWindow) {
			List<WindowPane> windowsNew = new ArrayList<WindowPane>();
			for (WindowPane wp : windows) {
				if (wp.isVisible()) windowsNew.add(wp);
			}
			windows = windowsNew;
			int c = windows.size();
			window.setPositionX(new Extent(50 + (c % 5)*40));
			window.setPositionY(new Extent(50 + (c % 5)*20));
			windows.add(window);
		}
		getContentPane().add(window);
	}
	
	/**
	 * Switches to the given page.
	 * 
	 * @param page The page to switch to.
	 */
	public void showPage(WikiPage page) {
		if (!currentPage.equals(page)) {
			history.push(currentPage);
			if (history.size() > 20) history.remove(0);
			forward.clear();
		}
		setCurrentPage(page);
		log("navi", "goto: " + page);
		update();
	}
	
	/**
	 * Switches to the page of the given ontology element.
	 * 
	 * @param e The ontology element the page of which should be shown.
	 */
	public void showPage(OntologyElement e) {
		showPage(ArticlePage.create(e, this));
	}
	
	/**
	 * Go to the previous page in the history.
	 */
	public void back() {
		if (history.isEmpty()) return;
		forward.push(currentPage);
		if (forward.size() > 20) forward.remove(0);
		WikiPage page = history.pop();
		setCurrentPage(page);
		log("navi", "back: " + page);
		update();
	}
	
	/**
	 * Go to the next page in the history.
	 */
	public void forward() {
		if (forward.isEmpty()) return;
		history.push(currentPage);
		if (history.size() > 20) history.remove(0);
		WikiPage page = forward.pop();
		setCurrentPage(page);
		log("navi", "forw: " + page);
		update();
	}
	
	/**
	 * Show the start page.
	 */
	public void showStartPage() {
		showPage(startPage);
	}
	
	/**
	 * Show the index page.
	 */
	public void showIndexPage() {
		showPage(new IndexPage(this));
	}
	
	/**
	 * Show the search page.
	 */
	public void showSearchPage() {
		showPage(new SearchPage(this, ""));
	}
	
	/**
	 * Returns the ontology;
	 * 
	 * @return The ontology.
	 */
	public Ontology getOntology() {
		return ontology;
	}
	
	/**
	 * Returns all ontology elements.
	 * 
	 * @return A collection of all ontology elements.
	 */
	public Collection<OntologyElement> getOntologyElements() {
		return ontology.getOntologyElements();
	}
	
	/**
	 * Updates the GUI.
	 */
	public void update() {
		mainPane.removeAll();
		mainPane.add(currentPage);
		
		backButton.setEnabled(!history.isEmpty());
		forwardButton.setEnabled(!forward.isEmpty());
		
		// The commented-out code below checks at every GUI update whether the ontology is consistent or not.
		// If not, a red AceWiki logo is shown. Usually, this case should never occur because we check for
		// consistency after every new statement.
		//if (ontology.isConsistent()) {
		//	logo.setIcon(new ResourceImageReference("ch/uzh/ifi/attempto/acewiki/gui/img/AceWikiLogoSmall.png"));
		//} else {
		//	logo.setIcon(new ResourceImageReference("ch/uzh/ifi/attempto/acewiki/gui/img/AceWikiLogoSmallRed.png"));
		//}
	}
	
	private void setCurrentPage(WikiPage currentPage) {
		this.currentPage = currentPage;
		refresh();
	}
	
	/**
	 * Refreshes the current page.
	 */
	public void refresh() {
		currentPage.update();
	}
	
	public void actionPerformed(ActionEvent e) {
		if (e.getSource() == backButton) {
			log("page", "pressed: back");
			back();
		} else if (e.getSource() == forwardButton) {
			log("page", "pressed: forward");
			forward();
		} else if (e.getSource() == indexButton) {
			log("page", "pressed: index");
			showIndexPage();
		} else if (e.getSource() == homeButton) {
			log("page", "pressed: main page");
			showStartPage();
		} else if (e.getSource() == randomButton) {
			log("page", "pressed: random page");
			List<OntologyElement> elements = new ArrayList<OntologyElement>(ontology.getOntologyElements());
			if (elements.size() > 0) {
				int r = (new Random()).nextInt(elements.size());
				showPage(elements.get(r));
			} else {
				showStartPage();
			}
		} else if (e.getSource() == refreshButton) {
			log("page", "pressed: refresh");
			update();
			refresh();
		} else if (e.getSource() == newButton) {
			log("page", "pressed: new word");
			WordEditorWindow w = new WordEditorWindow("Word Creator");
			w.addTab(new ProperNameForm(new Individual(), w, this, this));
			w.addTab(new NounForm(new NounConcept(), 0, w, this, this));
			w.addTab(new NounOfForm(new OfRole(), w, this, this));
			w.addTab(new VerbForm(new VerbRole(), 0, w, this, this));
			w.addTab(new TrAdjForm(new TrAdjRole(), w, this, this));
			showWindow(w);
		} else if (e.getSource() == searchButton || e.getSource() == searchTextField) {
			log("page", "pressed: search '" + searchTextField.getText() + "'");
			String s = searchTextField.getText();
			searchTextField.setText("");
			OntologyElement el = ontology.get(s.replace(' ', '_'));
			if (el == null) {
				showPage(new SearchPage(this, s));
			} else {
				showPage(el);
			}
		} else if (e.getSource() == exportButton) {
			showWindow(new ExportWindow(this));
		} else if (e.getSource() == logoutButton) {
			showWindow(new MessageWindow("Logout", "Do you really want to log out?", null, this, "Yes", "No"));
		} else if (e.getSource() instanceof MessageWindow && e.getActionCommand().equals("Yes")) {
			((AceWikiApp) ApplicationInstance.getActive()).logout();
		} else if (e.getSource() instanceof OntologyTextElement) {
			// for newly generated elements
			OntologyTextElement te = (OntologyTextElement) e.getSource();
			log("edit", "new word: " + te.getOntologyElement().getWord());
			showPage(te.getOntologyElement());
		}
	}
	
	public void externalEvent(ExternalEvent e) {
		OntologyElement oe = ontology.get(e.getParameter("page"));
		if (oe != null) {
			showPage(ontology.get(e.getParameter("page")));
		}
	}
	
	/**
	 * Writes the log entry to the log file.
	 * 
	 * @param type The type of the log entry.
	 * @param text The text of the log entry.
	 */
	public void log(String type, String text) {
		logger.log(type, text);
	}
	
	/**
	 * Sets the user name.
	 * 
	 * @param username The user name.
	 */
	public void setUsername(String username) {
		logger.setUsername(username);
		logoutButton.setText("Logout: " + username);
		logoutListItem.setVisible(true);
	}
	
	/**
	 * Returns the grammar to be used for this wiki.
	 * 
	 * @return The grammar.
	 */
	public Grammar getGrammar() {
		return grammar;
	}
	
	/**
	 * Returns the logger object.
	 * 
	 * @return The logger object.
	 */
	public Logger getLogger() {
		return logger;
	}
	
	/**
	 * Runs the task without showing a wait window while it is executed.
	 * 
	 * @param task The task.
	 */
	public void enqueueTask(Runnable task) {
		application.enqueueTask(taskQueue, task);
	}

	/**
	 * Runs the task in an asynchronous way and shows a wait window while it is executed. The task
	 * is treated as a strong task that takes precedence over weak tasks.
	 * 
	 * @param title The title of the wait window.
	 * @param message The message of the wait window.
	 * @param task The task.
	 */
	public void enqueueStrongAsyncTask(String title, String message, Task task) {
		waitWindow = new MessageWindow(
			title,
			new ResourceImageReference("ch/uzh/ifi/attempto/acewiki/gui/img/wait.gif"),
			message,
			null,
			null
		);
		waitWindow.setClosable(false);
		showWindow(waitWindow);
		
		strongTasks.add(task);
	}

	/**
	 * Runs the task in an asynchronous way without showing a wait window. The task is treated as a
	 * weak task that can be overtaken by strong tasks.
	 * 
	 * @param task The task.
	 */
	public void enqueueWeakAsyncTask(Task task) {
		weakTasks.add(task);
	}
	
	/**
	 * Returns information about AceWiki, like the version number and the release date. This
	 * information is read from the file "acewiki.properties".
	 * 
	 * @param key The key string.
	 * @return The value for the given key.
	 */
	public static String getInfo(String key) {
		if (properties == null) {
			String f = "ch/uzh/ifi/attempto/acewiki/acewiki.properties";
			InputStream in = Thread.currentThread().getContextClassLoader().getResourceAsStream(f);
			properties = new Properties();
			try {
				properties.load(in);
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
		
		return properties.getProperty(key);
	}

}
