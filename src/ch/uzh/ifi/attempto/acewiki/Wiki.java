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

package ch.uzh.ifi.attempto.acewiki;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Random;
import java.util.Stack;

import javax.servlet.http.Cookie;

import nextapp.echo.app.Alignment;
import nextapp.echo.app.ApplicationInstance;
import nextapp.echo.app.Color;
import nextapp.echo.app.Column;
import nextapp.echo.app.Component;
import nextapp.echo.app.ContentPane;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import nextapp.echo.app.ResourceImageReference;
import nextapp.echo.app.Row;
import nextapp.echo.app.SplitPane;
import nextapp.echo.app.TaskQueueHandle;
import nextapp.echo.app.WindowPane;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import nextapp.echo.app.layout.ColumnLayoutData;
import nextapp.echo.webcontainer.ContainerContext;
import ch.uzh.ifi.attempto.acewiki.core.AceWikiDataExporter;
import ch.uzh.ifi.attempto.acewiki.core.AceWikiEngine;
import ch.uzh.ifi.attempto.acewiki.core.AceWikiStorage;
import ch.uzh.ifi.attempto.acewiki.core.LanguageHandler;
import ch.uzh.ifi.attempto.acewiki.core.LexiconTableExporter;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.OntologyExportManager;
import ch.uzh.ifi.attempto.acewiki.core.OntologyExporter;
import ch.uzh.ifi.attempto.acewiki.core.OntologyTextElement;
import ch.uzh.ifi.attempto.acewiki.core.StatementTableExporter;
import ch.uzh.ifi.attempto.acewiki.core.User;
import ch.uzh.ifi.attempto.acewiki.core.UserBase;
import ch.uzh.ifi.attempto.acewiki.gfservice.GFEngine;
import ch.uzh.ifi.attempto.acewiki.gui.AboutPage;
import ch.uzh.ifi.attempto.acewiki.gui.ArticlePage;
import ch.uzh.ifi.attempto.acewiki.gui.ExportWindow;
import ch.uzh.ifi.attempto.acewiki.gui.FormPane;
import ch.uzh.ifi.attempto.acewiki.gui.GrammarPage;
import ch.uzh.ifi.attempto.acewiki.gui.IconButton;
import ch.uzh.ifi.attempto.acewiki.gui.IndexPage;
import ch.uzh.ifi.attempto.acewiki.gui.ListItem;
import ch.uzh.ifi.attempto.acewiki.gui.LoginWindow;
import ch.uzh.ifi.attempto.acewiki.gui.SearchPage;
import ch.uzh.ifi.attempto.acewiki.gui.StartPage;
import ch.uzh.ifi.attempto.acewiki.gui.Title;
import ch.uzh.ifi.attempto.acewiki.gui.UserWindow;
import ch.uzh.ifi.attempto.acewiki.gui.WikiPage;
import ch.uzh.ifi.attempto.base.Logger;
import ch.uzh.ifi.attempto.echocomp.HSpace;
import ch.uzh.ifi.attempto.echocomp.Label;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;
import ch.uzh.ifi.attempto.echocomp.SectionTitle;
import ch.uzh.ifi.attempto.echocomp.SmallButton;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import ch.uzh.ifi.attempto.echocomp.Style;
import ch.uzh.ifi.attempto.echocomp.TextAreaWindow;
import ch.uzh.ifi.attempto.echocomp.TextField;
import ch.uzh.ifi.attempto.echocomp.VSpace;
import ch.uzh.ifi.attempto.gfservice.GfServiceException;
import ch.uzh.ifi.attempto.preditor.PreditorWindow;
import ch.uzh.ifi.attempto.preditor.WordEditorWindow;
import echopoint.externalevent.ExternalEvent;
import echopoint.externalevent.ExternalEventListener;
import echopoint.externalevent.ExternalEventMonitor;

/**
 * This class represents an AceWiki wiki instance (including its graphical user interface).
 * There is such a wiki object for every wiki user.
 * The actions displayed in this GUI refer to the whole wiki not its individual articles
 * (Main Page and Random Article being the exceptions).
 *
 * @author Tobias Kuhn
 * @author Kaarel Kaljurand
 */
public class Wiki implements ActionListener, ExternalEventListener {

	// In standard AceWiki, Word == Page. In a more general setting, Page
	// seems to be a more clear term.
	public static final String LABEL_BUTTON_NEW_PAGE = "New Page...";
	public static final String LABEL_WINDOW_NEW_PAGE = "Page Creator";

	public static final String LABEL_ABOUT_GRAMMAR = "About Grammar";

	private static final long serialVersionUID = 2777443689044226043L;

	private Map<String, String> parameters;

	private final Ontology ontology;
	private final AceWikiEngine engine;
	private String language;
	private User user;
	private OntologyExportManager ontologyExportManager;
	private static AceWikiStorage storage;

	private WikiPage currentPage;
	private Column pageCol;
	private ContentPane contentPane = new ContentPane();
	private Row navigationButtons = new Row();
	private Logger logger;
	private SplitPane wikiPane;
	private Row loginBackground;

	private final IconButton backButton = new IconButton("Back", this);
	private final IconButton forwardButton = new IconButton("Forward", this);
	private final IconButton refreshButton = new IconButton("Refresh", this);
	private final IconButton userButton = new IconButton("User", this);
	private final IconButton logoutButton = new IconButton("Logout", this);
	private final IconButton searchButton = new IconButton("Search", this);
	private final TextField searchTextField = new TextField(170, this);
	private final Label userLabel = new SolidLabel("Anonymous", Font.ITALIC);

	private final SmallButton homeButton = new SmallButton("Main Page", this, 12);
	private final SmallButton indexButton = new SmallButton("Index", this, 12);
	private final SmallButton searchButton2 = new SmallButton("Search", this, 12);
	private final SmallButton aboutButton = new SmallButton("About", this, 12);
	private final SmallButton randomButton = new SmallButton("Random Article", this, 12);
	private final SmallButton newButton = new SmallButton(LABEL_BUTTON_NEW_PAGE, this, 12);
	private final SmallButton exportButton = new SmallButton("Export...", this, 12);

	private final SmallButton aboutGrammarButton = new SmallButton(LABEL_ABOUT_GRAMMAR, this, 12);

	private List<SmallButton> languageButtons = new ArrayList<SmallButton>();

	private StartPage startPage;

	private Stack<WikiPage> history = new Stack<WikiPage>();
	private Stack<WikiPage> forward = new Stack<WikiPage>();

	private TaskQueueHandle taskQueue;
	private MessageWindow waitWindow;
	private List<Task> strongTasks = new ArrayList<Task>();
	private List<Task> weakTasks = new ArrayList<Task>();

	private ExternalEventMonitor externalEventMonitor;

	private AceWikiApp application;

	private static Properties properties;

	private boolean disposed = false;

	private boolean locked = false;
	private ActionListener lockedListener;

	/**
	 * Creates a new wiki instance.
	 *
	 * @param backend The backend object contains ontology of the wiki.
	 * @param parameters A set of parameters in the form of name/value pairs.
	 * @param sessionID The session id.
	 */
	Wiki(Backend backend, Map<String, String> parameters, int sessionID) {
		this.parameters = parameters;

		storage = backend.getStorage();
		ontology = backend.getOntology();

		engine = ontology.getEngine();
		language = getParameter("language");
		if (language == null || language.equals("")) {
			language = engine.getLanguages()[0];
		}
		logger = new Logger(getParameter("context:logdir") + "/" + ontology.getName(), "anon", sessionID);
		application = (AceWikiApp) ApplicationInstance.getActive();
		taskQueue = application.createTaskQueue();

		ontologyExportManager = new OntologyExportManager(ontology);
		for (OntologyExporter o : engine.getExporters()) {
			ontologyExportManager.addExporter(o);
		}
		ontologyExportManager.addExporter(new LexiconTableExporter());
		ontologyExportManager.addExporter(new StatementTableExporter(language));
		ontologyExportManager.addExporter(new AceWikiDataExporter());

		SplitPane splitPane1 = new SplitPane(SplitPane.ORIENTATION_VERTICAL_TOP_BOTTOM);
		splitPane1.setSeparatorPosition(new Extent(50));
		splitPane1.setSeparatorHeight(new Extent(0));

		SplitPane splitPane2 = new SplitPane(SplitPane.ORIENTATION_HORIZONTAL_RIGHT_LEFT);
		splitPane2.setSeparatorPosition(new Extent(215));
		splitPane2.setSeparatorWidth(new Extent(0));

		navigationButtons.setInsets(new Insets(5));
		navigationButtons.setBackground(Style.shadedBackground);

		navigationButtons.add(backButton);
		navigationButtons.add(new HSpace(5));
		navigationButtons.add(forwardButton);
		navigationButtons.add(new HSpace(5));
		navigationButtons.add(refreshButton);
		navigationButtons.add(new HSpace(30));
		Row userRow = new Row();
		userRow.add(userButton);
		userRow.add(new HSpace(3));
		userLabel.setForeground(Color.DARKGRAY);
		userRow.add(userLabel);
		logoutButton.setVisible(false);
		userRow.add(logoutButton);
		userRow.setVisible(isLoginEnabled());
		navigationButtons.add(userRow);

		ContentPane menuBar = new ContentPane();
		menuBar.setBackground(Style.shadedBackground);
		menuBar.add(navigationButtons);

		Row searchRow = new Row();
		searchRow.setInsets(new Insets(5));
		searchRow.setBackground(Style.shadedBackground);
		searchRow.add(searchButton);
		searchRow.add(new HSpace(5));
		searchRow.add(searchTextField);

		ContentPane searchBar = new ContentPane();
		searchBar.setBackground(Style.shadedBackground);
		searchBar.add(searchRow);

		wikiPane = new SplitPane(
				SplitPane.ORIENTATION_HORIZONTAL_LEFT_RIGHT,
				new Extent(145)
				);
		wikiPane.setSeparatorHeight(new Extent(0));

		ContentPane sideBar = new ContentPane();
		sideBar.setBackground(Style.shadedBackground);
		Column sideCol = new Column();
		sideCol.setInsets(new Insets(10, 10));
		sideCol.setCellSpacing(new Extent(1));

		Label logo = new Label(new ResourceImageReference(
				"ch/uzh/ifi/attempto/acewiki/gui/img/AceWikiLogoSmall.png"
				));
		sideCol.add(logo);

		sideCol.add(new VSpace(10));

		ColumnLayoutData layout = new ColumnLayoutData();
		layout.setAlignment(Alignment.ALIGN_CENTER);

		String title = getParameter("title");
		if (title != null && title.length() > 0) {
			Label titleLabel = new Label(title, Font.ITALIC, 14);
			sideCol.add(titleLabel);
			titleLabel.setLayoutData(layout);
			sideCol.add(new VSpace(5));
		}

		if (isReadOnly()) {
			SolidLabel rolabel = new SolidLabel("— READ ONLY —", Font.ITALIC);
			rolabel.setFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(10)));
			rolabel.setLayoutData(layout);
			sideCol.add(rolabel);
			sideCol.add(new VSpace(5));
		}

		sideCol.add(new VSpace(20));

		sideCol.add(new SectionTitle("Navigation"));
		sideCol.add(new ListItem(homeButton));
		sideCol.add(new ListItem(indexButton));
		sideCol.add(new ListItem(searchButton2));
		sideCol.add(new ListItem(aboutButton));
		sideCol.add(new ListItem(randomButton));

		sideCol.add(new VSpace(10));
		sideCol.add(new SectionTitle("Actions"));
		if (!isReadOnly() && getEngine().getLexicalTypes().length > 0) {
			sideCol.add(new ListItem(newButton));
		}

		sideCol.add(new ListItem(exportButton));

		sideCol.add(new VSpace(10));
		sideCol.add(new SectionTitle("Grammar"));
		sideCol.add(new ListItem(aboutGrammarButton));

		if (engine.getLanguages().length > 1 && !"off".equals(getParameter("language_switching"))) {
			// show language switcher

			sideCol.add(new VSpace(10));
			sideCol.add(new SectionTitle("Languages"));
			
			for (String lang : engine.getLanguages()) {
				SmallButton b = new SmallButton(lang, this, 12);
				if (lang.equals(language)) b.setEnabled(false);
				languageButtons.add(b);
				sideCol.add(new ListItem(b));
			}
		}

		externalEventMonitor = new ExternalEventMonitor();
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
		pageCol = new Column();
		splitPane4.add(pageCol);

		splitPane2.add(searchBar);
		splitPane2.add(menuBar);

		splitPane1.add(splitPane2);
		splitPane1.add(splitPane3);

		wikiPane.add(sideBar);
		wikiPane.add(splitPane1);

		contentPane.add(wikiPane);

		startPage = new StartPage(this);

		// auto login
		if (isLoginEnabled()) {
			String userName = getCookie("lastusername");
			boolean stayLoggedIn = getCookie("stayloggedin").equals("true");
			if (getUserBase().containsUser(userName) && stayLoggedIn) {
				String clientToken = getCookie("stayloggedintoken");
				if (clientToken.length() > 0) {
					log("syst", "try auto login...");
					user = getUserBase().autoLogin(userName, clientToken);
					if (user != null) {
						log("syst", "auto login successful: " + user.getName());
						setUser(user);
					} else {
						log("syst", "auto login failed: " + userName);
						clearCookie("stayloggedintoken");
					}
				}
			}
		}

		String p = null;
		try {
			p = ((String[]) getContainerContext().getInitialRequestParameterMap()
					.get("showpage"))[0];
		} catch (Exception ex) {}

		if (p != null && ontology.getElement(p) != null) {
			setCurrentPage(ArticlePage.create(ontology.getElement(p), this));
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

					if (disposed) {
						break;
					}

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
									removeWindow(waitWindow);
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
	 * Returns whether the login features are enabled.
	 *
	 * @return true if login is enabled.
	 */
	public boolean isLoginEnabled() {
		return "yes".equals(getParameter("login"));
	}

	/**
	 * Returns whether login is required for viewing the wiki data.
	 *
	 * @return true if login is required for viewing.
	 */
	public boolean isLoginRequiredForViewing() {
		if (!isLoginEnabled()) return false;
		return "yes".equals(getParameter("login_required"));
	}

	/**
	 * Returns whether login is required for editing the wiki data.
	 *
	 * @return true if login is required for editing.
	 */
	public boolean isLoginRequiredForEditing() {
		if (!isLoginEnabled()) return false;
		if (isLoginRequiredForViewing()) return true;
		return "edit".equals(getParameter("login_required"));
	}

	/**
	 * Returns whether the user registration is open to everyone.
	 *
	 * @return true if the user registration is open.
	 */
	public boolean isUserRegistrationOpen() {
		if (!isLoginEnabled()) return false;
		return !"no".equals(getParameter("register"));
	}

	/**
	 * Returns whether the wiki is in the current situation editable. This depends on the fact
	 * whether a user is logged in and whether login is required for editing the wiki data.
	 *
	 * @return true if the wiki is editable.
	 */
	public boolean isEditable() {
		return (user != null || !isLoginRequiredForEditing());
	}

	/**
	 * Returns true if this wiki is set to be read-only.
	 *
	 * @return true if this wiki is read-only.
	 */
	public boolean isReadOnly() {
		return "on".equals(parameters.get("readonly"));
	}

	/**
	 * Shows the window.
	 *
	 * @param window The window to be shown.
	 */
	public void showWindow(WindowPane window) {
		cleanWindows();
		if (window instanceof WordEditorWindow ||
				window instanceof PreditorWindow ||
				window instanceof TextAreaWindow) {
			int c = getContentPane().getComponentCount() - 1;
			window.setPositionX(new Extent(50 + (c % 5)*40));
			window.setPositionY(new Extent(50 + (c % 5)*20));
		}
		getContentPane().add(window);
	}

	/**
	 * Shows a word editor window.
	 *
	 * @param element The ontology element to be edited.
	 */
	public void showEditorWindow(OntologyElement element) {
		WordEditorWindow editorWindow = new WordEditorWindow("Word Editor");
		editorWindow.addTab(new FormPane(element, editorWindow, this));
		showWindow(editorWindow);
	}

	/**
	 * Shows a word creator window for the given word type and number.
	 *
	 * @param type The word type.
	 * @param wordNumber The word number.
	 * @param actionListener The actionlistener.
	 */
	public void showCreatorWindow(String type, int wordNumber, ActionListener actionListener) {
		WordEditorWindow creatorWindow = new WordEditorWindow(LABEL_WINDOW_NEW_PAGE);
		creatorWindow.addTab(new FormPane(type, wordNumber, creatorWindow, this, actionListener));
		showWindow(creatorWindow);
	}

	/**
	 * Removes the window.
	 *
	 * @param window The window to be removed.
	 */
	public void removeWindow(WindowPane window) {
		window.setVisible(false);
		window.dispose();
		cleanWindows();
	}

	private void cleanWindows() {
		for (Component c : getContentPane().getComponents()) {
			if (!c.isVisible()) {
				getContentPane().remove(c);
			}
		}
	}

	/**
	 * Shows the login window.
	 */
	public void showLoginWindow() {
		if (isLoginRequiredForViewing()) {
			getContentPane().removeAll();
			loginBackground = new Row();
			loginBackground.setInsets(new Insets(10, 10));
			loginBackground.setCellSpacing(new Extent(30));
			Label loginBgLogo = new Label(getImage("AceWikiLogoSmall.png"));
			loginBackground.add(loginBgLogo);
			loginBackground.add(new Title(getParameter("title"), true));
			getContentPane().add(loginBackground);
			getContentPane().setBackground(new Color(230, 230, 230));
		}
		showWindow(new LoginWindow(this));
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
	 * Show the about page.
	 */
	public void showAboutPage() {
		showPage(new AboutPage(this));
	}

	/**
	 * Show the about grammar page.
	 */
	public void showAboutGrammarPage() {
		if (engine instanceof GFEngine) {
			GFEngine gfEngine = (GFEngine) engine;
			try {
				showPage(new GrammarPage(this, gfEngine.getGFGrammar()));
			} catch (GfServiceException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
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
	 * Returns the ontology export manager.
	 *
	 * @return The ontology export manager.
	 */
	public OntologyExportManager getOntologyExportManager() {
		return ontologyExportManager;
	}

	/**
	 * Returns the user base for this wiki.
	 *
	 * @return The user base.
	 */
	public UserBase getUserBase() {
		return storage.getUserBase(ontology);
	}

	/**
	 * Returns all ontology elements. The list is a copy of the internal list.
	 *
	 * @return A list of all ontology elements.
	 */
	public List<OntologyElement> getOntologyElements() {
		return ontology.getOntologyElements();
	}

	/**
	 * Updates the GUI.
	 */
	public void update() {
		pageCol.removeAll();
		pageCol.add(currentPage);

		removeExpiredPages(history);
		removeExpiredPages(forward);
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

	private void removeExpiredPages(Stack<WikiPage> stack) {
		WikiPage previousPage = null;
		for (WikiPage page : new ArrayList<WikiPage>(stack)) {
			if (page.isExpired() || page.equals(previousPage)) {
				stack.remove(page);
			} else {
				previousPage = page;
			}
		}
		if (stack.size() > 0 && currentPage.equals(stack.peek())) {
			stack.pop();
		}
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
		Object src = e.getSource();
		String c = e.getActionCommand();

		if (locked) {
			if (lockedListener != null) {
				lockedListener.actionPerformed(new ActionEvent(this, "locked"));
			}
			return;
		}

		if (src == backButton) {
			log("page", "pressed: back");
			back();
		} else if (src == forwardButton) {
			log("page", "pressed: forward");
			forward();
		} else if (src == indexButton) {
			log("page", "pressed: index");
			showIndexPage();
		} else if (src == aboutButton) {
			log("page", "pressed: about");
			showAboutPage();
		} else if (src == aboutGrammarButton) {
			log("page", "pressed: about grammar");
			showAboutGrammarPage();
		} else if (src == homeButton) {
			log("page", "pressed: main page");
			showStartPage();
		} else if (src == randomButton) {
			log("page", "pressed: random page");
			List<OntologyElement> elements = ontology.getOntologyElements();
			if (elements.size() > 0) {
				int r = (new Random()).nextInt(elements.size());
				showPage(elements.get(r));
			} else {
				showStartPage();
			}
		} else if (src == refreshButton) {
			log("page", "pressed: refresh");
			update();
			refresh();
		} else if (src == newButton) {
			log("page", "pressed: new word");
			if (!isEditable()) {
				showLoginWindow();
			} else {
				WordEditorWindow w = new WordEditorWindow(LABEL_WINDOW_NEW_PAGE);
				for (String t : getEngine().getLexicalTypes()) {
					w.addTab(new FormPane(t, w, this));
				}
				showWindow(w);
			}
		} else if (src == searchButton || src == searchTextField || src == searchButton2) {
			log("page", "pressed: search '" + searchTextField.getText() + "'");
			String s = searchTextField.getText();
			searchTextField.setText("");
			OntologyElement el = ontology.getElement(s.replace(' ', '_'));
			if (el == null) {
				showPage(new SearchPage(this, s));
			} else {
				showPage(el);
			}
		} else if (src == exportButton) {
			showWindow(new ExportWindow(this));
		} else if (src == logoutButton) {
			showWindow(new MessageWindow(
					"Logout",
					"Do you really want to log out?",
					null,
					this,
					"Yes", "No"
					));
		} else if (src == userButton) {
			if (user == null) {
				showLoginWindow();
			} else {
				showWindow(new UserWindow(this));
			}
		} else if (src instanceof MessageWindow && c.equals("Yes")) {
			logout();
		} else if (src instanceof OntologyTextElement) {
			// for newly generated elements
			OntologyTextElement te = (OntologyTextElement) src;
			log("edit", "new word: " + te.getOntologyElement().getWord());
			showPage(te.getOntologyElement());
		} else if (languageButtons.contains(src)) {
			switchLanguage(((SmallButton) src).getText());
		}
	}

	public void externalEvent(ExternalEvent e) {
		OntologyElement oe = ontology.getElement(e.getParameter("page"));
		if (oe != null) showPage(oe);
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
	 * Logs in the given user.
	 *
	 * @param user The user to log in.
	 * @param stayLoggedIn Defines whether the user should stay logged in or not.
	 */
	public void login(User user, boolean stayLoggedIn) {
		log("syst", "login");
		user.setUserData("stayloggedin", stayLoggedIn + "");
		setCookie("stayloggedin", stayLoggedIn + "");
		String stayloggedintoken;
		if (stayLoggedIn) {
			stayloggedintoken = (new Random()).nextLong() + "";
		} else {
			stayloggedintoken = "";
		}
		user.setUserData("stayloggedintoken", stayloggedintoken);
		setCookie("stayloggedintoken", stayloggedintoken);
		setUser(user);
	}

	/**
	 * Logs out the current user.
	 */
	public void logout() {
		log("syst", "logout");
		user.setUserData("stayloggedintoken", "");
		setCookie("stayloggedintoken", "");
		application.logout();
	}

	/**
	 * Returns the user of this wiki object.
	 *
	 * @return The user.
	 */
	public User getUser() {
		return user;
	}

	/**
	 * Sets the user.
	 *
	 * @param user The user.
	 */
	public void setUser(User user) {
		this.user = user;
		logger.setUsername(user.getName());
		userLabel.setForeground(Color.BLACK);
		userLabel.setText(user.getName());
		logoutButton.setVisible(true);
		if (loginBackground != null) {
			getContentPane().removeAll();
			getContentPane().add(wikiPane);
			getContentPane().setBackground(Color.WHITE);
			loginBackground = null;
		}
		setCookie("lastusername", user.getName());
	}

	/**
	 * Sets a cookie on the client.
	 *
	 * @param name The name of the cookie.
	 * @param value The value of the cookie.
	 */
	public void setCookie(String name, String value) {
		Cookie cookie = new Cookie(name, value);
		cookie.setMaxAge(1000000000);
		getContainerContext().addCookie(cookie);
	}

	/**
	 * Clears the given cookie on the client.
	 *
	 * @param name The name of the cookie.
	 */
	public void clearCookie(String name) {
		getContainerContext().addCookie(new Cookie(name, null));
	}

	/**
	 * Returns the value of the cookie on the client, or "" if there is no such cookie.
	 *
	 * @param name The name of the cookie.
	 * @return The value of the cookie.
	 */
	public String getCookie(String name) {
		for (Cookie cookie : getContainerContext().getCookies()) {
			if ((name + "").equals(cookie.getName())) {
				String value = cookie.getValue();
				if (value == null) return "";
				return value;
			}
		}
		return "";
	}

	private ContainerContext getContainerContext() {
		return (ContainerContext) application.getContextProperty(
				ContainerContext.CONTEXT_PROPERTY_NAME
				);
	}

	/**
	 * Returns the AceWiki engine.
	 *
	 * @return The AceWiki engine.
	 */
	public AceWikiEngine getEngine() {
		return engine;
	}

	/**
	 * Returns the language of this wiki instance.
	 * 
	 * @return The name of the language.
	 */
	public String getLanguage() {
		return language;
	}

	/**
	 * Switches to another language.
	 * 
	 * @param language The new language.
	 */
	public void switchLanguage(String language) {
		this.language = language;
		for (SmallButton b : languageButtons) {
			b.setEnabled(!b.getText().equals(language));
		}
		update();
		refresh();
	}
	
	/**
	 * Returns the language handler.
	 *
	 * @return The language handler.
	 */
	public LanguageHandler getLanguageHandler() {
		return engine.getLanguageHandler(language);
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

	/**
	 * Cleans up when the object is no longer used.
	 */
	public void dispose() {
		disposed = true;
		externalEventMonitor.removeExternalEventListener(this);
		externalEventMonitor.dispose();
	}

	/**
	 * This methods locks the general buttons of the wiki interface. When one of these buttons
	 * is pressed, the locked-listener is called.
	 *
	 * @param lockedListener The listener to be called when one of the buttons is pressed.
	 */
	public void lock(ActionListener lockedListener) {
		if (locked) return;
		locked = true;
		this.lockedListener = lockedListener;
	}

	/**
	 * Unlocks the wiki interface, if it has been locked before.
	 */
	public void unlock() {
		locked = false;
	}

	/**
	 * Returns an image reference for a file in the AceWiki image directory with the given file
	 * name.
	 *
	 * @param fileName The name of the image file.
	 * @return The image reference.
	 */
	public static ResourceImageReference getImage(String fileName) {
		return Style.getImage("ch/uzh/ifi/attempto/acewiki/gui/img/" + fileName);
	}

}
