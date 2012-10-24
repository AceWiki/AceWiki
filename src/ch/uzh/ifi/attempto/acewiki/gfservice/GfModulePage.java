package ch.uzh.ifi.attempto.acewiki.gfservice;

import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Sets;

import nextapp.echo.app.Color;
import nextapp.echo.app.event.ActionEvent;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.AceWikiEngine;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.gui.ArticlePage;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;
import ch.uzh.ifi.attempto.gfservice.GfModule;
import ch.uzh.ifi.attempto.gfservice.GfParseResult;
import ch.uzh.ifi.attempto.gfservice.GfServiceException;
import ch.uzh.ifi.attempto.gfservice.GfStorageResult;

public class GfModulePage extends ArticlePage {

	private final Logger mLogger = LoggerFactory.getLogger(GfModulePage.class);

	private static final long serialVersionUID = -5592272938081004472L;

	// TODO: make it a button (not a tab), and enable it only if the grammar is error free
	private static final String ACTION_MAKE = "Rebuild grammar";

	private final OntologyElement mElement;
	private final GFEngine mEngine;

	public GfModulePage(OntologyElement element, Wiki wiki) {
		super(wiki, element);
		mElement = element;

		// TODO: improve
		AceWikiEngine engine = getArticle().getOntology().getEngine();
		if (engine instanceof GFEngine) {
			mEngine = (GFEngine) engine;
		} else {
			mEngine = null;
		}
		getTitle().setColor(new Color(102, 153, 0));
		addTab(ACTION_MAKE, this);
	}

	public OntologyElement getOntologyElement() {
		return mElement;
	}

	public void actionPerformed(ActionEvent e) {
		super.actionPerformed(e);
		if (ACTION_MAKE.equals(e.getActionCommand())) {
			log("page", "pressed: make");
			integrate();
		}
	}

	protected void doUpdate() {
		super.doUpdate();
		getTitle().setText(mElement.getWord());
		parse();
	}


	private void integrate() {
		// TODO: this blocks, do it in the background
		if (mEngine.isGrammarEditable() && hasContent()) {
			try {
				GfStorageResult result = mEngine.integrateGfModule(
						getGfModule(),
						getModuleNames(getArticle().getOntology()));

				if (! result.isSuccess()) {
					// Pop up error message
					mLogger.info("integrate: GfStorageResult: '{}'", result);
					getWiki().showWindow(new MessageWindow(result.getResultCode(),
							result.getMessage() + " (" + result.getCommand() + ")"));
				}
			} catch (GfServiceException e) {
				mLogger.info("make: GfServiceException: '{}'", e.getMessage());
			}
		}
	}


	private void parse() {
		// TODO: this blocks, do it in the background
		if (hasContent()) {
			try {
				GfParseResult result = mEngine.parseGfModule(getGfModule());
				if (! result.isSuccess()) {
					// Pop up error message
					mLogger.info("parse: GfParseResult: '{}'", result);
					getWiki().showWindow(new MessageWindow(result.getResultCode(), "Line:Column = " + result.getLocation()));
				}
			} catch (GfServiceException e) {
				mLogger.info("parse: GfServiceException: '{}'", e.getMessage());
			}
		}
	}


	private GfModule getGfModule() {
		return new GfModule(getName(), getContent());
	}


	private boolean hasContent() {
		return ! getArticle().getStatements().isEmpty();
	}


	private String getName() {
		return mElement.getWord();
	}


	private String getContent() {
		return getArticle().getStatements().iterator().next().toString();
	}


	/**
	 * Returns the names of all the modules that are part of the given
	 * ontology.
	 */
	private static Set<String> getModuleNames(Ontology ont) {
		Set<String> names = Sets.newHashSet();
		for (TypeGfModule el : ont.getOntologyElements(TypeGfModule.class)) {
			names.add(el.getWord());
		}
		return names;
	}

}