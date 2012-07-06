package ch.uzh.ifi.attempto.acewiki.gui;

import java.util.Arrays;

import ch.uzh.ifi.attempto.acewiki.core.AceWikiEngine;
import nextapp.echo.app.event.ListDataListener;
import nextapp.echo.app.list.ListModel;

/**
 * TODO: allow the list of languages to change and notify the user
 * if this occurs
 *
 * @author Kaarel Kaljurand
 */
public class LanguageListModel implements ListModel {

	private static final long serialVersionUID = 3637618110302453887L;
	private String[] mLanguages;

	public LanguageListModel(AceWikiEngine engine) {
		mLanguages = engine.getLanguages();
		Arrays.sort(mLanguages);
	}

	@Override
	public Object get(int index) {
		return mLanguages[index];
	}


	@Override
	public int size() {
		return mLanguages.length;
	}


	@Override
	public void addListDataListener(ListDataListener arg0) {
		// TODO Auto-generated method stub
	}


	@Override
	public void removeListDataListener(ListDataListener arg0) {
		// TODO Auto-generated method stub
	}

}