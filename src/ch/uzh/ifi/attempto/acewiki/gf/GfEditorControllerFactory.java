package ch.uzh.ifi.attempto.acewiki.gf;

import ch.uzh.ifi.attempto.acewiki.core.EditorController;

public class GfEditorControllerFactory {

	public static final int MAX_MENU_GROUP_COUNT = 7;

	/**
	 * <p>Creates an {@code EditorController} on the basis of the grammar and
	 * the current language. The grammar determines the largest categories to
	 * show (other tokens go into the default category) and the category
	 * labels which respect the current language.</p>
	 */
	public static EditorController createFromCats(GfGrammar grammar, String language) {
		EditorController ec = new EditorController();
		int shift = 0;
		int menuGroupCount = 1;
		ec.setAutocompleteTokens(".", "?");
		ec.setDefaultMenuGroup("..."); // TODO: localize
		ec.addMenuGroup("...", shift);

		for (String cat : grammar.getLargestCategories(MAX_MENU_GROUP_COUNT)) {
			if (++menuGroupCount > MAX_MENU_GROUP_COUNT) {
				break;
			}
			shift += 60;
			String menuGroup = grammar.getCategoryName(cat, language);
			ec.addMenuGroup(menuGroup, shift);
			ec.addPlainCategory(cat, menuGroup);
		}
		return ec;
	}

}