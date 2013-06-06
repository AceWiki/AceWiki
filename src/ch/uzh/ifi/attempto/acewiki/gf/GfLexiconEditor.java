package ch.uzh.ifi.attempto.acewiki.gf;

import ch.uzh.ifi.attempto.acewiki.Wiki;
import nextapp.echo.app.Border;
import nextapp.echo.app.Color;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import nextapp.echo.app.Table;
import nextapp.echo.app.table.TableModel;

public class GfLexiconEditor extends Table {

	private static final long serialVersionUID = -3002899523942337116L;
	private final Wiki mWiki;

	public GfLexiconEditor(Wiki wiki, TableModel model) {
		super(model);
		mWiki = wiki;
		setHeaderVisible(true);
		setFont(new Font(Font.MONOSPACE, Font.PLAIN, new Extent(11)));
		setInsets(new Insets(6, 6, 6, 6));
		setBorder(new Border(1, Color.LIGHTGRAY, Border.SIDE_BOTTOM));
		setDefaultRenderer(Object.class, new GfLexiconEditorCellRenderer(mWiki));
	}

}