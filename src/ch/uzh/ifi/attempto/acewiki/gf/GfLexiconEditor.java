package ch.uzh.ifi.attempto.acewiki.gf;

import nextapp.echo.app.Border;
import nextapp.echo.app.Color;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import nextapp.echo.app.Table;
import nextapp.echo.app.table.TableModel;

public class GfLexiconEditor extends Table {

	private static final long serialVersionUID = -3002899523942337116L;

	public GfLexiconEditor(TableModel model) {
		super(model);
		setHeaderVisible(true);
		setRolloverEnabled(true);
		setFont(new Font(Font.MONOSPACE, Font.PLAIN, new Extent(12)));
		setSelectionEnabled(true);
		setInsets(new Insets(8, 8, 8, 8));
		setBorder(new Border(1, Color.LIGHTGRAY, Border.STYLE_GROOVE));
	}

}