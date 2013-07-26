package ch.uzh.ifi.attempto.acewiki.gf;

import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.ModuleElement;
import ch.uzh.ifi.attempto.acewiki.gui.WikiLink;
import ch.uzh.ifi.attempto.echocomp.Label;
import ch.uzh.ifi.attempto.echocomp.VSpace;
import nextapp.echo.app.Border;
import nextapp.echo.app.Color;
import nextapp.echo.app.Column;
import nextapp.echo.app.Component;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Grid;
import nextapp.echo.app.Insets;
import nextapp.echo.app.Table;
import nextapp.echo.app.table.TableCellRenderer;

public class GfLexiconEditor extends Column {

	private static final long serialVersionUID = 886670658151721528L;
	private static final Font FONT_CELL = new Font(Font.MONOSPACE, Font.BOLD, new Extent(12));
	private final Wiki mWiki;
	private final Grid mGridModules;
	private final Table mTable;
	private final Label mLabel;

	public GfLexiconEditor(Wiki wiki, GfLexiconEditorModel model) {
		mWiki = wiki;
		mGridModules = new Grid(5);
		mGridModules.setInsets(new Insets(0, 2, 8, 2));
		mLabel = new Label(makeLabel(model));
		mTable = new Table(model);
		mTable.setHeaderVisible(true);
		mTable.setDefaultHeaderRenderer(new TableCellRenderer() {

			private static final long serialVersionUID = 2675263246653547541L;

			@Override
			public Component getTableCellRendererComponent(Table t, Object v, int c, int r) {
				if (v == null) {
					return null;
				}
				Label l = new Label(v.toString());
				l.setFont(FONT_CELL);
				return l;
			}

		});
		mTable.setFont(new Font(Font.MONOSPACE, Font.PLAIN, new Extent(11)));
		mTable.setInsets(new Insets(6, 6, 6, 6));
		mTable.setBorder(new Border(1, Color.LIGHTGRAY, Border.SIDE_BOTTOM));
		mTable.setDefaultRenderer(Object.class, new GfLexiconEditorCellRenderer(mWiki));

		add(mGridModules);
		add(new VSpace(10));
		add(mLabel);
		add(new VSpace(10));
		add(mTable);
	}


	public void setModel(GfLexiconEditorModel model) {
		mTable.setModel(model);
		mLabel.setText(makeLabel(model));

		mGridModules.removeAll();
		for (ModuleElement oe :  model.getModules()) {
			mGridModules.add(new WikiLink(oe, oe.getWord(), mWiki, false));
		}
	}


	public GfLexiconEditorModel getModel() {
		return (GfLexiconEditorModel) mTable.getModel();
	}


	private String makeLabel(GfLexiconEditorModel model) {
		// TODO: localize
		return model.getRowCount() + " functions in " + (model.getColumnCount() - 1) + " modules";
	}

}