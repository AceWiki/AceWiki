package ch.uzh.ifi.attempto.acewiki.gf;

import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.ModuleElement;
import ch.uzh.ifi.attempto.acewiki.core.ModuleElement.InvalidSyntaxException;
import ch.uzh.ifi.attempto.acewiki.gui.Executable;
import ch.uzh.ifi.attempto.acewiki.gui.TextEditorDialog;
import ch.uzh.ifi.attempto.echocomp.Label;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;
import ch.uzh.ifi.attempto.echocomp.SimpleErrorMessageWindow;
import ch.uzh.ifi.attempto.gfservice.GfModule;
import ch.uzh.ifi.attempto.gfservice.GfServiceException;
import nextapp.echo.app.Button;
import nextapp.echo.app.Component;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Table;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import nextapp.echo.app.table.TableCellRenderer;

public class GfLexiconEditorCellRenderer implements TableCellRenderer {

	private static final long serialVersionUID = 2047282097510552137L;
	private static final Font FONT_CELL = new Font(Font.MONOSPACE, Font.PLAIN, new Extent(11));

	private final Wiki mWiki;

	public GfLexiconEditorCellRenderer(Wiki wiki) {
		mWiki = wiki;
	}

	public Component getTableCellRendererComponent(final Table table, Object value, final int column, final int row) {

		// Non-empty string is required to make the button clickable
		final String valueAsString = (value == null ? "\u00A0" : value.toString());

		if (column > 0 && mWiki.isEditable()) {
			final GfLexiconEditorModel model = (GfLexiconEditorModel) table.getModel();
			Button b = new Button(valueAsString);
			b.addActionListener(new ActionListener() {
				private static final long serialVersionUID = -7968619997506232092L;

				@Override
				public void actionPerformed(ActionEvent arg0) {
					TextEditorDialog.Builder editor = new TextEditorDialog.Builder(valueAsString)
					.setTitle(model.getColumnName(column) + " linearization for function " + model.getValueAt(0, row))
					.setSize(600, 200)
					.setFont(new Font(Font.MONOSPACE, Font.PLAIN, new Extent(12)))
					.setPositiveButton(new Executable() {
						@Override
						public void execute(Object... args) {
							model.setValueAt(args[0], column, row);
							ModuleElement moduleElement = model.getModuleElement(column);
							boolean success = parse(moduleElement);
							// If there were no syntax errors then push the grammar to the server.
							if (success) {
								GfEngine engine = (GfEngine) mWiki.getEngine();
								try {
									engine.getGfGrammar().upload(
											new GfModule(moduleElement.getWord(),
													moduleElement.getModuleContent().getText()));
								} catch (GfServiceException e) {
									mWiki.showWindow(new MessageWindow("Error", "Push to server failed."));
									e.printStackTrace();
								}
							}
						}
					});
					mWiki.showWindow(editor.create());
				}

			});
			return b;
		}

		// Return plain content
		Label l = new Label(valueAsString);
		l.setFont(FONT_CELL);
		return l;

	}


	private boolean parse(ModuleElement moduleElement) {
		try {
			// TODO: this blocks, do it in the background
			moduleElement.parse();
		} catch (InvalidSyntaxException iex) {
			mWiki.showWindow(new SimpleErrorMessageWindow("Syntax Error", iex.getText()));
			return false;
		} catch (Exception ex) {
			mWiki.showWindow(new SimpleErrorMessageWindow("Error", ex.getMessage()));
			return false;
		}
		return true;
	}

}