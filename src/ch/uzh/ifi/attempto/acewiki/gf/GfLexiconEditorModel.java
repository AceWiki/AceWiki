package ch.uzh.ifi.attempto.acewiki.gf;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import ch.uzh.ifi.attempto.acewiki.core.Article;
import ch.uzh.ifi.attempto.acewiki.core.Comment;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;

import com.google.common.collect.HashBasedTable;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Table;

import nextapp.echo.app.table.AbstractTableModel;
import nextapp.echo.app.table.TableModel;

/**
 * <p>Represents a set of the GF modules as a table where rows are functions, columns are the modules,
 * and each cell contains the linearization definition of the given function in the given module.
 * The table is compiled only from the source files.</p>
 *
 * <p>Note that we only consider modules whose source has a certain simple structure which
 * we can easily parse with a regex. The module must be a concrete module with a single "lin"
 * on its own line, separating the "header" of the module from the linearization definitions.</p>
 *
 * @author Kaarel Kaljurand
 */
public class GfLexiconEditorModel extends AbstractTableModel implements TableModel {

	private static final long serialVersionUID = -2494830121762821312L;

	private static final Pattern PATTERN_LIN = Pattern.compile("\\s*([A-Za-z_][A-Za-z0-9_']*)\\s*=\\s*(.+)\\s*;\\s*");
	private static final Pattern PATTERN_SPLIT = Pattern.compile("^(.*concrete.*\\nlin\\n)(.*)}\\s*$", Pattern.DOTALL);

	private final Ontology mOntology;
	private final Table<String, TypeGfModule, Object> funToModuleToLin = HashBasedTable.create();
	private final List<String> mFuns;
	private final List<TypeGfModule> mModules;
	private final Map<TypeGfModule, String> mModuleToHeader = Maps.newHashMap();

	public GfLexiconEditorModel(Ontology ont, final String language) {
		mOntology = ont;
		for (TypeGfModule gfModule : ont.getOntologyElements(TypeGfModule.class)) {
			String content = gfModule.getModuleContent().getText();

			Matcher matcherSplit = PATTERN_SPLIT.matcher(content);
			if (! matcherSplit.matches()) {
				continue;
			}
			mModuleToHeader.put(gfModule, matcherSplit.group(1));
			Matcher matcherLins = PATTERN_LIN.matcher(matcherSplit.group(2));
			while (matcherLins.find()) {
				String fun = matcherLins.group(1);
				String lin = matcherLins.group(2);
				funToModuleToLin.put(fun, gfModule, lin);
			}
		}
		mFuns = Lists.newArrayList(funToModuleToLin.rowKeySet());
		mModules = Lists.newArrayList(funToModuleToLin.columnKeySet());
		Collections.sort(mFuns);

		/*
		 * We sort the columns so that the modules for the selected language comes first.
		 * TODO: This does not always work correctly, e.g. incomplete modules are not handled.
		 */
		Collections.sort(mModules, new Comparator<TypeGfModule>() {
			@Override
			public int compare(TypeGfModule arg1, TypeGfModule arg2) {
				String moduleName1 = arg1.getWord();
				if (moduleName1.equals(language)) {
					return -1;
				}
				String moduleName2 = arg2.getWord();
				if (moduleName2.equals(language)) {
					return 1;
				}
				return moduleName1.compareTo(moduleName2);
			}

		});
	}

	@Override
	public int getColumnCount() {
		return 1 + mModules.size();
	}

	@Override
	public int getRowCount() {
		return mFuns.size();
	}

	@Override
	public Object getValueAt(int column, int row) {
		String fun = mFuns.get(row);
		if (column == 0) {
			return fun;
		}
		return funToModuleToLin.get(fun, mModules.get(column - 1));
	}

	@Override
	public String getColumnName(int column) {
		if (column == 0) {
			return null;
		}
		return mModules.get(column - 1).getWord();
	}


	public void setValueAt(Object newValue, int column, int row) {
		funToModuleToLin.put(mFuns.get(row), mModules.get(column - 1), newValue);
		fireTableCellUpdated(column, row);
	}


	public void writeColumn(int column) {
		TypeGfModule module = mModules.get(column - 1);
		Article article = module.getArticle();
		Comment comment = mOntology.getStatementFactory().createComment(getText(module), article);
		article.edit(article.getStatements().get(0), comment);
	}


	private String getText(TypeGfModule module) {
		StringBuilder sb = new StringBuilder();
		for (String fun : mFuns) {
			Object value = funToModuleToLin.get(fun, module);
			sb.append(fun);
			sb.append(" = ");
			sb.append(value);
			sb.append(" ;\n");
		}
		return mModuleToHeader.get(module) + sb.toString() + "}";
	}
}