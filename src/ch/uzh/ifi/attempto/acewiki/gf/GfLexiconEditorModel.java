package ch.uzh.ifi.attempto.acewiki.gf;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import ch.uzh.ifi.attempto.acewiki.core.Ontology;

import com.google.common.collect.HashBasedTable;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.google.common.collect.Table;

import nextapp.echo.app.table.AbstractTableModel;
import nextapp.echo.app.table.TableModel;

/**
 * <p>TODO: very experimental</p>
 *
 * <p>Represents a set of the GF modules as a table where rows are functions, columns are the modules,
 * and each cell contains the linearization definition of the given function in the given module.
 * The table is compiled only from the source files.</p>
 *
 * <p>Note that we only consider modules whose source has a certain simple structure which
 * we can easily parse with a regex.</p>
 *
 * @author Kaarel Kaljurand
 */
public class GfLexiconEditorModel extends AbstractTableModel implements TableModel {

	private static final long serialVersionUID = -2494830121762821312L;

	private static final Pattern PATTERN_LIN = Pattern.compile("\\s*([A-Za-z_][A-Za-z0-9_']*)\\s*=\\s*(.+)\\s*;\\s*");
	private static final Pattern PATTERN_HEADER = Pattern.compile("^(.*concrete.*)\\nlin\\n(.*)}\\s*$", Pattern.DOTALL);

	private final Table<String, String, String> funToModuleToLin = HashBasedTable.create();

	private final List<String> mFuns;
	private final List<String> mModules;

	public GfLexiconEditorModel(Ontology ont) {
		Set<String> funs = Sets.newHashSet();
		for (TypeGfModule gfModule : ont.getOntologyElements(TypeGfModule.class)) {
			String content = gfModule.getModuleContent().getText();

			Matcher m1 = PATTERN_HEADER.matcher(content);
			if (! m1.matches()) {
				continue;
			}

			String module = gfModule.getWord();
			// TODO: save m1.group(1) for later
			Matcher m = PATTERN_LIN.matcher(m1.group(2));
			while (m.find()) {
				String fun = m.group(1);
				String lin = m.group(2);
				funs.add(fun);
				funToModuleToLin.put(fun, module, lin);
			}
		}
		mFuns = Lists.newArrayList(funToModuleToLin.rowKeySet());
		mModules = Lists.newArrayList(funToModuleToLin.columnKeySet());
		Collections.sort(mFuns);
		Collections.sort(mModules);
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
		String module = mModules.get(column - 1);
		return funToModuleToLin.get(fun, module);
	}

	@Override
	public String getColumnName(int column) {
		if (column == 0) {
			return "Function";
		}
		return mModules.get(column - 1);
	}

}