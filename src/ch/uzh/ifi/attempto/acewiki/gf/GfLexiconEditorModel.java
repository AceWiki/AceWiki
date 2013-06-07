package ch.uzh.ifi.attempto.acewiki.gf;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import ch.uzh.ifi.attempto.acewiki.core.ModuleElement;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;

import com.google.common.collect.HashBasedTable;
import com.google.common.collect.ImmutableList;
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

	// TODO: using ; in the linearization definition is not supported
	private static final Pattern PATTERN_LIN = Pattern.compile("\\s*([A-Za-z_][A-Za-z0-9_']*)\\s*=(.+);");
	private static final Pattern PATTERN_SPLIT = Pattern.compile("^(.*concrete.+of.+\\nlin\\n)(.*)}\\s*$", Pattern.DOTALL);

	private final Table<String, ModuleElement, Object> funToModuleToLin = HashBasedTable.create();
	private final List<String> mFuns;
	private final List<ModuleElement> mModules;
	private final Map<ModuleElement, String> mModuleToHeader = Maps.newHashMap();

	public GfLexiconEditorModel(Ontology ont, final String language) {
		for (ModuleElement gfModule : ont.getOntologyElements(ModuleElement.class)) {
			refreshModuleElement(gfModule);
		}
		mFuns = Lists.newArrayList(funToModuleToLin.rowKeySet());
		mModules = Lists.newArrayList(funToModuleToLin.columnKeySet());
		Collections.sort(mFuns);

		/*
		 * We sort the columns so that the module for the selected language comes first.
		 * TODO: This does not always work correctly, e.g. incomplete modules are not handled.
		 */
		Collections.sort(mModules, new Comparator<ModuleElement>() {
			@Override
			public int compare(ModuleElement arg1, ModuleElement arg2) {
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
		return funToModuleToLin.get(fun, getModuleElement(column));
	}

	@Override
	public String getColumnName(int column) {
		if (column == 0) {
			return null;
		}
		return getModuleElement(column).getWord();
	}


	public void setValueAt(Object newValue, int column, int row) {
		ModuleElement moduleElement = getModuleElement(column);
		// Refresh module
		refreshModuleElement(moduleElement);
		String header = mModuleToHeader.get(moduleElement);
		if (header != null) {
			funToModuleToLin.put(mFuns.get(row), moduleElement, newValue);
			moduleElement.replaceModuleContent(header + makeModuleSource(moduleElement) + "}");
		}
		fireTableCellUpdated(column, row);
	}


	/**
	 * @param column table column
	 * @return module element that corresponds to the given column
	 */
	public ModuleElement getModuleElement(int column) {
		if (column > 0 && column <= mModules.size()) {
			return mModules.get(column - 1);
		} else {
			throw new IllegalArgumentException();
		}
	}


	public List<ModuleElement> getModules() {
		return ImmutableList.copyOf(mModules);
	}


	private boolean refreshModuleElement(ModuleElement gfModule) {
		String content = gfModule.getModuleContent().getText();

		Matcher matcherSplit = PATTERN_SPLIT.matcher(content);
		if (! matcherSplit.matches()) {
			mModuleToHeader.remove(gfModule);
			return false;
		}
		mModuleToHeader.put(gfModule, matcherSplit.group(1));
		Matcher matcherLins = PATTERN_LIN.matcher(matcherSplit.group(2));
		while (matcherLins.find()) {
			String fun = matcherLins.group(1);
			String lin = matcherLins.group(2).trim();
			funToModuleToLin.put(fun, gfModule, lin);
		}
		return true;
	}


	/**
	 * <p>For the given module, return its linearization definitions.
	 * If a definition is missing for a function then the respective entry is not included.</p>
	 */
	private StringBuilder makeModuleSource(ModuleElement module) {
		StringBuilder sb = new StringBuilder();
		for (String fun : mFuns) {
			Object value = funToModuleToLin.get(fun, module);
			if (value != null && ! "".equals(value.toString())) {
				sb.append(fun);
				sb.append(" = ");
				sb.append(value);
				sb.append(" ;\n");
			}
		}
		return sb;
	}
}