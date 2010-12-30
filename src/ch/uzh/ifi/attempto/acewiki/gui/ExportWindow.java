// This file is part of AceWiki.
// Copyright 2008-2010, Tobias Kuhn.
// 
// AceWiki is free software: you can redistribute it and/or modify it under the terms of the GNU
// Lesser General Public License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
// 
// AceWiki is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
// even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General Public License along with AceWiki. If
// not, see http://www.gnu.org/licenses/.

package ch.uzh.ifi.attempto.acewiki.gui;

import java.io.IOException;
import java.io.OutputStream;

import nextapp.echo.app.Alignment;
import nextapp.echo.app.Column;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Grid;
import nextapp.echo.app.Insets;
import nextapp.echo.app.ListBox;
import nextapp.echo.app.Row;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import nextapp.echo.app.event.WindowPaneEvent;
import nextapp.echo.app.event.WindowPaneListener;
import nextapp.echo.app.layout.GridLayoutData;
import nextapp.echo.filetransfer.app.AbstractDownloadProvider;
import nextapp.echo.filetransfer.app.DownloadCommand;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.ontology.ACELexiconExporter;
import ch.uzh.ifi.attempto.acewiki.core.ontology.ACETextExporter;
import ch.uzh.ifi.attempto.acewiki.core.ontology.AceWikiDataExporter;
import ch.uzh.ifi.attempto.acewiki.core.ontology.LexiconTableExporter;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OWLXMLExporter;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OntologyExporter;
import ch.uzh.ifi.attempto.acewiki.core.ontology.StatementTableExporter;
import ch.uzh.ifi.attempto.echocomp.GeneralButton;
import ch.uzh.ifi.attempto.echocomp.Label;
import ch.uzh.ifi.attempto.echocomp.Style;
import ch.uzh.ifi.attempto.echocomp.VSpace;
import ch.uzh.ifi.attempto.echocomp.WindowPane;

/**
 * This is a window that allows the user to choose from different kinds of file types for
 * exporting the current knowledge base.
 * 
 * @author Tobias Kuhn
 */
public class ExportWindow extends WindowPane implements ActionListener {

	private static final long serialVersionUID = -8594954833738936914L;
	
	private Wiki wiki;
	
	private ListBox listBox;

	/**
	 * Creates a new export window.
	 * 
	 * @param wiki The wiki instance.
	 */
	public ExportWindow(Wiki wiki) {
		this.wiki = wiki;
		
		setTitle("Export");
		setTitleFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(13)));
		setModal(true);
		setWidth(new Extent(420));
		setHeight(new Extent(200));
		setResizable(false);
		setMovable(true);
		setTitleBackground(Style.windowTitleBackground);
		setStyleName("Default");

		addWindowPaneListener(new WindowPaneListener() {

			private static final long serialVersionUID = -3897741327122083261L;

			public void windowPaneClosing(WindowPaneEvent e) {
				actionPerformed(new ActionEvent(ExportWindow.this, "Close"));
			}

		});

		Grid grid = new Grid(1);
		grid.setInsets(new Insets(10, 10, 10, 0));
		grid.setColumnWidth(0, new Extent(400));
		grid.setRowHeight(0, new Extent(110));

		Column messageColumn = new Column();
		GridLayoutData layout1 = new GridLayoutData();
		layout1.setAlignment(new Alignment(Alignment.LEFT, Alignment.TOP));
		messageColumn.setLayoutData(layout1);

		Label label = new Label("Choose the type of export from the list:");
		label.setFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(13)));
		messageColumn.add(label);
		messageColumn.add(new VSpace());
		
		listBox = new ListBox(new String[] {
				"ACE Text, consistent (.ace.txt)",
				"ACE Text, full (.ace.txt)",
				"ACE Lexicon (.lex.pl)",
				"OWL Ontology, consistent (.owl)",
				"OWL Ontology, full (.owl)",
				"Lexicon Table (.csv)",
				"Statement Table (.csv)",
				"AceWiki data file (.acewikidata)"
		});
		listBox.setFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(11)));
		listBox.setBackground(Style.lightBackground);
		listBox.setHeight(new Extent(70));
		listBox.setSelectedIndex(0);
		messageColumn.add(listBox);
		
		grid.add(messageColumn);

		Row buttonBar = new Row();
		buttonBar.setCellSpacing(new Extent(10));
		buttonBar.setInsets(new Insets(0, 0, 0, 10));
		buttonBar.add(new GeneralButton("Export", 80, this));
		buttonBar.add(new GeneralButton("Cancel", 80, this));
		GridLayoutData layout2 = new GridLayoutData();
		layout2.setAlignment(new Alignment(Alignment.CENTER, Alignment.BOTTOM));
		buttonBar.setLayoutData(layout2);
		grid.add(buttonBar);

		add(grid);
	}

	public void actionPerformed(ActionEvent e) {
		setVisible(false);
		
		if (e.getActionCommand().equals("Export")) {
			
			final OntologyExporter exporter;
			final Ontology ontology = wiki.getOntology();
			String export = listBox.getSelectedValue().toString();
			boolean consistent = export.indexOf("consistent") > 0;
			
			if (export.startsWith("OWL Ontology")) {
				exporter = new OWLXMLExporter(ontology, consistent);
			} else if (export.startsWith("ACE Text")) {
				exporter = new ACETextExporter(ontology, consistent);
			} else if (export.startsWith("ACE Lexicon")) {
				exporter = new ACELexiconExporter(ontology);
			} else if (export.startsWith("Lexicon Table")) {
				exporter = new LexiconTableExporter(ontology);
			} else if (export.startsWith("Statement Table")) {
				exporter = new StatementTableExporter(ontology);
			} else if (export.startsWith("AceWiki data")) {
				exporter = new AceWikiDataExporter(ontology);
			} else {
				return;
			}
			
			AbstractDownloadProvider provider = new AbstractDownloadProvider() {
				
				private static final long serialVersionUID = 3491081007747916029L;
				
				public String getContentType() {
					return exporter.getContentType();
				}

				public String getFileName() {
					return ontology.getName() + exporter.getFileSuffix();
				}

				public void writeFile(OutputStream out) throws IOException {
					exporter.export(out);
				}
				
			};
			
			wiki.getApplication().enqueueCommand(new DownloadCommand(provider));
		}
	}

}
