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

import nextapp.echo2.app.Alignment;
import nextapp.echo2.app.Column;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.Grid;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.ListBox;
import nextapp.echo2.app.Row;
import nextapp.echo2.app.event.ActionEvent;
import nextapp.echo2.app.event.ActionListener;
import nextapp.echo2.app.event.WindowPaneEvent;
import nextapp.echo2.app.event.WindowPaneListener;
import nextapp.echo2.app.filetransfer.Download;
import nextapp.echo2.app.filetransfer.DownloadProvider;
import nextapp.echo2.app.layout.GridLayoutData;
import ch.uzh.ifi.attempto.acewiki.Wiki;
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
				"Consistent ACE Text (.ace.txt)",
				"Full ACE Text (.ace.txt)",
				"ACE Lexicon (.lex.pl)",
				"Consistent OWL Ontology (.owl)",
				"Full OWL Ontology (.owl)"
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
			final String ending;
			final String content;
			final String contenttype;
			String export = listBox.getSelectedValue().toString();
			if (export.startsWith("Consistent OWL Ontology")) {
				ending = ".owl";
				contenttype = "application/owl+xml";
				content = wiki.getOntology().getOWLOntologyAsXML(true);
			} else if (export.startsWith("Full OWL Ontology")) {
				ending = ".owl";
				contenttype = "application/owl+xml";
				content = wiki.getOntology().getOWLOntologyAsXML(false);
			} else if (export.startsWith("Consistent ACE Text")) {
				ending = ".ace.txt";
				contenttype = "text/plain";
				content = wiki.getOntology().getACEText(true);
			} else if (export.startsWith("Full ACE Text")) {
				ending = ".ace.txt";
				contenttype = "text/plain";
				content = wiki.getOntology().getACEText(false);
			} else if (export.startsWith("ACE Lexicon")) {
				ending = ".lex.pl";
				contenttype = "text/plain";
				content = wiki.getOntology().getLexiconDef();
			} else {
				return;
			}
			
			DownloadProvider provider = new DownloadProvider() {
				
				public String getContentType() {
					return contenttype;
				}

				public String getFileName() {
					return wiki.getOntology().getName() + ending;
				}

				public int getSize() {
					return content.length();
				}

				public void writeFile(OutputStream out) throws IOException {
					out.write(content.getBytes());
					out.close();
				}
				
			};
			
			wiki.getApplication().enqueueCommand(new Download(provider, true));
		}
	}

}
