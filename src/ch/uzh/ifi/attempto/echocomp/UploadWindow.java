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

package ch.uzh.ifi.attempto.echocomp;

import java.io.IOException;
import java.util.TooManyListenersException;

import nextapp.echo2.app.Alignment;
import nextapp.echo2.app.Column;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.Grid;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.Row;
import nextapp.echo2.app.event.ActionEvent;
import nextapp.echo2.app.event.ActionListener;
import nextapp.echo2.app.event.WindowPaneEvent;
import nextapp.echo2.app.event.WindowPaneListener;
import nextapp.echo2.app.filetransfer.UploadEvent;
import nextapp.echo2.app.filetransfer.UploadListener;
import nextapp.echo2.app.filetransfer.UploadSelect;
import nextapp.echo2.app.layout.GridLayoutData;

/**
 * This is an upload window that allows the user to choose a local file to be uploaded to the server.
 * 
 * @author Tobias Kuhn
 */
public class UploadWindow extends WindowPane implements ActionListener, UploadListener {

	private static final long serialVersionUID = -8594954833738936914L;

	private ActionListener actionListener;
	private String fileContent;
	private Label fileLabel;
	private GeneralButton openButton;
	private long maxFileSize = 0;
	private String actionCommand = "Upload";

	/**
	 * Creates a new upload window.
	 * 
	 * @param title The window title.
	 * @param message The message that is displayed above the upload button.
	 * @param parent The parent window.
	 * @param actionListener An action-listener or null.
	 */
	public UploadWindow(String title, String message, WindowPane parent, ActionListener actionListener) {
		this.actionListener = actionListener;
		setTitle(title);
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
				actionPerformed(new ActionEvent(UploadWindow.this, "Close"));
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

		for (String l : message.split("\\n")) {
			Label label = new Label(l);
			label.setFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(13)));
			messageColumn.add(label);
		}

		UploadSelect uploadSelect = new UploadSelect();
		try {
			uploadSelect.addUploadListener(this);
			uploadSelect.setSendButtonDisplayed(false);
			uploadSelect.setHeight(new Extent(40));
			uploadSelect.setWidth(new Extent(300));
			messageColumn.add(uploadSelect);
		} catch (TooManyListenersException ex) {
			ex.printStackTrace();
		}

		fileLabel = new Label();
		messageColumn.add(fileLabel);

		grid.add(messageColumn);

		Row buttonBar = new Row();
		buttonBar.setCellSpacing(new Extent(10));
		buttonBar.setInsets(new Insets(0, 0, 0, 10));
		openButton = new GeneralButton("Open", 80, this);
		openButton.setEnabled(false);
		buttonBar.add(openButton);
		buttonBar.add(new GeneralButton("Cancel", 80, this));
		GridLayoutData layout2 = new GridLayoutData();
		layout2.setAlignment(new Alignment(Alignment.CENTER, Alignment.BOTTOM));
		buttonBar.setLayoutData(layout2);
		grid.add(buttonBar);

		add(grid);

		if (parent != null) {
			setPositionX(new Extent(parent.getPositionX().getValue() + (parent.getWidth().getValue() - getWidth().getValue())/2));
			setPositionY(new Extent(parent.getPositionY().getValue() + (parent.getHeight().getValue() - getHeight().getValue())/2));
		}
	}

	/**
	 * Sets the maximum file size.
	 * 
	 * @param maxFileSize The maximum file size in bytes. 0 for unlimited size.
	 */
	public void setMaxFileSize(long maxFileSize) {
		this.maxFileSize = maxFileSize;
	}
	
	/**
	 * Sets the action command for the upload event.
	 * 
	 * @param actionCommand The action command.
	 */
	public void setActionCommand(String actionCommand) {
		this.actionCommand = actionCommand;
	}

	/**
	 * Returns the content of the uploaded file as a string.
	 * 
	 * @return The content of the uploaded file.
	 */
	public String getFileContent() {
		return fileContent;
	}

	public void actionPerformed(ActionEvent e) {
		setVisible(false);
		if (actionListener == null) return;
		if (e.getActionCommand().equals("Open")) {
			actionListener.actionPerformed(new ActionEvent(this, actionCommand));
		}
	}

	public void fileUpload(UploadEvent e) {
		if (maxFileSize > 0 && e.getSize() > maxFileSize) {
			fileContent = null;
			fileLabel.setText("The chosen file is too large (" + e.getSize() + " Bytes).");
			openButton.setEnabled(false);
			return;
		}
		try {
			byte[] b = new byte[e.getSize()];
			e.getInputStream().read(b, 0, e.getSize());
			fileContent = new String(b);
			String fileName = e.getFileName();
			if (fileName.length() > 15) fileName = fileName.substring(0, 15) + "...";
			fileLabel.setText("Chosen file: " + fileName + " (" + e.getSize() + " Bytes)");
			openButton.setEnabled(true);
		} catch (IOException ioe) {
			ioe.printStackTrace();
		}
	}

	public void invalidFileUpload(UploadEvent uploadEvent) {}

}
