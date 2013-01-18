package ch.uzh.ifi.attempto.acewiki.gui;

import nextapp.echo.app.WindowPane;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Task;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;

public class SentenceAction {

	private String mTitle;
	private String mDesc;
	private String mMsgConfirm;
	private String mMsgProgress;

	SentenceAction(String title, String desc, String msgConfirm, String msgProgress) {
		mTitle = title;
		mDesc = desc;
		mMsgConfirm = msgConfirm;
		mMsgProgress = msgProgress;
	}


	SentenceAction(String title, String desc, String msgConfirm) {
		this(title, desc, msgConfirm, "This sentence is being processed");
	}


	public String getTitle() {
		return mTitle;
	}


	public String getDesc() {
		return mDesc;
	}


	public boolean hasTitle(String title) {
		return mTitle.equals(title);
	}


	public void performAction(final Wiki wiki, final Executable executable) {
		wiki.enqueueStrongAsyncTask(
				"Updating",
				mMsgProgress,
				new Task() {
					public void run() {
						executable.execute();
					}
					public void updateGUI() {
						wiki.update();
						wiki.refresh();
					}
				}
				);
	}


	public MessageWindow getYesNoDialog(WindowPane parent, ActionListener listener) {
		return new MessageWindow(mTitle, mMsgConfirm, parent, listener, "Yes", "No");		
	}

}