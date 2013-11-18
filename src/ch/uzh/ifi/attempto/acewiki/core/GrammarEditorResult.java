package ch.uzh.ifi.attempto.acewiki.core;

public interface GrammarEditorResult {
	boolean isSuccess();
	String getResultCode();
	String getCommand();
	String getMessage();
}
