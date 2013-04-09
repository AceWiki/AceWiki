package ch.uzh.ifi.attempto.acewiki.core;

import java.util.HashMap;
import java.util.Map;

public class AceWikiConfig {

	private Map<String, String> parameters;
	private UserProvider userProvider;

	public AceWikiConfig(Map<String, String> parameters, UserProvider userProvider) {
		this.parameters = parameters;
		this.userProvider = userProvider;
	}

	public Map<String, String> getParameters() {
		return new HashMap<String, String>(parameters);
	}

	/**
	 * Returns the value of the given parameter. These parameters are defined in the web.xml file
	 * of the web application.
	 *
	 * @param paramName The parameter name.
	 * @return The value of the parameter.
	 */
	public String getParameter(String paramName) {
		return parameters.get(paramName);
	}

	public boolean hasParameter(String paramName) {
		return parameters.get(paramName) != null;
	}

	/**
	 * Returns whether the login features are enabled.
	 *
	 * @return true if login is enabled.
	 */
	public boolean isLoginEnabled() {
		return "yes".equals(getParameter("login"));
	}

	/**
	 * Returns whether login is required for viewing the wiki data.
	 *
	 * @return true if login is required for viewing.
	 */
	public boolean isLoginRequiredForViewing() {
		if (!isLoginEnabled()) return false;
		return "yes".equals(getParameter("login_required"));
	}

	/**
	 * Returns whether login is required for editing the wiki data.
	 *
	 * @return true if login is required for editing.
	 */
	public boolean isLoginRequiredForEditing() {
		if (!isLoginEnabled()) return false;
		if (isLoginRequiredForViewing()) return true;
		return "edit".equals(getParameter("login_required"));
	}

	/**
	 * Returns whether the user registration is open to everyone.
	 *
	 * @return true if the user registration is open.
	 */
	public boolean isUserRegistrationOpen() {
		if (!isLoginEnabled()) return false;
		return !"no".equals(getParameter("register"));
	}

	/**
	 * Returns true if this wiki is set to be read-only.
	 *
	 * @return true if this wiki is read-only.
	 */
	public boolean isReadOnly() {
		return "on".equals(getParameter("readonly"));
	}

	/**
	 * Returns true if language switching is enabled.
	 * 
	 * @return true if language switching is enabled.
	 */
	public boolean isLanguageSwitchingEnabled() {
		return !"off".equals(getParameter("language_switching"));
	}

	/**
	 * Returns true if comment feature is enabled.
	 * 
	 * @return true if enabled.
	 */
	public boolean isCommentingEnabled() {
		if ("on".equals(getParameter("comments"))) return true;
		User u = userProvider.getUser();
		if (u != null && u.hasRight("write_comments")) return true;
		return false;
	}

	/**
	 * Returns true if comments are disabled and hidden.
	 * 
	 * @return true if hidden.
	 */
	public boolean isCommentHidingEnabled() {
		return "hide".equals(getParameter("comments"));
	}

	/**
	 * Returns true if retract/reassert actions on sentences are enabled.
	 * 
	 * @return true if enabled.
	 */
	public boolean isRetractReassertEnabled() {
		return !"off".equals(getParameter("retractreassert"));
	}

	public boolean isDetailsPageEnabled() {
		return !"off".equals(getParameter("details_page"));
	}

	public boolean isTranslationsPageEnabled() {
		return !"off".equals(getParameter("translations_page"));
	}

	public boolean isGrammarIntegrationEnabled() {
		return "on".equals(getParameter("grammar_integration"));
	}

	public boolean isAutodisambiguationEnabled() {
		return "on".equals(getParameter("autodisambiguation"));
	}

}
