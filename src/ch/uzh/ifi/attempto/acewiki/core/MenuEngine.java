package ch.uzh.ifi.attempto.acewiki.core;

import java.util.List;

public interface MenuEngine {
	
	public List<String> getMenuGroups();
	
	public int getColorShift(String menuGroup);
	
	public String getMenuGroup(String category);
	
	public List<String> getExtensibleCategories();
	
	public String getWordType(String extensibleCategory);
	
	public int getWordNumber(String extensibleCategory);

}
