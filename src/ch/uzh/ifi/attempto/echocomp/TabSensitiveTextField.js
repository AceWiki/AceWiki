// This file is part of AceWiki.
// Copyright 2008-2012, AceWiki developers.
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

if (!Core.get(window, ["Attempto"])) {
	Core.set(window, ["Attempto"], {});
}

Attempto.TabSensitiveTextField = Core.extend(Echo.TextField, {

	$load : function() {
		Echo.ComponentFactory.registerType("Attempto.TabSensitiveTextField", this);
	},
	
	componentType : "Attempto.TabSensitiveTextField"

});

Attempto.TabSensitiveTextField.Peer = Core.extend(Echo.Sync.TextField, {

	$load : function() {
		Echo.Render.registerPeer("Attempto.TabSensitiveTextField", this);
	},

	$construct : function() {
		Echo.Sync.TextField.call(this);
	},
	
	clientKeyDown : function(keyEvent) {
		var status = Echo.Sync.TextField.prototype.clientKeyDown.call(this, keyEvent);
		if (keyEvent.keyCode == 9) {
			this.component.doAction();
		}
		return status;
	}

});
