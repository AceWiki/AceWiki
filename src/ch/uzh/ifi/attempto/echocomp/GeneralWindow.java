package ch.uzh.ifi.attempto.echocomp;

import nextapp.echo.app.Extent;
import nextapp.echo.app.WindowPane;

public class GeneralWindow extends WindowPane {

	private static final long serialVersionUID = 3862130026472878855L;

	public void setCentered(WindowPane parent) {
		if (parent != null && parent.getPositionX() != null) {
			int px = parent.getPositionX().getValue();
			int py = parent.getPositionY().getValue();
			int pw = parent.getWidth().getValue();
			int ph = parent.getHeight().getValue();
			setPositionX(new Extent(px + (pw - getWidth().getValue())/2));
			setPositionY(new Extent(py + (ph - getHeight().getValue())/2));
		}
	}
}
