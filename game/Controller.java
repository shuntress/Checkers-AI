import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

//handles user input
public class Controller implements MouseListener {	

	//convert mouse coordinates to game board coordinates, and let the game decide what to do
	//intercept this method using the attribute Game.turn to introduce non-human play
	public void mousePressed(MouseEvent e) {
		Game.boardClicked(new Point(e.getPoint().x / Game.GRID_SIZE, e.getPoint().y / Game.GRID_SIZE));
	}

	public void mouseReleased(MouseEvent e) {
	}
	
	public void mouseClicked(MouseEvent e) {
	}

	public void mouseEntered(MouseEvent e) {
	}

	public void mouseExited(MouseEvent e) {
	}
}