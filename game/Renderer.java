import java.awt.Canvas;
import java.awt.Graphics;
import java.awt.Graphics2D;

public class Renderer extends Canvas {

	private static final long serialVersionUID = 1L;

	//draw the board
	public void paint(Graphics g) {
		Game.board.render((Graphics2D) g);
	}
	
	public void update(Graphics g) {
		paint(g);
	}
}