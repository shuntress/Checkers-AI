/**
 * A Game of Checkers
 * @author Nick DeArruda - jmakobi1@gmail.com
 * March 8 2012
 * 
 * A simple version of checkers which highlights any pieces that are allowed to move
 * on a given turn.  By default, players are allowed to de-select pieces before moving
 * them in case they were clicked on in error -- this can be changed however by setting
 * the constant ALLOW_ACCIDENTAL_CLICKS to 'false' (which is more similar to the actual
 * rules of checkers).
 * 
 */
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Point;
import java.awt.Toolkit;
import java.util.Random;

import java.io.*;

import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JButton;

public class Game {
	
	public static final int GRID_SIZE = 32;
	//allow the player to de-select pieces
	public static final boolean ALLOW_ACCIDENTAL_CLICKS = false;
	
	//whose turn it is
	public static Player turn;
	//pieces owned by each player; Player.BLACK.ordinal() = 0, Player.BLUE.ordinal() = 1
	public static int [] piecesOwned = new int [2];
	//the currently selected piece
	public static Point selection;
	//disables the player from selecting other pieces if multi-jumps are possible
	private static boolean lockSelection;
	
	public static Board board = new Board();
	public static Renderer renderer = new Renderer();
	public static Controller controller = new Controller();
	private static Random whoGoesFirst = new Random();
	
	public static void start() {
		//choose player at random to go first
		turn = (whoGoesFirst.nextBoolean() ? Player.BLACK : Player.BLUE);
		piecesOwned[0] = piecesOwned[1] = 12;
		selection = null;
		lockSelection = false;
		
		//game on
		board.set();
		renderer.repaint();
		prettyPrint();
	}
	
	//interface between the game and the controller; all moves are carried out through here
	public static void boardClicked(Point space) {
		//select a piece to move
		if (selection == null && !lockSelection && board.pieceIsMobile(space)) {
			Game.selection = space;
			lockSelection = !ALLOW_ACCIDENTAL_CLICKS;
		//try to move selected piece
		} else if (selection != null) {
			if (board.tryMove(selection, space)) {
				//tryMove() returns true if jump moves still have to be made; if that's the case,
				//then the player should be unable to select any other pieces
				lockSelection = true;
				selection = space;
			} else if (!Game.lockSelection) {
				selection = null;
			}
		}
		//redraw the board
		renderer.repaint();
	}
	
	public static void losePiece(Player p) {
		//subtract number of pieces owned and check for win condition
		--piecesOwned[p.ordinal()];
	}
	
	public static void endTurn() {
		prettyPrint();
		//board.pib(getState(true));
	
		//switch to the other player's turn
		turn = turn.other();
		lockSelection = false;
		
		//check if there are any pieces left
		if (piecesOwned[turn.ordinal()] == 0) {
			gameOver(turn.other());
		}
	}
	
	public static void gameOver(Player winner) {
		String s = (winner == Player.BLACK ? "Black" : "Blue");
		JOptionPane.showMessageDialog(renderer, "Game over: " + s + " has won.");
		start();
	}
	
	/*BLUE=1 BLACK=2 BLUE KING=3 BLACK KING=4*/
	public static int[][] getState(boolean compress)
	{
		if(compress)
			return board.compress(board.getBoard());
		else
			return board.getBoard();
	}
	
	public static void prettyPrint()
	{
		int[][] small=getState(true), large=getState(false);
		System.out.println("-------------------------");
		for(int y=0;y<small[0].length;y++)
		{
		
			for(int x=0;x<small.length;x++)
				System.out.print(small[x][y]);
			System.out.print("\t\t");
			for(int x=0;x<large.length;x++)
				System.out.print(large[x][y]);
			System.out.println();
		}
		System.out.println("-------------------------\n");
	}
	
	public static void printStateToFile(boolean compress) throws IOException
	{
		BufferedWriter bw = new BufferedWriter(new FileWriter("state.txt"));
		int[][] ra = getState(compress);
		System.out.print("Overriding old state...\nSaving state: ");
		for(int y=0;y<ra[0].length;y++)
		{
			for(int x=0;x<ra.length;x++)
			{
				System.out.print((char)(48+ra[x][y]));
				bw.write((char)(48+ra[x][y]));
			}
		}
		System.out.println();
		bw.flush();
		bw.close();
	}
	
	//set up the game window
	public static void main(String [] args) {
		JFrame frame = new JFrame();
		JButton butt = new JButton("Save state");
		butt.addActionListener(new ButtonListener());
		frame.setTitle("Let's Play Some Checkers");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.setResizable(false);
		frame.setLayout(new FlowLayout());
		Dimension screen = Toolkit.getDefaultToolkit().getScreenSize();
		frame.setLocation(screen.width / 2 - GRID_SIZE * 4, screen.height / 2 - GRID_SIZE * 4);
		renderer.setSize(GRID_SIZE * 8, GRID_SIZE * 8);
		frame.add(renderer);
		renderer.addMouseListener(controller);
		frame.add(butt);
		frame.pack();
		frame.setVisible(true);
		
		start();
	}
}