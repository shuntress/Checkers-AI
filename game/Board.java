import java.awt.Color;
import java.awt.GradientPaint;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Polygon;

public class Board {
	//8 x 8 array of game pieces
	private Piece [][] spaces = new Piece [8][8];
	
	//clear the board and set up all pieces
	public void set() {
		//set all spaces to null
		for (int y = 0; y < 8; ++y)
			for (int x = 0; x < 8; ++x)
				spaces[x][y] = null;
		
		//set 12 blue pieces every other space at the top of the board
		for (int y = 0; y < 3; ++y)
			for (int x = (y + 1) % 2; x < 8; x += 2)
				spaces[x][y] = new Piece(Player.BLUE);
		
		//set 12 black pieces in the same manner on the bottom
		for (int y = 5; y < 8; ++y)
			for (int x = (y + 1) % 2; x < 8; x += 2)
				spaces[x][y] = new Piece(Player.BLACK);
		printState();
	}
	
	//checks if space exists
	public boolean isValidSpace(Point space) {
		return space.x >= 0 && space.x <= 7 && space.y >= 0 && space.y <= 7;
	}
	
	//checks jump availability for entire board
	public boolean jumpsAreAvailable(Player p) {
		Point space = new Point();
		for (int y = 0; y < 8; ++y) {
			for (int x = 0; x < 8; ++x) {
				space.setLocation(x, y);
				if (!emptyAt(space) && jumpIsAvailable(space) && pieceAt(space).owner == p)
					return true;
			}
		}
		return false;
	}
	
	//checks jump availability for an individual piece
	public boolean jumpIsAvailable(Point src) {
		Point dest = new Point(), jump = new Point();
		Piece subject = pieceAt(src);
		
		//downward jumps
		if (subject.isKing || subject.owner == Player.BLUE) {
			//down-left
			dest.setLocation(src.x - 2, src.y + 2);
			jump.setLocation(src.x - 1, src.y + 1);
			if (isValidSpace(dest) && emptyAt(dest) && !emptyAt(jump) && pieceAt(jump).owner != subject.owner)
				return true;
			//down-right
			dest.x += 4;
			jump.x += 2;
			if (isValidSpace(dest) && emptyAt(dest) && !emptyAt(jump) && pieceAt(jump).owner != subject.owner)
				return true;
		}
		//upward jumps
		if (subject.isKing || subject.owner == Player.BLACK) {
			//up-left
			dest.setLocation(src.x - 2, src.y - 2);
			jump.setLocation(src.x - 1, src.y - 1);
			if (isValidSpace(dest) && emptyAt(dest) && !emptyAt(jump) && pieceAt(jump).owner != subject.owner)
				return true;
			//up-right
			dest.x += 4;
			jump.x += 2;
			if (isValidSpace(dest) && emptyAt(dest) && !emptyAt(jump) && pieceAt(jump).owner != subject.owner)
				return true;
		}	
		return false;
	}
	
	//checks for any available moves for a given space
	public boolean moveIsAvailable(Point src) {
		Point dest = new Point(src.x - 1, src.y - 1);
		if (isValidMove(src, dest)) return true;
		dest.x += 2;
		if (isValidMove(src, dest)) return true;
		dest.y += 2;
		if (isValidMove(src, dest)) return true;
		dest.x -= 2;
		if (isValidMove(src, dest)) return true;
		
		if (jumpIsAvailable(src)) return true;
		
		return false;
	}
	
	//determines if a move is possible, disregarding jump opportunities elsewhere on the board
	public boolean isValidMove(Point src, Point dest) {
		//check for moves that are invalid regardless of who's making them
		if (!isValidSpace(src) || !isValidSpace(dest) || emptyAt(src) || !emptyAt(dest)) 
			return false;
		
		//at this point, we know that a piece is trying to move to an empty space on the board
		Piece subject = pieceAt(src);
		Point jump = new Point((src.x + dest.x) / 2, (src.y + dest.y) / 2);
				
		//check for downward moves
		if (subject.isKing || subject.owner == Player.BLUE) {
			if (Math.abs(src.x - dest.x) == 1 && src.y - dest.y == -1) {
				return !jumpIsAvailable(src);
			} 
			if (Math.abs(src.x - dest.x) == 2 && src.y - dest.y == -2) {
				if (!emptyAt(jump) && pieceAt(jump).owner != subject.owner)
					return true;
			}
		}
		//check for upward moves
		if (subject.isKing || subject.owner == Player.BLACK) {
			if (Math.abs(src.x - dest.x) == 1 && src.y - dest.y == 1) {
				return !jumpIsAvailable(src);
			}
			if (Math.abs(src.x - dest.x) == 2 && src.y - dest.y == 2) {
				if (!emptyAt(jump) && pieceAt(jump).owner != subject.owner)
					return true;
			}
		}	
		return false;
	}
	
	//definitively determines whether a piece can be moved on the current turn, taking into
	//account all of the current player's jump opportunities, etc.
	public boolean pieceIsMobile(Point space) {
		return !emptyAt(space) && pieceAt(space).owner == Game.turn && moveIsAvailable(space) 
				&& jumpsAreAvailable(Game.turn) == jumpIsAvailable(space);
	}
	
	//attempts to move piece at src to dest
	//returns true if jump moves can still be made before the turn ends, 
	//otherwise false -- even if no move is made
	public boolean tryMove(Point src, Point dest) {
		Point jumpedSpace;
		//if the move is valid, carry it out
		if (isValidMove(src, dest)) {
			setSpace(dest, pieceAt(src));
			setSpace(src, null);
			
			//if a jump has occurred, kill the piece that was jumped over
			if (Math.abs(src.x - dest.x) == 2) {
				jumpedSpace = new Point((src.x + dest.x) / 2, (src.y + dest.y) / 2); 
				
				Game.losePiece(pieceAt(jumpedSpace).owner);
				setSpace(jumpedSpace, null);
			}
			
			//check for crowning of kings
			if (dest.y == 0 && pieceAt(dest).owner == Player.BLACK)
				pieceAt(dest).isKing = true;
			if (dest.y == 7 && pieceAt(dest).owner == Player.BLUE)
				pieceAt(dest).isKing = true;
			
			//only end the turn if no more jumps can be made
			if (!jumpIsAvailable(dest) || Math.abs(src.x - dest.x) == 1) {
				Game.endTurn();
				return false;
			} else {
				return true;
			}
 		}
		return false;
	}
	
	//returns piece at space
	public Piece pieceAt(Point space) {
		if (isValidSpace(space))
			return spaces[space.x][space.y];
		else
			throw new IndexOutOfBoundsException("Invalid coordinate");
	}
	
	public void setSpace(Point space, Piece piece) {
		if (isValidSpace(space))
			spaces[space.x][space.y] = piece;
		else
			throw new IndexOutOfBoundsException("Invalid coordinate");
	}
	
	public boolean emptyAt(Point space) {
		return pieceAt(space) == null;
	}
	
	public void printState()
	{
		for(int y=0;y<spaces.length;y++)
		{
			System.out.print(y+": ");
			for(int x=0;x<spaces[y].length;x++)
				System.out.print(((spaces[x][y]!=null)?spaces[x][y].owner:"null")+" ");
			System.out.print("***\n");
		}
	}
	public int[][] getBoard()
	{
		int[][] out = new int[8][8];
		int t=0;
		for(int y=0;y<spaces.length;y++)
		{
			for(int x=0;x<spaces[y].length;x++)
			{	if(spaces[x][y]!=null)
					if(spaces[x][y].owner==Player.BLACK)
						t=2;
					else
						t=1;
				else
					t=0;
				out[x][y]=t;
				if (spaces[x][y] != null && spaces[x][y].isKing) out[x][y]+=2;
			}
		}
		return out;
	}
	public int[][] compress(int[][] in)
	{
		int[][] out = new int[4][8];
		int xt=0;
		for(int y=0;y<in.length;y++)
		{
			xt=0;
			for(int x=(y+1)%2;x<in[y].length;x+=2)
				out[xt++][y]=in[x][y];
		}
		return out;
	}
	public void pib(int[][] in)
	{
		for(int y=0;y<in[0].length;y++)
		{
			for(int x=0;x<in.length;x++)
				System.out.print(in[x][y]+" ");
			System.out.print("\n");
		}
		System.out.println();
	}
//*****************************************************************************
//everything below here is only concerned with rendering the board and pieces
	
	public void render(Graphics2D g) {
		int size = Game.GRID_SIZE;

		//create paints
		Point begin = new Point(0,0), end = new Point(size * 8, size * 8);
		GradientPaint redSpaces = new GradientPaint(begin, new Color(0xFF3333), end, new Color(0xAA2222));
		GradientPaint redBorder = new GradientPaint(begin, new Color(0x993333), end, new Color(0x551111));
		GradientPaint blackSpaces = new GradientPaint(begin, new Color(0x888888), end, new Color(0x444444));
		GradientPaint blackBorder = new GradientPaint(begin, new Color(0x444444), end, new Color(0x111111));
		Color trimLight = new Color(0xEEAA11), trimDark = new Color(0x995511);
				
		//draw alternating red and black spaces with gradient and border effects
		for (int y = 0; y < 8; ++y) {
			for (int x = y % 2; x < 8; x += 2) {
				g.setPaint(redBorder);
				g.fillRect(y * size, x * size, size, size);
				g.setPaint(redSpaces);
				g.fillRect(y * size + 3, x * size + 3, size - 6, size - 6);
			}
		}		
		for (int y = 0; y < 8; ++y) {
			for (int x = (y + 1) % 2; x < 8; x += 2) {
				g.setPaint(blackBorder);
				g.fillRect(y * size, x * size, size, size);
				g.setPaint(blackSpaces);
				g.fillRect(y * size + 3, x * size + 3, size - 6, size - 6);
			}
		}
		
		//draw some fancy lighted trim between the spaces
		g.setPaint(trimLight);
		for (int i = 0; i < 8; ++i) {
			g.drawLine(i * size, 0, i * size, 8 * size);
			g.drawLine(0, i * size, 8 * size, i * size);
		}
		g.setPaint(trimDark);
		for (int i = 0; i < 8; ++i) {
			g.drawLine(i * size + size - 1, 0, i * size + size - 1, 8 * size);
			g.drawLine(0, i * size + size - 1, 8 * size, i * size + size - 1);
		}
		
		//draw the game pieces
		drawPieces(g);
	}

	private void drawPieces(Graphics2D g) {
		int size = Game.GRID_SIZE;
		int xpos, ypos;
		Point space = new Point();

		Color blue = new Color(0x2255AA), black = new Color(0x393939);
		Color blueShadow = new Color(0x113377), blackShadow = new Color(0x111111);
		Color blueHighlight = new Color(0x6699FF), blackHighlight = new Color(0x888888);
		Color main, shadow, highlight;

		for (int y = 0; y < 8; ++y) {
			for (int x = 0; x < 8; ++x) {
				space.setLocation(x, y);
				if (!emptyAt(space)) {
					if (pieceAt(space).owner == Player.BLUE) {
						main = blue;
						shadow = blueShadow;
						highlight = blueHighlight;
					} else {
						main = black;
						shadow = blackShadow;
						highlight = blackHighlight;
					}
					xpos = x * size; ypos = y * size;
					g.setPaint(highlight); g.fillOval(xpos + 4, ypos + 4, 24, 20);
					g.setPaint(shadow); g.fillOval(xpos + 4, ypos + 8, 24, 20);
					g.setPaint(main); g.fillOval(xpos + 4, ypos + 5, 24, 20);
					g.setPaint(shadow); g.fillOval(xpos + 8, ypos + 9, 16, 12);
					g.setPaint(main); g.fillOval(xpos + 8, ypos + 10, 16, 12);
					
					if (Game.selection == null && pieceIsMobile(space)) {
						g.setPaint(new Color(0xFFFFFF)); g.drawOval(xpos + 3, ypos + 4, 26, 24);
					}
					
					if (pieceAt(space).isKing) {
						xpos += 12; ypos += 12;
						Polygon crown = new Polygon();
						crown.addPoint(xpos, ypos); crown.addPoint(xpos + 2, ypos + 4);
						crown.addPoint(xpos + 4, ypos); crown.addPoint(xpos + 6, ypos + 4);
						crown.addPoint(xpos + 8, ypos); crown.addPoint(xpos + 10, ypos + 6);
						crown.addPoint(xpos, ypos + 6); crown.addPoint(xpos, ypos);
						g.setPaint(new Color(0xFFCC66)); g.fillPolygon(crown);
					}
				}
			}
		}
		if (Game.selection != null) {
			g.setPaint(new Color(0x55FFFFFF, true));
			g.fillOval(Game.selection.x * size + 3, Game.selection.y * size + 4, 26, 24);
			g.setPaint(new Color(0xFFFFFF));
			g.drawOval(Game.selection.x * size + 3, Game.selection.y * size + 4, 26, 24);
		}
	}
}
