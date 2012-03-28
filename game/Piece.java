
public class Piece  {
/*BLUE=1 BLACK=2 BLUE KING=3 BLACK KING=4*/
	public Player owner;
	public boolean isKing = false;
	
	public Piece(Player owner){
		this.owner = owner;
	}
}