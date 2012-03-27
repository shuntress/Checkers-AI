
public enum Player {
	BLACK,
	BLUE;
	
	public Player other() {
		//return the opposite player
		return (this.equals(Player.BLACK) ? Player.BLUE : Player.BLACK);
	}
}