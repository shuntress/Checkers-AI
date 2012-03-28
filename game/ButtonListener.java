import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
public class ButtonListener implements ActionListener
{

	public void actionPerformed(ActionEvent e)
	{
		try{
		Game.printStateToFile(true);
		}
		catch(Exception x){System.out.println("Something went horribly wrong: "+x);}
	}
}