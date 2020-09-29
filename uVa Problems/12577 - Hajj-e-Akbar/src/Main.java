import java.io.*;
import java.math.*;
import java.util.*;

public class Main {

	public static void main(String [] args){
		Scanner in = new Scanner(System.in);

		boolean guard = true;
		int casecount = 1;
		
		while(guard){
			String word = in.nextLine();
			if(word.equals("*")){
				guard = false;
			} else if (word.equals("Hajj")){
				System.out.printf("Case %d: Hajj-e-Akbar\n", casecount);
			} else {
				System.out.printf("Case %d: Hajj-e-Asghar\n", casecount);
			}
			casecount = casecount + 1;
		}
	}
}
