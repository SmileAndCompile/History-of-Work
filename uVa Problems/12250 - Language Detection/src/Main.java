import java.io.*;
import java.math.*;
import java.util.*;

public class Main {

	public static void main(String [] args){
		Scanner in = new Scanner(System.in);

		boolean guard = true;
		int count = 1;

		while(guard){
			String word = in.nextLine();
			if (word.equals("#")){
				guard = false;
			} else if (word.equals("HELLO")){
				System.out.printf("Case %d: ENGLISH\n", count);
			} else if (word.equals("HOLA")){
				System.out.printf("Case %d: SPANISH\n", count);
			} else if (word.equals("HALLO")){
				System.out.printf("Case %d: GERMAN\n", count);
			} else if (word.equals("BONJOUR")){
				System.out.printf("Case %d: FRENCH\n", count);
			} else if (word.equals("CIAO")){
				System.out.printf("Case %d: ITALIAN\n", count);
			} else if (word.equals("ZDRAVSTVUJTE")){
				System.out.printf("Case %d: RUSSIAN\n", count);
			} else {
				System.out.printf("Case %d: UNKNOWN\n", count);
			}
			count = count + 1;
		}
	}
}
