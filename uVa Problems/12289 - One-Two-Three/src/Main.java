import java.io.*;
import java.math.*;
import java.util.*;

public class Main {

	public static void main(String [] args){
		Scanner in = new Scanner(System.in);
		int cases = in.nextInt();
		in.nextLine();

		for(int i = 0; i < cases; i++){
			String word = in.nextLine();
			if (word.length() == 5){
				System.out.println("3");
			} else if (word.matches("[a-z]ne") || word.matches("on[a-z]") || word.matches("o[a-z]e")) {
				System.out.println("1");
			} else {
				System.out.println("2");
			}
		}
	}
}
