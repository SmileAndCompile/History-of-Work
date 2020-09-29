import java.io.*;
import java.math.*;
import java.util.*;

public class Main {

	public static void main(String [] args){
		Scanner in = new Scanner(System.in);
		
		int cases, a, b;

		cases = in.nextInt();

		for(int i = 0; i < cases; i++){
			a = in.nextInt();
			b = in.nextInt();

			if (a < b){
				System.out.println("<");
			} else if (b < a){
				System.out.println(">");
			} else {
				System.out.println("=");
			}
		}

	}
}
