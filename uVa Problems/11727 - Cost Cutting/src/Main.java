import java.io.*;
import java.math.*;
import java.util.*;
import java.lang.Math.*;

public class Main {

	public static void main(String [] args){
		Scanner in = new Scanner(System.in);
		
		int cases, a, b, c, min, max;
		cases = in.nextInt();

		for(int i = 0; i < cases; i++){
			int[] salaries = new int[3];

			salaries[0] = in.nextInt();
			salaries[1] = in.nextInt();
			salaries[2] = in.nextInt();

			Arrays.sort(salaries);

			System.out.printf("Case %d: %d\n", (i+1), salaries[1]);
		}
	}
}
