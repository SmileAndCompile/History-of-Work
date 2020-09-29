import java.io.*;
import java.math.*;
import java.util.*;

public class Main {

	public static void main(String [] args){
		Scanner in = new Scanner(System.in);

		int cases, casenum, min, max;

		cases = in.nextInt();
	
		for(int i = 0; i < cases; i++){
			casenum = in.nextInt();
			Integer[] locations = new Integer[casenum];

			for(int j = 0; j < casenum; j++){
				locations[j] = in.nextInt();
			}

			min = Collections.min(Arrays.asList(locations));
			max = Collections.max(Arrays.asList(locations));

			System.out.println((max - min) * 2);
		}
	}
}
