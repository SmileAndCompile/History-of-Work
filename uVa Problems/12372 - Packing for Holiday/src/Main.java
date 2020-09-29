import java.io.*;
import java.math.*;
import java.util.*;

public class Main {

	public static void main(String [] args){
		Scanner in = new Scanner(System.in);

		int length, width, height, casenum, casecount = 1;

		casenum = in.nextInt();

		for(int i = 0; i < casenum; i++){
			length = in.nextInt();
			width = in.nextInt();
			height = in.nextInt();
			if (length > 20 || height > 20 || width > 20){
				System.out.printf("Case %d: bad\n", casecount);
			} else {
				System.out.printf("Case %d: good\n", casecount);
			}
			casecount = casecount + 1;
		}

	}
}
