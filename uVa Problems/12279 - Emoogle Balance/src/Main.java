import java.io.*;
import java.math.*;
import java.util.*;

public class Main {

	public static void main(String [] args){
		Scanner in = new Scanner(System.in);
		int casenum, casecount, treatcount, num;

		casenum = in.nextInt();
		casecount = 1;
		
		while(casenum != 0){
			treatcount = 0;
			for(int i = 0; i < casenum; i++){
				num = in.nextInt();
				if (num == 0){
					treatcount = treatcount - 1;
				} else {
					treatcount = treatcount + 1;
				}
			}
			System.out.printf("Case %d: %d\n", casecount, treatcount);
			casecount = casecount + 1; 
			casenum = in.nextInt();
			
		}
	}
}
