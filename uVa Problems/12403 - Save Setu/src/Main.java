import java.io.*;
import java.math.*;
import java.util.*;

public class Main {

	public static void main(String [] args){
		Scanner in = new Scanner(System.in);

		int casenum, total = 0;
		casenum = in.nextInt();
		in.nextLine();

		for(int i = 0; i < casenum; i++){
			String input = in.nextLine();
			if (input.equals("report")){
				System.out.println(total);
			} else {
				total = total + Integer.parseInt(input.split("\\s+")[1]);
			}
		}
	}
}
