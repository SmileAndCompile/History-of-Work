import java.io.*;
import java.math.*;
import java.util.*;

public class Main {

	public static void main(String [] args){
		
		int a, b, ans, cases;
		Scanner in = new Scanner(System.in);

		cases = in.nextInt();

		for(int i = 0; i < cases; i++){
			a = in.nextInt();
			b = in.nextInt();
			if (a < 3 || b < 3){
				ans = 0;
			} else {
				ans = (a / 3) * (b / 3);
			}
			System.out.println(ans);
		}
	}
}
