import java.io.*;
import java.math.*;
import java.util.*;

public class Main {

	public static void main(String [] args){
		Scanner in = new Scanner(System.in);
		int start, first, second, third;

		start = in.nextInt();
		first = in.nextInt();
		second = in.nextInt();
		third = in.nextInt();

		while (! (start == 0 && first == 0 && second == 0 && third == 0)){
			System.out.println(720 + (((start - first + 40) % 40) * 9) + 360 + (((second - first + 40) % 40) * 9) + (((second - third + 40) % 40) * 9));
			start = in.nextInt();
			first = in.nextInt();
			second = in.nextInt();
			third = in.nextInt();
			
		}
	}
}
