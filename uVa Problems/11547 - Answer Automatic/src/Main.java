import java.io.*;
import java.math.*;
import java.util.*;
import java.lang.Math.*;

public class Main {

	public static void main(String [] args){
		Scanner in = new Scanner(System.in);

		int cases = in.nextInt();
		int n, ones, hundreds;

		for (int i = 0; i < cases; i++){
			n = in.nextInt();
			n = ((((((n * 567) / 9) + 7492) * 235) / 47) - 498);
			ones = n % 10;
			hundreds = (n / 100) * 100;
			System.out.println(Math.abs((n - ones - hundreds) / 10));
			
		}
	}
}
