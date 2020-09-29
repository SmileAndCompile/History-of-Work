import java.io.*;
import java.math.*;
import java.util.*;

public class Main {

	public static void main(String [] args){
		Scanner in = new Scanner(System.in);
		int queries, divx, divy, x, y;

		queries = in.nextInt();

		while(queries != 0){
			divx = in.nextInt();
			divy = in.nextInt();

			for(int i = 0; i < queries; i++){
				x = in.nextInt();
				y = in.nextInt();

				if(x == divx || y == divy){
					System.out.println("divisa");
				} else if (x < divx && y < divy){
					System.out.println("SO");
				} else if (x < divx && y > divy){
					System.out.println("NO");
				} else if (x > divx && y < divy){
					System.out.println("SE");
				} else {
					System.out.println("NE");
				}
			}

			queries = in.nextInt();
		}
	}
}
