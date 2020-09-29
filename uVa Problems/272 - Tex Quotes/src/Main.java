import java.io.*;
import java.math.*;
import java.util.*;

public class Main {

	public static void main(String [] args){
		int check = 0;
		String line = "";
		Scanner in = new Scanner(System.in);

		while(in.hasNextLine()){
			line = in.nextLine();
			for(int i = 0; i < line.length(); i++){
				if (line.charAt(i) == '"'){
					if(check == 0){
						System.out.print("``");
					} else {
						System.out.print("''");
					}
					check = (check + 1) % 2;
				} else {
					System.out.print(line.charAt(i));
				}
			}
			System.out.print("\n");
		}
		
	}
}
