import java.util.Scanner;

public class Runtime {

    private static Scanner scan = new Scanner(System.in);

    public static void printInt (int n) {
        System.out.println(n);
    }

    public static void printDouble (double x) {
        System.out.println(x);
    }

    // public static void printString (String s) {
    //     System.out.println(s);
    // }

    public static int readInt () {
        return scan.nextInt();
    }

    public static double readDouble () {
        return scan.nextDouble();
    }

}
