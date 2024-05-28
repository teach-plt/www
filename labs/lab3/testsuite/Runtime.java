import java.lang.String;
import java.util.Locale;
import java.util.Scanner;

public class Runtime {

    private static Scanner scan = (new Scanner(System.in)).useLocale(Locale.ROOT);

    public static void printInt (int n) {
        System.out.println(n);
    }

    // Use the ROOT locale to force a decimal dot rather than the country-specific decimal separator.
    public static void printDouble (double x) {
        System.out.println(String.format(Locale.ROOT, "%f", x));
    }

    public static int readInt () {
        return scan.nextInt();
    }

    public static double readDouble () {
        return scan.nextDouble();
    }

}
