package automata;

import java.util.*;

/* TODO: Implement a grep-like tool. */

public class MyApplication {

    // Your own regex search function here:
    // Takes a regex and a text as input
    // Returns true if the regex matches any substring of the text; otherwise returns false
    public static boolean mySearch(String regex, String text) {
        return false;  // should be something that correctly implements it
    }

    public static void main(String[] args) throws Exception {

        // Example of using the regexp parser:
        REParser.parse("(aba)*c+a|cd").accept(new PrettyPrinter());
        System.out.println();

        System.exit(0);

    }

}
