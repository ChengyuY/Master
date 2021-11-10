import java.util.ArrayList;

public class test_pc {

    public static void main(String[] args) {
        ArrayList<String> prod = new ArrayList<String>();
        prod.add("apple");
        prod.add("banana");
        Thread_principal th = new Thread_principal("main",5,2,2,5,prod,5);
        th.start();
    }

}
