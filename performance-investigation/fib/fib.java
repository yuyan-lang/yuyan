
public class fib {
    public static int fibonacci(int n) {
        if (n <= 1) {
            return n;
        }
        return fibonacci(n - 1) + fibonacci(n - 2);
    }

    public static void main(String[] args) {
        if (args.length != 1) {
            System.out.println("Usage: java Fibonacci <n>");
            System.exit(1);
        }

        int n = Integer.parseInt(args[0]);
        if (n < 0) {
            System.out.println("n must be a non-negative integer.");
            System.exit(1);
        }

        int result = fibonacci(n);
        System.out.println("The " + n + "th number in the Fibonacci sequence is: " + result);
    }
}
