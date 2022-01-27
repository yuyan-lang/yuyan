
import java.util.Random;

public class MatrixMultiplication {

    public static void main(String[] args) {

        int n = 2048;
        if (args.length > 0) {
            n = Integer.parseInt(args[0]);
        }

        double[][] A = new double[n][n];
        double[][] B = new double[n][n];
        double[][] C = new double[n][n];
        //populate the matrices with random values between 0.0 and 1.0
        Random r = new Random();
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                A[i][j] = r.nextDouble();
                B[i][j] = r.nextDouble();
                C[i][j] = 0;
            }
        }

        long start = System.nanoTime();
        //matrix multiplication
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                for (int k = 0; k < n; k++) {
                    C[i][j] += A[i][k] * B[k][j];
                }
            }
        }

        long stop = System.nanoTime();
        double timeDiff = (stop - start) * 1e-9;
        System.out.printf("%f",timeDiff);
    }
}