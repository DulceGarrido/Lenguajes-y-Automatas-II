import java.util.Scanner;
import java.math.MathContext;

public class CalcV2 {
    // INMUTABLE: sin estado global; más fácil de probar.
    private static final MathContext MC = MathContext.DECIMAL64;

    // Utilidad: memoria usada (KB)
    private static long memoriaUsadaKB() {
        Runtime rt = Runtime.getRuntime();
        rt.gc(); // limpiar basura previo a medir (opcional)
        long usadaBytes = rt.totalMemory() - rt.freeMemory();
        return usadaBytes / 1024;
    }

    static double powIter(double a, int b) {
        // Iterativa para evitar crecimiento de pila
        double res = 1.0;
        int n = Math.abs(b);
        double base = a;
        while (n > 0) {
            if ((n & 1) == 1) res *= base;
            base *= base;
            n >>= 1;
        }
        return b < 0 ? 1.0 / res : res;
    }

    static double operar(int op, double a, double b) {
        switch (op) {
            case 1: return a + b;
            case 2: return a - b;
            case 3: return a * b;
            case 4:
                if (b == 0.0) throw new ArithmeticException("División entre cero");
                return a / b;
            case 5: // potencia iterativa
                // Si quisieras alta precisión: BigDecimal
                return powIter(a, (int) Math.round(b));
            default:
                throw new IllegalArgumentException("Opción inválida");
        }
    }

    public static void main(String[] args) {
        try (Scanner sc = new Scanner(System.in)) { // Reusar un solo Scanner
            while (true) {
                System.out.println("\n=== CalcV2 (optimizada) ===");
                System.out.println("1) Suma  2) Resta  3) Mult  4) Div  5) Potencia  0) Salir");
                System.out.print("Opción: ");
                String sOp = sc.nextLine().trim();
                if (sOp.equals("0")) break;

                int op;
                try { op = Integer.parseInt(sOp); }
                catch (NumberFormatException e) { System.out.println("Opción inválida."); continue; }

                System.out.print("a: ");
                Double a = leerDouble(sc); // Validación centralizada
                if (a == null) continue;

                System.out.print("b: ");
                Double b = leerDouble(sc);
                if (b == null) continue;

                try {
                    // Medición de memoria ANTES
                    long beforeKB = memoriaUsadaKB();

                    double r = operar(op, a, b);

                    // Medición de memoria DESPUÉS
                    long afterKB = memoriaUsadaKB();
                    long deltaKB = afterKB - beforeKB;

                    System.out.println("Resultado: " + r);
                    System.out.println("Memoria usada -> antes: " + beforeKB + " KB | después: " + afterKB + " KB | Δ: " + deltaKB + " KB");
                } catch (ArithmeticException | IllegalArgumentException e) {
                    System.out.println("Error: " + e.getMessage());
                }
            }
        }
    }

    private static Double leerDouble(Scanner sc) {
        String s = sc.nextLine().trim();
        try { return Double.parseDouble(s); }
        catch (NumberFormatException e) {
            System.out.println("Número inválido.");
            return null;
        }
    }
}