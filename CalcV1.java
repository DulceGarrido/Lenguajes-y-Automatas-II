import java.util.Scanner;

public class CalcV1 {
    // INEFICIENTE: variable global mutable (acopla y dificulta pruebas)
    static String ultimaOperacion = "";

    // INEFICIENTE: recursivo para potencia (crece la pila innecesariamente)
    static double powRec(double a, int b) {
        if (b == 0) return 1;
        return a * powRec(a, b - 1);
    }

    // Método para medir la memoria usada
    public static void medirMemoria(String mensaje) {
        Runtime runtime = Runtime.getRuntime();
        runtime.gc(); // limpia la basura antes de medir (opcional pero recomendable)
        long memoriaTotal = runtime.totalMemory();
        long memoriaLibre = runtime.freeMemory();
        long memoriaUsada = memoriaTotal - memoriaLibre;
        System.out.println(mensaje + " -> Memoria usada: " + memoriaUsada / 1024 + " KB");
    }

    public static void main(String[] args) {
        boolean seguir = true;
        while (seguir) {
            // INEFICIENTE: crear Scanner en cada iteración
            Scanner sc = new Scanner(System.in);

            System.out.println("\n=== CalcV1 (mejorable) ===");
            System.out.println("1) Suma  2) Resta  3) Mult  4) Div  5) Potencia  0) Salir");
            System.out.print("Opción: ");
            int op = Integer.parseInt(sc.nextLine()); // INEFICIENTE: parseos repetidos

            if (op == 0) {
                seguir = false;
                continue;
            }

            System.out.print("a: ");
            double a = Double.parseDouble(sc.nextLine());
            System.out.print("b: ");
            double b = Double.parseDouble(sc.nextLine());

            // Medir memoria ANTES de ejecutar la operación
            medirMemoria("Antes de la operación");

            double r = 0.0;
            if (op == 1) { r = a + b; ultimaOperacion = "suma"; }
            else if (op == 2) { r = a - b; ultimaOperacion = "resta"; }
            else if (op == 3) { r = a * b; ultimaOperacion = "mult"; }
            else if (op == 4) { // INEFICIENTE: sin manejo formal de /0
                r = a / b;
                ultimaOperacion = "div";
            }
            else if (op == 5) {
                // INEFICIENTE: castear b a int sin validar + recursión
                r = powRec(a, (int)b);
                ultimaOperacion = "powRec";
            } else {
                System.out.println("Opción inválida.");
                continue;
            }

            // Medir memoria DESPUÉS de ejecutar la operación
            medirMemoria("Después de la operación");

            // INEFICIENTE: concatena strings en loop (aquí es mínimo, pero ejemplo)
            System.out.println("Resultado: " + r + " | Última op: " + ultimaOperacion);
            // sc.close();  // INEFICIENTE/PELIGRO: cerrar System.in repetidamente puede romper lecturas
        }
    }
}
