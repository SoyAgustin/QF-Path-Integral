#include <stdio.h>

int main() {
    // Abre el archivo para escribir
    FILE *archivo = fopen("pares_numeros.csv", "w");
    
    if (archivo == NULL) {
        printf("Error al abrir el archivo.");
        return 1;
    }
    
    // Genera los pares de números y escribe en el archivo .csv
    for (int i = 1; i <= 10; i++) {
        // Genera los números en este ejemplo de forma simple
        int numero1 = i * 2;
        int numero2 = i * 3;
        
        // Escribe los números en el archivo .csv
        fprintf(archivo, "%d,%d\n", numero1, numero2);
    }
    
    // Cierra el archivo
    fclose(archivo);
    
    printf("Los pares de números se han guardado en el archivo 'pares_numeros.csv'.\n");
    
    return 0;
}
