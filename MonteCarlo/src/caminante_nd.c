#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "../lib/my_math_stats.h"

int dxnd(int dim){
    float num = randnum_int(1,2*dim);
}

/*
dim - dimensión
x - vector de posición (array[dim])
n - número de pasos
rep - número de repeticiones (caminantes) */

float prob_origin(int dim, int x[],int n, int rep){
    int cont=0;
    int dir;//dirección

    for (int k=0;k<rep;k++){
        for (int i = 0; i<n; i++){
            dir = randnum_int(1,dim);
            x[dir] += dx1d();

            if (is_origin(dim,x)==1){
                cont++;
                break;
            }
        }
    }
    return (float)cont/rep;
}

int main(){
    srand(time(NULL));

    int dim=3;//dim y x[] misma dimension
    int x[3]={0};
    int p=5;//Potencia de 10 para el número de repeticiones (caminantes)

    int n;
    int rep=1; 
    float prob;

    FILE *archivo = fopen("../Notebooks_Py/Datos/caminante3d.csv", "w"); 
    fprintf(archivo,"rep,prob\n");

    printf("Número máximo de pasos por caminante:");
    scanf("%d", &n);printf("\n");
    
    for(int i=0;i<=p;i++){
        rep *= 10;
        prob = prob_origin(dim,x,n, rep);
        printf("Número de repeticiones (caminantes): %d, Probabilidad de regresar al origen: %f\n", rep, prob);
        fprintf(archivo, "%d,%f\n", rep, prob);
    }

    fclose(archivo);
    
    return 0;
}