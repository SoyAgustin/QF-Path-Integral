#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

float randnum(float min, float max) {
    float num = (float)rand()/RAND_MAX;
    num = min+(max-min)*num;
    return num;
}

/*El caminante aleatorio puede avanzar solo en 2 direcciones, adelante o hacia atrás
para esto, con el generador de numeros aleatorios de entre 0 y 1 se puede dividir
el intervalo en 2, cuando se obtiene un número menor o igual al primer intervalo
el caminante avanza hacia atrás y en caso contrario avanza hacia la derecha*/

int dx1d(){
    int dim = 1;
    int posibilities = 2*dim;

    float num = randnum(0,1);
    if (num <= 0.5){
        return -1;
    }
    else{
        return 1;
    }
}

/*
n - número de pasos
rep - número de repeticiones (caminantes) */
float prob_origin(int n, int rep){
    int cont=0;
    int x = 0;
    for (int k=1;k<=rep;k++){
        for (int i = 1; i <= n; i++){
            x += dx1d();
            if (x==0){
                cont++;
                break;
            }
        }
        //cont++;
    }
    return (float)cont/rep;
}

int main(){
    srand(time(NULL));
    int n;
    int p;
    int rep=1;
    float prob;

    FILE *archivo = fopen("../Notebooks_Py/Datos/caminante1d.csv", "w"); 
    fprintf(archivo,"rep,prob\n");

    printf("Número de pasos por caminante:");
    scanf("%d", &n);printf("\n");
    p=5;
    /*
    printf("Potencia de 10 para el número de repeticiones (caminantes):");
    scanf("%d", &p);printf("\n");*/

    for(int i=0;i<=p;i++){
        rep *= 10;
        prob = prob_origin(n, rep);
        printf("Número de repeticiones (caminantes): %d, Probabilidad de regresar al origen: %f\n", rep, prob);
        fprintf(archivo, "%d,%f\n", rep, prob);
    }

    fclose(archivo);

    return 0;
}