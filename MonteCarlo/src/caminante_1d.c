#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "../lib/my_math_stats.h"

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

/*n - número de pasos
cam - número de caminantes o random walkers (experimentos) */
double prob_origin(int n, int cam){
    int cont=0;
    int x = 0;
    for (int k=0;k<cam;k++){
        for (int i = 0; i < n; i++){
            x += dx1d();
            if (x==0){
                cont++;
                break;
            }
        }
    }
    return (double)cont/cam;
}

int main(){
    srand(time(NULL));
    
    int n_max=10;//maximo numero de pasos por experimento
    int pow=10;//n_max_final = n_max^pow
    int cam=15;//numero de caminantes o experimentos
    int rep = 1000;//numero de repeticiones por cada experimento
    double prob[1000];//arreglo de probabilidades para cada n_max de pasos

    double prob_mean;
    double err;

    FILE *archivo = fopen("../../Notebooks_Py/Datos/caminante_1d.csv", "w"); 
    fprintf(archivo,"n_max,caminantes,experimentos,prob,error\n");
    

    for(int i=0;i<pow;i++){
        n_max*= 2;
        for(int k=0;k<rep;k++){
            prob[k]= prob_origin(n_max, cam);
            
        }
        prob_mean= mean(rep,prob);
        err = error(rep,prob);
        printf("n_max: %d, rw:%d ,exp: %d, prob_mean: %lf, error: %lf\n",n_max,cam,rep,prob_mean,err);
        fprintf(archivo, "%d,%d,%d,%lf,%lf\n", n_max,cam,rep,prob_mean,err);
    }

    fclose(archivo);
    
    return 0;
}