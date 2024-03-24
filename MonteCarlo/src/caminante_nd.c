#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "../lib/my_math_stats.h"

int dx1d(){
    float num = randnum(0,1);
    if (num <= 0.5){
        return -1;
    }
    else{
        return 1;
    }
}

/*n - número de pasos
exp - número de caminantes o random walkers (experimentos) */
#define DIMENSION 3
double prob_origin(int n, int exp){
    int cont=0;

    for (int k=0;k<exp;k++){
        int vec_x[DIMENSION]={0};
        for (int i = 0; i < n; i++){

            int direction = randnum_int(0,DIMENSION-1); 
            vec_x[direction]+=dx1d();

            if (is_origin(DIMENSION,vec_x)){
                cont++;
                break;
            }
        }
        
    }
    return (double)cont/exp;
}


int main(){
    srand(time(NULL));

    int n_max=1;//maximo numero de pasos por experimento
    int pow=10;//
    int exp=15;//numero de caminantes o experimentos
    int rep = 1000;//numero de repeticiones por cada experimento
    double prob[1000];//arreglo de probabilidades para cada n_max de pasos

    double prob_mean;
    double err;

    clock_t start, end;
    double cpu_time_used;

    start = clock();

    char ruta[50];
    sprintf(ruta,"../../Notebooks_Py/Datos/caminante_%dd.csv",DIMENSION);
    FILE *archivo = fopen(ruta, "w"); 
    fprintf(archivo,"n_max,caminantes,experimentos,prob,error\n");
    

    for(int i=0;i<pow;i++){
        n_max*= 2;
        for(int k=0;k<rep;k++){
            prob[k]= prob_origin(n_max, exp);
            
        }
        prob_mean= mean(rep,prob);
        err = error(rep,prob);
        printf("n_max: %d, rw:%d ,exp: %d, prob_mean: %lf, error: %lf\n",n_max,exp,rep,prob_mean,err);
        fprintf(archivo, "%d,%d,%d,%lf,%lf\n", n_max,exp,rep,prob_mean,err);
    }

    fclose(archivo);
    
    end = clock();
    cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
    printf("Tiempo de ejecución en %dD: %.6lf segundos\n",DIMENSION, cpu_time_used);
    
    return 0;
}