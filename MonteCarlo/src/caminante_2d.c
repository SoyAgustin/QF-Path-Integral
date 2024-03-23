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
double prob_origin(int n, int exp){
    int cont=0;

    for (int k=0;k<exp;k++){
        int x = 0;
        int y = 0;
        for (int i = 0; i < n; i++){
            int dx = dx1d(); 
            if(dx==1){
            x += dx1d();
            //printf("x+1 ");
            }
            else{
                y += dx1d();
                //printf("y+1 ");
            }
            //printf("i=%d (%d,%d)\n",i,x,y);
            if (x==0 && y==0){
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

    FILE *archivo = fopen("../../Notebooks_Py/Datos/caminante_2d.csv", "w"); 
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
    
    
    return 0;
}