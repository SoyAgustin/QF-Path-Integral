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
cam - número de cameticiones (caminantes) */
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

double mean(int N,double x[]){
    double sum=0.0;
    for(int i=0;i<N;i++){
        sum+=x[i];
    }
    return (double) sum/N;
}

double var(int N, double x[]){
    double mean_x=mean(N,x);
    double sum_x2=0;
    for(int i=0;i<N;i++){
        sum_x2+=x[i]*x[i];
    } 
    return  (double)(sum_x2-N*(mean_x*mean_x))/(N-1);
}

double error(int N, double x[]){
    return sqrt((double) var(N,x)/N);
}

int main(){
    double valores[100];
    int row=0;
    double promedio;
    double varianza;
    double err;

    FILE *file = fopen("/home/agustin/Escritorio/Servicio/Notebooks_Py/Datos/randomnumbers.csv", "r");

    char line[1024];
    while (fgets(line, 1024, file)) {
        valores[row++] = atof(line);
    }

    fclose(file);

    promedio=mean(100,valores);
    varianza =var(100,valores);
    err = error(100,valores);
    printf("mean: %f\n var: %f\n err: %f\n",promedio,varianza,err);
    return 0;
}