#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

float randnum(float min, float max) {
    float num = (float)rand()/RAND_MAX;
    num = min+(max-min)*num;
    return num;
}

int randnum_int(int min,int max){
    int num = roundf(randnum(min,max));
    return num;
}

int dx1d(){
    float num = randnum(0,1);
    if (num <= 0.5){
        return -1;
    }
    else{
        return 1;
    }
}

int is_origin(int dim,int array[]){
    for(int i=0;i<dim;i++){
        if(array[i]!=0){
            return 0;
        }
    }
    return 1;
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

double mean(int N,double x[]){
    double sum=0.0;
    for(int i=0;i<N;i++){
        sum+=x[i];
    }
    return (double) sum/N;
}

double var(int N, double x[]){
    double mean_x=mean(N,x);
    double sum_x2=0.0;
    for(int i=0;i<N;i++){
        sum_x2+=x[i]*x[i];
    } 
    return (double) (sum_x2-N*(mean_x*mean_x))/(N-1);
}

double error(int N, double x[]){
    return sqrt((double) var(N,x)/N);
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