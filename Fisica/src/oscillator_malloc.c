#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "../lib/my_math_stats.h"


int N=10;
double epsilon=0.75;
double lambda=0.0;
const int L=10;
double a=0.0; //Se modifica el valor en cada función que hace sweep

int sweeps = 401000;
int  termalization = 1000;
int steps = 10;


// Función para generar un arreglo de tamaño N
double* initialize_array(int N, int start) {
    double* x = (double*) malloc(N * sizeof(double));

    if (x == NULL) {
        printf("Error al asignar memoria.\n");
        exit(1);
    }

    if (start == 0) {
        for (int i = 0; i < N; i++) {
            x[i] = 0.0;
        }
    } else if (start == 1) {
        for (int i = 0; i < N; i++) {
            x[i] = randnum(-1.0, 1.0);
        }
    }

    return x;
    
}

void print_x(double* x,int N){
    for (int i = 0; i < N; i++){
        if(i==N-1){
            printf("%f",x[i]);
        }else{
            printf("%f,",x[i]);
        }
    }
    printf("\n");
}

double S_E(double* x,int N){
    double sum=0.0;

    for(int i =0; i<N; i++){
        if(i==N-1){
            sum += 0.5*pow((x[0] - x[i])/a, 2) + 0.5*pow(x[i],2) + lambda*pow(x[i],4.0); //Condición periodica x[N+1] = x[0]
        }else{
            sum += 0.5*pow((x[i+1] - x[i])/a, 2) + 0.5 * pow(x[i], 2) + lambda*pow(x[i], 4.0);
        } 
    }
    return a*sum;
}

double dSE(double xl, double x0, double xr, double xp){
    double sum=0.0;
    sum += (pow(xr - xp, 2) - pow(xr - x0, 2)) /(2*a*a);
    sum += (pow(xp - xl, 2) - pow(x0 - xl, 2)) /(2*a*a);
    sum += 0.5*(pow(xp,2)-pow(x0,2));
    sum += lambda*(pow(xp,4.0)-pow(x0,4.0));
    return a*sum;
}


double sweep(double* x,int N, double epsilon){
    double xp,dse= 0.0;
    double acc = 0.0;  

    for(int i=0;i<N;i++){

        xp = x[i]+randnum(-epsilon,epsilon); //numero aleatorio entre -epsilon y +epsilon

        dse = dSE(x[(i-1)%N],x[i],x[(i+1)%N],xp); //calculamos el cambio de accion

        if(dse<=0.0){ //si es menor o igual que cero se realiza el cambio
            x[i] = xp;
            acc++;
        }else{
            //Aplicar Metropolis
            double p = exp(-dse);
            double rand = randnum(0.0,1.0);
            if(rand<p){ //Si rand es menor que p, se hace el cambio
                x[i] = xp;
                acc++;
            }
        }

    }
    return (double) acc/N;
}

double* test_x1(double* x,int N){
    double mean_x = mean(N,x);
    double std_x = error(N,x);

    printf("mean_x: %f, std_x: %f\n",mean_x,std_x);
}

int main() {
    srand(time(NULL));
    N=100;
    epsilon = 0.75;
    lambda = 0.0;

    sweeps = 101000;
    termalization = 1000;
    steps = 10;
    int measures = (int)((sweeps-termalization)/steps);

    //Inicializamos el vector x
    double* x = initialize_array(N, 1);

    a = (double)L/N;
    double sw,acc = 0.0;
    double E0 = 0.0;
    
    int j=0;
    for(int i=0;i<sweeps;i++){
        sw = sweep(x,N,epsilon);
        if(i >= termalization && i%steps==0){
            acc+=sw;
            E0 += pow(x[0],2) + 3*lambda*pow(x[0],4.0);

            j++;
        }
    }

    acc /= measures; 
    E0 /= measures;
   
    
    printf("acc: %f, a: %f\n",acc,a);
    printf("E0: %f\n",E0);
    

    

    free(x);

    return 0;
}
