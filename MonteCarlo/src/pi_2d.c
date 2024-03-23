#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "../lib/my_math_stats.h"

double aprox_2d(int n){
    double Ac=0.0; 
    double r=1.0;
    double x,y,d;

    for(int i=1;i<=n;i++){
        x = randnum(-r,r);
        y = randnum(-r,r);
        d = sqrt(x*x+y*y);
        if(d<=1){Ac++;} 
    }

    return (double)4.0*(Ac/n);
    
}

#define REPETITIONS 1000
int main() {
    srand(time(NULL));
    int n_max = 10000000;
    double pi[REPETITIONS]={0};
    
    FILE *archivo = fopen("../../Notebooks_Py/Datos/pi_2d.csv","w");
    fprintf(archivo,"n,rep,pi,error\n");

    for(int n=10;n<n_max;n*=10){
        for(int i=0;i<REPETITIONS;i++){
            pi[i]=aprox_2d(n);
        }
        double pi_mean=mean(REPETITIONS,pi);
        double pi_err = error(REPETITIONS,pi);

        printf("n: %d, rep: %d,pi: %lf, error: %lf\n",n,REPETITIONS,pi_mean,pi_err);
        fprintf(archivo,"%d,%d,%.15lf,%.15lf\n",n,REPETITIONS,pi_mean,pi_err);
    }
    fclose(archivo);
    return 0;
}


