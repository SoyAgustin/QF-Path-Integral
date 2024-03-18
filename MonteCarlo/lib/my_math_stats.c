#include <stdlib.h>
#include <math.h>

double randnum(float min, float max) {
    float num = (float)rand()/RAND_MAX;
    num = min+(max-min)*num;
    return num;
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

