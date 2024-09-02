#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "../lib/my_math_stats.h"


#define N 20
double epsilon = 0.55;
const double lambda  = 1.0;

#define SIZE 10
const double a = ((double)SIZE/N);


// iteraciones de Humberto 4000000
#define REPETITIONS 401000//101000//510000
#define TERMALIZATION 1000//1000//10000
#define STEPS_TO_MEASURE 10
#define MEASURES ((REPETITIONS-TERMALIZATION)/STEPS_TO_MEASURE)


void initialize_x(double x[N],int start){
    if (start==0){
        for (int i = 0; i < N; i++){
            x[i]=0.0;
    }
    }else{
        for (int i = 0; i < N; i++){
            double pos = randnum(-1.0,1.0); //reales entre -1 y 1
            x[i]=pos;
            } 
        } 
}

void print_x(double x[N]){
    
    for (int i = 0; i < N; i++){
        
        if(i==0){
            printf("%f,",x[i]);
        }
        else if(i==N-1){
            printf("%f\n",x[i]);
        }else{
        printf("%f,",x[i]);
        }
        
    }
    
    
}

double S_E(double x[N]){
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

double sweep(double x[N],double epsilon){
    double dse,xp = 0.0;
    double p,rand,rho = 0.0;
    double acc_rate = 0.0;
    int i = 0;

    for(i=0;i<N;i++){
        
        rho = randnum(-epsilon,epsilon); //numero aleatorio entre -epsilon y +epsilon
        xp = x[i]+rho; //Hacemos un cambio en la componente i 
        dse = dSE(x[(i-1)%N],x[i],x[(i+1)%N],xp); //calculamos el cambio de accion

        if(dse<=0.0){ //si es menor o igual que cero se realiza el cambio
            acc_rate++;
            x[i] = xp;
        }

        if(dse>0.0){ //Si es mayor que cero  se aplica algoritmo metropolis
            p = exp(-dse);
            rand = randnum(0.0,1.0);
            if(rand<p){ //Si rand es menor que p, se hace el cambio
                acc_rate++;
                x[i] = xp;
            }
        }
   
    }
    return acc_rate/(N);
}

void test_SE_dse(double x_0, double xp){
    double x0[N]={0.1,0.2,0.3,0.4,0.5,x_0,0.7,0.8,0.9,1.0};
    double x1[N]={0.1,0.2,0.3,0.4,0.5,xp,0.7,0.8,0.9,1.0};

    double s0  = S_E(x0);
    double s1  = S_E(x1);

    printf("s0: %f\n",s0);
    printf("s1: %f\n",s1);

    printf("s1-s0 completo: %f\n",s1-s0);
    printf("s1-s0 simplificado: %f\n",dSE(x0[4],x0[5],x0[6],x1[5]));
}

void SE_cold_hot(double epsilon){
    double x0[N] = {0.0};
    double x1[N] = {0.0};
    
    double SE_0[MEASURES]={0.0};
    double SE_1[MEASURES]={0.0};
    int j=0;

    initialize_x(x0,0);
    initialize_x(x1,1);
    
    printf("SE_cold,SE_hot\n");
    for (int i = 0; i < REPETITIONS; i++){
        sweep(x0,epsilon);
        sweep(x1,epsilon);
        if (i>=TERMALIZATION && i%STEPS_TO_MEASURE==0){
            printf("%f,%f\n",S_E(x0),S_E(x1));
            SE_0[j]=S_E(x0);
            SE_1[j]=S_E(x1);
            j++;   
        }
    }

    //printf("mean cold: %.6f, mean hot: %.6f\n",mean(MEASURES,SE_0),mean(MEASURES,SE_1));
    //printf("error cold: %.6f, error hot: %.6f\n",error(MEASURES,SE_0),error(MEASURES,SE_1));
    //printf("Measures: %d\n",MEASURES);
}

void acc_rate_core(double epsilon){
    double x[N] = {0.0};
    int start = 1;
    initialize_x(x,start);
    double acc_rates[MEASURES] = {0.0};
    double sweep_i = 0.0;
    double mean_acc_rates = 0.0;
    double se = 0.0;
    
    int index = 0;
    for (int i = 0; i < REPETITIONS; i++){
        sweep_i = sweep(x,epsilon);
        if (i>=TERMALIZATION && i%STEPS_TO_MEASURE==0){
              acc_rates[index]+=sweep_i; 
              index++;
        }
    }

    mean_acc_rates = mean(MEASURES,acc_rates);
    se = error(MEASURES,acc_rates);
    printf("%.1f,%.6f,%.6f\n",epsilon,mean_acc_rates,se);
}

void acc_rates(float step, int final){
    double epsilon = 0.0;
    printf("epsilon,mean,se\n");
    for(int i = 0; i<=final; i++){
        acc_rate_core(epsilon);
        epsilon+=step;
    }
}

void corr(){
    double x[N] = {0.0};
    int start = 1;
    initialize_x(x,start);
    double corr_measures[MEASURES][N+1];
    double corr_mean[2][N+1];

    int ind = 0;
    for(int i = 0; i < REPETITIONS; i++){
        sweep(x,epsilon);
        if (i>=TERMALIZATION && i%STEPS_TO_MEASURE==0){
            for(int j = 0; j<=N; j++){
                if(j==N){
                    corr_measures[ind][j] = corr_measures[ind][0];
                }
                else{
                    corr_measures[ind][j] = x[0]*x[j];
                }
            }
            ind++;
        }
    }

    for(int i = 0; i <= N; i++) {
        double temp[MEASURES];
        for (int j = 0; j < MEASURES; j++) {
            temp[j] = corr_measures[j][i];
        }
        corr_mean[0][i] = mean(MEASURES, temp);
        corr_mean[1][i] = error(MEASURES, temp);
    }

    for (int i = 0; i < 2; i++) {
        for (int j = 0; j <= N; j++) {
            printf("%f, ", corr_mean[i][j]);
        }
        printf("\n");
    }
    
}

int main(){
    srand(time(NULL));

    // double x[N];
    // int start = 1; //Hot: 1, Cold: 0
    // initialize_x(x,start);
    
    // double acc_rate = 0.0;
    // double sw=0.0;

    // double E_0[MEASURES];
    // int j=0;
    // for (int i = 0; i < REPETITIONS; i++){
    //     sw = sweep(x,epsilon);
    //     if (i>=TERMALIZATION && i%STEPS_TO_MEASURE==0){
    //          acc_rate+=sw;
    //          E_0[j]=pow(x[5],2)+3*lambda*pow(x[5],4); 
    //          j++;
    //     }
    // }

    // printf("acc_rate: %f\n",acc_rate/j);
    // printf("E_0: %f\nstderr: %f\n",mean(MEASURES,E_0),error(MEASURES,E_0));
    
    
    corr();
    //Information
    printf("MEASURES: %d , epsilon: %0.2f , lambda: %0.2f, a: %0.2f \n",MEASURES,epsilon,lambda,a);

    
    /*
    //Test de acción y dSE
    double x_0 = 0.6;
    double xp = 0.7; 
    test_SE_dse(x_0,xp);
    */
    

    
    //acc_rates
    /*
    float step = 0.1;
    int N = 20;
    acc_rates(step,N);
    */
    


    
    //SE_cold,SE_hot
    //SE_cold_hot(epsilon);
    

    return 0;
}