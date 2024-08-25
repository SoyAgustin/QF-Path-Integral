#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "../lib/my_math_stats.h"

#define SIZE 10
#define a (10.0/SIZE)
#define lambda 0.0
double epsilon = 0.3;

// iteraciones de Humberto 4000000
#define REPETITIONS 101000//101000//510000
#define TERMALIZATION 1000//1000//10000
#define STEPS_TO_MEASURE 10
#define MEASURES ((REPETITIONS-TERMALIZATION)/STEPS_TO_MEASURE)


void initialize_x(double x[SIZE],int start){
    if (start==0){
        for (int i = 0; i < SIZE; i++){
            x[i]=0.0;
    }
    }else{
        for (int i = 0; i < SIZE; i++){
            double pos = randnum(-1.0,1.0); //reales entre -1 y 1
            x[i]=pos;
            } 
        } 
}

void print_x(double x[SIZE]){
    
    for (int i = 0; i < SIZE; i++){
        
        if(i==0){
            printf("%f,",x[i]);
        }
        else if(i==SIZE-1){
            printf("%f\n",x[i]);
        }else{
        printf("%f,",x[i]);
        }
        
    }
    
    
}

double S_E(double x[SIZE]){
    double sum=0.0;

    for(int i =0; i<SIZE; i++){
        if(i==SIZE-1){
            sum+=0.5 * pow((x[0] - x[i]) / a, 2)+0.5*pow(x[i],2)+lambda*pow(x[i],4.0); //Condición periodica x[N+1] = x[0]
        }else{
            sum += 0.5 * pow((x[i+1] - x[i]) / a, 2) + 0.5 * pow(x[i], 2) + lambda * pow(x[i], 4.0);
        } 
    }
    return a*sum;
}

double dSE(double xl, double x0, double xr, double xf){
    double sum=0.0;
    sum += (pow(xr - xf, 2) - pow(xr - x0, 2)) /(2*a*a);
    sum += (pow(xf - xl, 2) - pow(x0 - xl, 2)) /(2*a*a);
    sum += 0.5*(pow(xf,2)-pow(x0,2));
    sum += lambda*(pow(xf,4.0)-pow(x0,4.0));
    return a*sum;
}

double sweep(double x[SIZE],double epsilon){
    double dse,xp = 0.0;
    double p,rand,rho = 0.0;
    double acc_rate = 0.0;

    for(int i=0;i<SIZE;i++){
        
        rho = randnum(-epsilon,epsilon);
        
        xp = x[i]+rho; //Hacemos un cambio
        
        dse = dSE(x[(i-1)%SIZE],x[i],x[(i+1)%SIZE],xp); //calculamos el cambio de accion

        if(dse<=0){ //si es menor o igual que cero se realiza el cambio
            acc_rate++;
            x[i] = xp;
        }

        if(dse>0){ //Si es mayor que cero metropolis
            p = exp(-dse);
            rand = randnum(0.0,1.0);
            if(rand>p){ //Si rand es mayor que p, se rechaza el cambio
                //no se hace nada con el array
            }else{ //si es menor o igual que p se realiza el cambio
                acc_rate++;
                x[i] = xp;
            }
        }
   
    }
    return acc_rate/(SIZE);
}

void test_SE_dse(double x_0, double xp){
    double x0[SIZE]={5,1,2,4,5,x_0,7,8,9,2};
    double x1[SIZE]={5,1,2,4,5,xp,7,8,9,2};
    
    //double lambda = 1;
    

    double s0  = S_E(x0);
    double s1  = S_E(x1);

    printf("s0: %f\n",s0);
    printf("s1: %f\n",s1);

    printf("s1-s0: %f\n",s1-s0);
    printf("s1-s0: %f\n",dSE(x0[4],x0[5],x0[6],x1[5]));
}

void SE_cold_hot(double epsilon){
    double x0[SIZE] = {0.0};
    double x1[SIZE] = {0.0};
    //int start = 1; //Hot: 1, Cold: 0
    initialize_x(x0,0);
    initialize_x(x1,1);
    
    printf("SE_cold,SE_hot\n");
    for (int i = 0; i < REPETITIONS; i++){
        sweep(x0,epsilon);
        sweep(x1,epsilon);
        if (i>=TERMALIZATION && i%STEPS_TO_MEASURE==0){
            printf("%f,%f\n",S_E(x0),S_E(x1));   
        }
    }
}

void acc_rate_core(double epsilon){
    double x[SIZE] = {0.0};
    int start = 1;
    initialize_x(x,start);
    double acc_rates[MEASURES] = {0.0};
    double sweep_i = 0.0;
    double mean_se[2] = {0.0};
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

void acc_rates(float step, int N){
    double epsilon = 0.0;
    printf("epsilon,mean,se\n");
    for(int i = 0; i<=N; i++){
        acc_rate_core(epsilon);
        epsilon+=step;
    }
}

int main(){
    srand(time(NULL));

/*
    double x[SIZE];
    int start = 1; //Hot: 1, Cold: 0
    initialize_x(x,start);
    
    double acc_rate = 0.0;
    double sw;
    double x0_2=0;
    double x5_2=0;
    double x9_2=0;
    double SE[MEASURES] = {0};
    int j=0;
    for (int i = 0; i < REPETITIONS; i++){
        sw = sweep(x,epsilon);
        if (i>=TERMALIZATION && i%STEPS_TO_MEASURE==0){
             acc_rate+=sw;
             SE[j]=S_E(x);
             x0_2+=x[0]*x[0];
             x5_2+=x[5]*x[5];
             x9_2+=x[9]*x[9];
             j++;
        }
    }

    printf("acc_rate: %f\n",acc_rate/MEASURES);
    printf("<SE>: %f, err: %f\n",mean(MEASURES,SE),error(MEASURES,SE));
    printf("x0: %f\n",x0_2/(MEASURES));
    printf("x5: %f\n",x5_2/MEASURES);
    printf("x9: %f\n",x9_2/MEASURES);
    */
    

    //Information
    //printf("MEASURES: %d , epsilon: %0.2f , lambda: %0.2f, a: %0.2f \n",MEASURES,epsilon,lambda,a);

    /*
    //Test de acción y dSE
    srand(time(NULL));
    double x_0 = 5.0;
    double xp = 6.0; 
    test_SE_dse(x_0,xp);
    */

    /*
    //acc_rates
    float step = 0.1;
    int N = 20;
    acc_rates(step,N);
    */


    /*
    //SE_cold,SE_hot
    double epsilon = 0.7;
    double lambda = 0;
    SE_cold_hot(epsilon,lambda);
    */

    return 0;
}