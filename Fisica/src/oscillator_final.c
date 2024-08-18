#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <string.h>
#include "../lib/my_math_stats.h"

#define SIZE 10

void initialize_x(float x[SIZE],int start){
    if (start==0){
        for (int i = 0; i < SIZE; i++){
            x[i]=0;
    }
    }else{
        for (int i = 0; i < SIZE-1; i++){
            float pos = randnum(-1,1); //reales entre -1 y 1
            x[i]=pos; 
        }
        x[SIZE-1]=x[0]; //Condiciones periodicas
    }
}

void print_x(float x[SIZE]){
    printf("\n");
    printf("[");
    for (int i = 0; i < SIZE; i++){
        printf("%f,",x[i]);
    }
    printf("]");
    printf("\n");
}

float S_E(float x[SIZE],float a,float lambda){
    float sum=0.0;

    for(int i =0; i<SIZE; i++){
        if(i==SIZE-1){
            sum+=0.5*pow(x[i],2)+lambda*pow(x[i],4.0);
        }
        sum += 0.5*pow(x[i+1]-x[i]/a,2)+0.5*pow(x[i],2)+lambda*pow(x[i],4.0); 
    }
    return a*sum;
}

float sweep(float x[SIZE],float epsilon, float a,float lambda){
    float SE_0,SE_s,dSE;
    float p,rand,rho;
    float acc_rate = 0.0;

    for(int i=0;i<SIZE;i++){
        if(i==SIZE-1){
            x[i]=x[0];// condicion peridica
        }
        else{
            rho = randnum(-epsilon,epsilon);
            SE_0 = S_E(x,a,lambda);
            x[i] = x[i]+rho;//Hacemos un cambio 
            SE_s = S_E(x,a,lambda);

            dSE = SE_s - SE_0;
            if(dSE<=0){
                acc_rate++;
            }
            if(dSE>0){ //Si es mayor que cero 
                p = exp(-1*dSE);
                rand = randnum(0,1);
                if(rand>p){ //Si es mayor que p, se rechaza el cambio
                    x[i] = x[i]-rho;
                }else{
                    acc_rate++;
                }
            }
        }  
   
    }
    return acc_rate/(SIZE-1);
}

float corr_d(float x[SIZE],int d){
    return x[0]*x[d];
}


#define REPETITIONS 110000//510000
#define TERMALIZATION 1000//10000
int main(){
    
    srand(time(NULL));

    float x[SIZE];
    int start = 1; //Hot: 1, Cold: 0
    initialize_x(x,start);
    int steps_to_measure = 10;
    int measures = (REPETITIONS-TERMALIZATION)/steps_to_measure;

    float epsilon = 0.7;
    float a = 1;
    float lambda = 0.0;

    float SE[measures];
    float SE_mean;
    float SE_error;

    float acc_rates[measures];
    float rate_i;
    float rate_mean;
    float rate_error;

    float x0_squared[measures];
    float x0_squared_mean;
    float x0_squared_error;

    float x5_squared[measures];
    float x5_squared_mean;
    float x5_squared_error;

    float corr[measures];
    float corr_mean;
    float corr_error;
    
    int ind_measures = 0;
    for (int i = 0; i < REPETITIONS; i++){
        rate_i = sweep(x,epsilon,a,lambda);
        if (i>=TERMALIZATION && i%steps_to_measure==0){
            SE[ind_measures] = S_E(x,a,lambda);
            acc_rates[ind_measures] = rate_i;
            x0_squared[ind_measures] = pow(x[0],2);
            x5_squared[ind_measures] = pow(x[5],2);
            ind_measures++;
        }
    }

    SE_mean = mean(measures,SE);
    SE_error = error(measures,SE);
    
    printf("mean SE: %f\n",SE_mean);
    printf("error SE: %f\n",SE_error);

    rate_mean = mean(measures,acc_rates);
    rate_error = error(measures,acc_rates);

    printf("mean rate: %f\n",rate_mean);
    printf("error rate: %f\n",rate_error);

    x0_squared_mean = mean(measures,x0_squared);
    x0_squared_error = error(measures,x0_squared);
    x5_squared_mean = mean(measures,x5_squared);
    x5_squared_error = error(measures,x5_squared);

    printf("mean x0_squared: %f\n",x0_squared_mean);
    printf("error x0_squared: %f\n",x0_squared_error);
    printf("mean x5_squared: %f\n",x5_squared_mean);
    printf("error x5_squared: %f\n",x5_squared_error);


      
    
    
    
    
    printf("measures: %d , epsilon: %0.2f , lambda: %0.2f, a: %0.2f \n",measures,epsilon,lambda,a);
    

    return 0;
}