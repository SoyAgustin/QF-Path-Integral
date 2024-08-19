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

float T(float x[SIZE],int i,float a){
    if (i==SIZE-1){
        return 0;
    }else{
        return pow((x[i+1]-x[i])/(2*a),2.0);
    }
}

float V(float x[SIZE], int i,float lambda){
    return 0.5*pow(x[i],2)+lambda*pow(x[i],4.0);
}

float S_E(float x[SIZE],float a,float lambda){
    float sum=0.0;
    
   for(int i =0; i<SIZE; i++){
        if(i==SIZE-1){
            sum+=0.5*pow(x[i],2)+lambda*pow(x[i],4.0);
        }else{
            sum += 0.5 * pow((x[i+1] - x[i]) / a, 2) + 0.5 * pow(x[i], 2) + lambda * pow(x[i], 4.0);

        } 
    }
    return a*sum;
    
}

float SE(float x[SIZE],float a,float lambda){
    float sum=0;
    for(int i =0; i<SIZE; i++){
        sum+=T(x,i,a)+V(x,i,lambda);
    }
    return sum*a;
}

float dSE(float xl, float x0, float xr, float xf, float a, float lambda){
    float sum=0.0;
    sum += (pow(xr - xf, 2) - pow(xr - x0, 2)) / (2 * pow(a, 2));
    sum += (pow(xf - xl, 2) - pow(x0 - xl, 2)) / (2 * pow(a, 2));
    sum += 0.5*(pow(xf,2)-pow(x0,2));
    sum += lambda*(pow(xf,4.0)-pow(x0,4.0));
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


#define REPETITIONS 510000//101000//510000
#define TERMALIZATION 10000//1000//10000
#define STEPS_TO_MEASURE 10
#define MEASURES ((REPETITIONS-TERMALIZATION)/STEPS_TO_MEASURE)
int main(){
    //Tests
/*
    srand(time(NULL));
    float x[SIZE];
    //int start = 1; //Hot: 1, Cold: 0
    
    initialize_x(x,1);
    int steps_to_measure = 10;

    float epsilon = 0.7;
    float a=1;
    float lambda = 0.0;
    
    
    float acc_rate = 0.0;
    float sw;
    float x0=0;
    float x5=0;
    float x9=0;
    float SE=0;
    for (int i = 0; i < REPETITIONS; i++){
        sw = sweep(x,epsilon,a,lambda);
        if (i>=TERMALIZATION && i%steps_to_measure==0){
             acc_rate+=sw;
             SE+=S_E(x,a,lambda);
             x0+=x[0]*x[0];
             x5+=x[5]*x[5];
             x9+=x[9]*x[9];
        }
    }

    printf("acc_rate: %f\n",acc_rate/MEASURES);
    printf("SE: %f\n",SE/MEASURES);
    printf("x0: %f\n",x0/(MEASURES));
    printf("x5: %f\n",x5/MEASURES);
    printf("x9: %f\n",x9/MEASURES);
*/


    
    srand(time(NULL));
    float x0[SIZE]={0,1,2,3,4,5,6,7,8,9};
    float x1[SIZE]={0,1,2,3,4,5,6,7,8,9};
    //initialize_x(x0,1);
    //initialize_x(x1,1);
   
    //params
    int max_sweeps = 510000;
    int thermalization = 10000;
    int start = 1;
    float epsilon=0.7;
    float a =1;
    float lambda = 0;

    printf("SE: %f\n",S_E(x0,a,lambda));
    printf("SE: %f\n",SE(x1,a,lambda));
    
    return 0;
}