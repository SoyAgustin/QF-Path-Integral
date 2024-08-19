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
        for (int i = 0; i < SIZE; i++){
            if(i==SIZE-1){
                x[SIZE-1]=x[0]; //Condiciones periodicas
            }else{
                float pos = randnum(-1,1); //reales entre -1 y 1
                x[i]=pos;
            } 
        }
    }
}

void print_x(float x[SIZE]){
    
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


float dSE(float xl, float x0, float xr, float xf, float a, float lambda){
    float sum=0.0;
    sum += (pow(xr - xf, 2) - pow(xr - x0, 2)) /(2*a*a);
    sum += (pow(xf - xl, 2) - pow(x0 - xl, 2)) /(2*a*a);
    sum += 0.5*(pow(xf,2)-pow(x0,2));
    sum += lambda*(pow(xf,4.0)-pow(x0,4.0));
    return a*sum;
}




float sweep(float x[SIZE],float epsilon, float a,float lambda){
    float xl,x0,xr,xf=0;
    float rho = 0.0;
    int acc_rate = 0;

    for(int i=0;i<SIZE;i++){
        if(i==SIZE-1){
            xr = x[0];
            x0 = x[i];
            xl = x[i-1];
        }
        else if(i==0){
            xr = x[i+1];
            x0 = x[i];
            xl = x[SIZE-1];
        }
        else{
            xr = x[i+1];
            x0 = x[i];
            xl = x[i-1];
        }
        
        rho = randnum(-epsilon,epsilon);
        xf = x0+rho; //Hacemos un cambio en x0 

        float dS = dSE(xl,x0,xr,xf,a,lambda); // Se calcula el cambio de acción
        
        if(dS<=0){
           x[i] = xf;  //Se acepta el cambio
           acc_rate++;
        }
        else{
            float p = exp(-dS); //probabilidad de rechazo
            float rand = randnum(0,1); 
            if(rand<=p){ 
                x[i] = xf; //Se acepta el cambio
                acc_rate++;
            }
        }
    }

    return (float)acc_rate/(SIZE);
}


#define REPETITIONS 101000//101000//510000
#define TERMALIZATION 1000//1000//10000
#define STEPS_TO_MEASURE 10
#define MEASURES ((REPETITIONS-TERMALIZATION)/STEPS_TO_MEASURE)
int main(){
/*
    srand(time(NULL));
    float x[SIZE];
    int start = 1; //Hot: 1, Cold: 0
    
    initialize_x(x,start);
    int steps_to_measure = 10;

    float epsilon = 0.7;
    float a=0.05;
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

    //SE 
/*
    float x0[SIZE];
    float x1[SIZE];
    //int start = 1; //Hot: 1, Cold: 0
    initialize_x(x0,0);
    initialize_x(x1,1);
    int steps_to_measure = 10;

    float epsilon = 0.2;
    float a=0.05;
    float lambda = 1;
    
    
    printf("SE_cold,SE_hot\n");
    for (int i = 0; i < REPETITIONS; i++){
        sweep(x0,epsilon,a,lambda);
        sweep(x1,epsilon,a,lambda);
        if (i>=TERMALIZATION && i%steps_to_measure==0){
            printf("%f,%f\n",S_E(x0,a,lambda),S_E(x1,a,lambda));   
        }
    }
*/

    //Information
   // printf("MEASURES: %d , epsilon: %0.2f , lambda: %0.2f, a: %0.2f \n",MEASURES,epsilon,lambda,a);

/*
     //CONTROL PASS
    srand(time(NULL));
    float x0[SIZE]={5,1,2,4,5,6,7,8,9,2};
    float x1[SIZE]={5,1,2,4,5,8,7,8,9,2};
    
    float a = 0.05;
    float lambda = 1;

    float s0  = S_E(x0,a,lambda);
    float s1  = S_E(x1,a,lambda);

    printf("s0: %f\n",s0);
    printf("s1: %f\n",s1);

    printf("s1-s0: %f\n",s1-s0);
    printf("s0-s1: %f\n",dSE(x0[4],x0[5],x0[6],x1[5],a,lambda));
*/

    return 0;
}