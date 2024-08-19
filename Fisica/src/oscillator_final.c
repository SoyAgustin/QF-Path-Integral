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
            printf("[%f,",x[i]);
        }
        else if(i==SIZE-1){
            printf("%f]\n",x[i]);
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
            sum += 0.5*pow(x[i+1]-x[i]/a,2)+0.5*pow(x[i],2)+lambda*pow(x[i],4.0);
        } 
    }
    return a*sum;
}

float dSE(float xl, float x0, float xr, float xf, float a, float lambda){
    float sum=0.0;
    sum += pow(xr-xf,2)-pow(xr-x0,2);
    sum += pow(xf-xl,2)-pow(x0-xl,2);
    sum *= 1/(2*pow(a,2));
    sum += 0.5*(pow(xf,2)-pow(x0,2));
    sum += lambda*(pow(xf,4.0)-pow(x0,4.0));
    return sum;
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


float corr_d(float x[SIZE],int d){
    return x[0]*x[d];
}



#define REPETITIONS 11000//101000//510000
#define TERMALIZATION 1000//1000//10000
#define STEPS_TO_MEASURE 10
#define MEASURES ((REPETITIONS-TERMALIZATION)/STEPS_TO_MEASURE)
int main(){
    
    srand(time(NULL));

    float x[SIZE]={0.0};
    int start = 1; //Hot: 1, Cold: 0
    initialize_x(x,start);

    int steps_to_measure = 10;
    float epsilon;
    float a;
    float lambda = 1;
    float acc_rate[MEASURES];
    float a_vals[5]={1.0,0.5,0.25,0.1,0.05}; 
    float eps[11]={0.0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0};
    
    printf("a,epsilon,acc,err\n");
    for(int ind_a=0;ind_a<5;ind_a++){
        a = a_vals[ind_a];
        for(int ind_eps=0;ind_eps<11;ind_eps++){
            epsilon = eps[ind_eps];
            int j = 0;
            for (int i = 0; i < REPETITIONS; i++){
                float s = sweep(x,epsilon,a,lambda);
                if (i>=TERMALIZATION && i%steps_to_measure==0){
                    acc_rate[j] = s;
                    j++;
                }
            }
            float mean_acc_rate = mean(MEASURES,acc_rate);
            float error_acc_rate = error(MEASURES,acc_rate);
            printf("%f,%f,%f,%f\n",a,epsilon,mean_acc_rate,error_acc_rate);
        }
    }  

    //Information
    //printf("MEASURES: %d , epsilon: %0.2f , lambda: %0.2f, a: %0.2f \n",MEASURES,epsilon,lambda,a);
    

    return 0;
}