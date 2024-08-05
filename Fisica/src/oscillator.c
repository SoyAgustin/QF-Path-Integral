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

float T(float x[SIZE],int i){
    if (i==SIZE-1){
        return 0;
    }else{
        return 0.5*pow((x[i+1]-x[i]),2.0);
    }
}

float V(float x[SIZE], int i,float lambda){
    return 0.5*pow(x[i],2)+lambda*pow(x[i],4.0);
}

float S_E(float x[SIZE],float a,float lambda){
    float sum=0.0;

    for(int i =0; i<SIZE; i++){
        sum+=T(x,i)+a*V(x,i,lambda);
    }
    return sum;
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

void init_simulation(int start, int max_sweeps,int thermalization, float epsilon, float a, float lambda){

    float x[SIZE]={0};
    char ruta_SE[500];
    char ruta_trayec[500];
    float SE;
    

//Rutas, archivos e inicialización hot y cold
    initialize_x(x,start);
    sprintf(ruta_SE,"../../Notebooks_Py/Datos/Oscillator/SE/start_%d_eps_%.2f_size_%d_a_%.2f.csv",start,epsilon,SIZE,a);
    sprintf(ruta_trayec,"../../Notebooks_Py/Datos/Oscillator/trayec/start_%d_eps_%.2f_size_%d_a_%.2f.csv",start,epsilon,SIZE,a);

//ruta y apertura de los archivos 
   
    FILE *archivo_SE = fopen(ruta_SE, "w");
    FILE *archivo_trayec = fopen(ruta_trayec, "w");

//Cabeceras de los archivos csv
    fprintf(archivo_SE,"sweep,S_E\n");
//cabecera del archivo de configuraciones "x"

    for(int i = 0;i<SIZE;i++){
        if(i==SIZE-1){
            fprintf(archivo_trayec,"%d\n",i);
        }else{
            fprintf(archivo_trayec,"%d,",i);
        }
    }
    
//Estado inicial
    SE = S_E(x,a,lambda);
    fprintf(archivo_SE, "%d,%f\n",0,SE);

//Sweeps
    for(int i =1;i<max_sweeps;i++){
       
        if(i>thermalization && i%10==0){
            for(int j=0;j<SIZE;j++){
                if(j==SIZE-1){
                    fprintf(archivo_trayec,"%f\n",x[j]);
                }else{
                    fprintf(archivo_trayec,"%f,",x[j]);
                }
            }
        }

        sweep(x,epsilon,a,lambda);
        SE = S_E(x,a,lambda);
        fprintf(archivo_SE, "%d,%f\n", i,SE);
    }

    fclose(archivo_SE);
    fclose(archivo_trayec);

    printf("\nHecho!\n");
}

int main(){
    
    
    //Tests
    

    //Simulation
    
    srand(time(NULL));
    int start = 0;
    int max_sweeps = 11000;
    int thermalization = 1000;
    float epsilon = 0.7;
    float a = 1.0;
    float lambda = 0.0;
    init_simulation(start,max_sweeps,thermalization,epsilon,a,lambda);
    

    //Epsilon rates
    /*
    srand(time(NULL));
    float init_epsilon = 0.0;
    float final_epsilon = 3.0;
    float step = 0.5;
    float a = 1.0;
    float lambda = 0.0;
    //epsilon_rates(init_epsilon,final_epsilon,step,a,lambda);
    */
    return 0;
}