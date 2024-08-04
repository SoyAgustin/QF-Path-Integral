#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <string.h>
#include "../lib/my_math_stats.h"

#define SIZE 10
#define a 0.1

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
        return 0.5*pow((x[i+1]-x[i])/a,2);
    }
}

float V(float x[SIZE], int i,float lambda){
    return 0.5*pow(x[i],2)+lambda*pow(x[i],4);
}

float S_E(float x[SIZE]){
    float sum=0.0;
    float lambda = 0.0;
    /*for(int i=0;i<SIZE-1;i++){
        sum+=pow(x[i+1]-x[i],2.0)+pow(x[i]*x[i],2.0);
    }
    sum+=pow(x[0],2.0);
    */

   for(int i =0; i<SIZE; i++){
    sum+=T(x,i)+V(x,i,lambda);
   }
    return a*sum;
}

float acc_rate = 0.0;
float rej_rate = 0.0;
float total_prob = 0.0;
void sweep(float x[SIZE],float epsilon){//Metropolis 
    float SE_0,SE_s,dSE;
    float p,rand,rho;

    for(int i=0;i<SIZE;i++){
        if(i==SIZE-1){
            x[i]=x[0];// condicion peridica
        }else{
        rho = randnum(-epsilon,epsilon);
        SE_0 = S_E(x);
        x[i] = x[i]+rho;//Hacemos un cambio 
        SE_s = S_E(x);

        dSE = SE_s - SE_0;
        if(dSE>0){ //Si es mayor que cero 
            p = exp(-1*dSE);
            rand = randnum(0,1);
            if(rand>p){ //Si es mayor que p, no hacemos el cambio
                x[i] = x[i]-rho;
                rej_rate++;
            }else{
                acc_rate++;
            }
        }else{
            acc_rate++;
        }
    }
    }
    total_prob = acc_rate+rej_rate;
}

void init_simulation(int start, int max_sweeps,float epsilon){
    
    float x[SIZE]={0};
    char ruta[500], ruta2[500];
    float SE;

//Rutas, archivos e inicialización hot y cold
    initialize_x(x,start);
    sprintf(ruta,"../../Notebooks_Py/Datos/Harmonic/start_%d_epsilon_%.2f_%d_%.2f.csv",start,epsilon,SIZE,a);
    sprintf(ruta2,"../../Notebooks_py/Datos/Harmonic/configs/start_%d_epsilon_%.2f_%d_%.2f.csv",start,epsilon,SIZE,a);

//ruta y apertura de los archivos 
   
    FILE *archivo = fopen(ruta, "w");
    FILE *archivo2 = fopen(ruta2, "w");

//Cabeceras de los archivos csv
    fprintf(archivo,"sweep,S_E\n");
    //cabecera del archivo de configuraciones "x"
    for(int i = 0;i<SIZE;i++){
        if(i==SIZE-1){
            fprintf(archivo2,"%d\n",i);
        }else{
            fprintf(archivo2,"%d,",i);
        }
    }
    
    
//Estado inicial
    SE = S_E(x);
    fprintf(archivo, "%d,%f\n",0,SE);

//Sweeps
    for(int i =1;i<max_sweeps;i++){
        if(i>1000 && i%10==0){
            for(int j=0;j<SIZE;j++){
                if(j==SIZE-1){
                    fprintf(archivo2,"%f\n",x[j]);
                }else{
                    fprintf(archivo2,"%f,",x[j]);
                }
            }
        }
        sweep(x,epsilon);
        SE = S_E(x);
        //if(i>=1000 && i%10 == 0){//tomamos valores cada 10 sweeps a partir de 1000 (termalizacion)    
          //  fprintf(archivo, "%d,%d,%d,%d,%d\n", i,H_tot,mag,H_tot,mag);
        //}else{
            fprintf(archivo, "%d,%f\n", i,SE);
        //}
    }

    fclose(archivo);
}

void epsilon_rates(float e_0,float e_f,float step){

    int start = 1;//hot or cold
    int max_sweeps = 10;
    char ruta[500];
    double acclist[50]={0};
    float acc,std;

    sprintf(ruta,"../../Notebooks_Py/Datos/Harmonic/acc_rates_%.2f.csv",a);
    FILE *archivo = fopen(ruta, "w");
    fprintf(archivo,"epsilon,acc,std\n");

    for(float epsilon=e_0;epsilon<=e_f;epsilon+=step){
        for(int i=0;i<50;i++){
        init_simulation(start,max_sweeps,epsilon);
        acclist[i] = acc_rate/total_prob;
        }
        fprintf(archivo,"%f,%f,%f\n",epsilon,mean(50,acclist),error(50,acclist));
        printf("epsilon: %0.1f,mean: %0.4f,stderr: %0.4f\n",epsilon,mean(50,acclist),error(50,acclist));
    }
}

int main(){
    srand(time(NULL));
    //srand(1720392752); //10 dE>0
    //srand(1720393054);
    //srand(1720393054);

    int start = 0;
    int max_sweeps = 11000;
    float epsilon = 0.7;

    //epsilon_rates(0.0,3,0.5);

    init_simulation(start,max_sweeps,epsilon);
    
return 0;
}
