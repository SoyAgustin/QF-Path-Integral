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
        sum+=T(x,i,a)+V(x,i,lambda);
    }
    return sum*a;
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

void init_simulation(int start, int max_sweeps,int thermalization, float epsilon, float a, float lambda,int save_acc_rates){

    float x[SIZE]={0};
    char ruta_SE[500];
    char ruta_trayec[500];
    char ruta_acc_rates[500];
    char ruta_corr[100];
    float SE;
    float acc_rate_i;
    float corr;

//Rutas, archivos e inicialización hot y cold
    initialize_x(x,start);
    sprintf(ruta_SE,"../../Notebooks_Py/Datos/Oscillator/SE/start_%d_eps_%.2f_size_%d_a_%.2f_lambda_%.2f.csv",start,epsilon,SIZE,a,lambda);
    sprintf(ruta_trayec,"../../Notebooks_Py/Datos/Oscillator/trayec/start_%d_eps_%.2f_size_%d_a_%.2f_lambda_%.2f.csv",start,epsilon,SIZE,a,lambda);
    sprintf(ruta_acc_rates,"../../Notebooks_Py/Datos/Oscillator/acc_rates/start_%d_eps_%.2f_size_%d_a_%.2f_lambda_%.2f.csv",start,epsilon,SIZE,a,lambda);
    sprintf(ruta_corr,"../../Notebooks_Py/Datos/Oscillator/corr/start_%d_eps_%.2f_size_%d_a_%.2f_lambda_%.2f.csv",start,epsilon,SIZE,a,lambda);

//ruta y apertura de los archivos 
   
    FILE *archivo_SE = fopen(ruta_SE, "w");
    FILE *archivo_trayec = fopen(ruta_trayec, "w");
    FILE *archivo_acc_rates = fopen(ruta_acc_rates, "w");
    FILE *archivo_corr = fopen(ruta_corr, "w");

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
//cabecera del archivo acc_rates
    fprintf(archivo_acc_rates,"epsilon,acc_rate\n");

//cabecera del archivo corr
    fprintf(archivo_corr,"d,corr\n");
    
//Estado inicial
    SE = S_E(x,a,lambda);
    fprintf(archivo_SE, "%d,%f\n",0,SE);

//Sweeps
    for(int i =1;i<max_sweeps;i++){
       
        if(i>thermalization && i%10==0){
            for(int j=0;j<SIZE;j++){
                if(j==SIZE-1){
                    fprintf(archivo_trayec,"%f\n",x[j]);
                    fprintf(archivo_corr,"%d,%f\n",j,corr_d(x,j));
                }else{
                    fprintf(archivo_trayec,"%f,",x[j]);
                    fprintf(archivo_corr,"%d,%f\n",j,corr_d(x,j));
                }
            }
            if(save_acc_rates==1){
                fprintf(archivo_acc_rates,"%f,%f\n",epsilon,acc_rate_i);
            }
            
        }

        acc_rate_i = sweep(x,epsilon,a,lambda);
        SE = S_E(x,a,lambda);
        fprintf(archivo_SE, "%d,%f\n", i,SE);
    }

    fclose(archivo_SE);
    fclose(archivo_trayec);
    fclose(archivo_acc_rates);
    fclose(archivo_corr);

}


void epsilon_rates(float init_epsilon, float final_epsilon, float step, float a, float lambda){

    float epsilon = init_epsilon;
    while(epsilon <=final_epsilon+step){
        int start = 1;
        int max_sweeps = 510000;
        int thermalization = 10000;
        int save_acc_rates = 1;
        init_simulation(start,max_sweeps,thermalization,epsilon,a,lambda,save_acc_rates);
        epsilon+=step;
    }
    
}


int main(){
    
    
    //Tests

    //Simulation
    /*
    srand(time(NULL));
    
    int max_sweeps = 510000;
    int thermalization = 10000;
    int start[2]={0,1};
    float epsilon = 0.7;
    float a = 1;
    float lambda = 1;
    int save_acc_rates = 0;
    for(int i=0;i<2;i++){
        init_simulation(start[i],max_sweeps,thermalization,epsilon,a,lambda,save_acc_rates);
        printf("start: %d,a: %f, lambda: %f\n",start[i],a,lambda);
    }
    printf("\nSimulación completada!\n");
    */

    //Epsilon rates
    
    srand(time(NULL));
    float init_epsilon = 0.0;
    float final_epsilon = 2.0;
    float step = 0.2;
    float lambda[3]={0.0,0.5,1.0};
    float a[5]={1.0,0.5,0.25,0.1,0.05};
    for(int j=0;j<3;j++){
        for(int i=0;i<5;i++){
            epsilon_rates(init_epsilon,final_epsilon,step,a[i],lambda[j]);
            
            printf("a: %f, lambda: %f",a[i],lambda[j]);
        }
    }
    printf("\nacc rates completado!\n");
    

    return 0;
}