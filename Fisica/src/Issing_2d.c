#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "../lib/my_math_stats.h"

#define SIZE 8

typedef struct{
    int down;
    int right;
    int up;
    int left;
} neighbor;

neighbor get_neighbors(int row, int col){
    neighbor n;

    n.right = (col + 1);
    if (n.right >= SIZE)
    {
        n.right = 0;
    }

    n.down = (row + 1);
    if (n.down >= SIZE)
    {
        n.down = 0;
    }

    n.up = (row - 1);
    if (n.up < 0)
    {
        n.up = SIZE - 1;
    }

    n.left = (col - 1);
    if (n.left < 0)
    {
        n.left = SIZE - 1;
    }
    return n;
}

int H_ij(int lattice[SIZE][SIZE], int row, int col){
    neighbor n = get_neighbors(row, col);
    int sum = 0;

    sum += lattice[n.down][col];
    sum += lattice[row][n.right];
    sum += lattice[n.up][col];
    sum += lattice[row][n.left];

    return -1*lattice[row][col]*sum;
}

int H(int lattice[SIZE][SIZE]){
    neighbor n ;
    int sum = 0;
    for (int i = 0; i < SIZE; i++){
        for (int j = 0; j < SIZE; j++){
            sum += H_ij(lattice,i,j);
        }
    }
    return sum/2;
}

void initialize_lattice(int lattice[SIZE][SIZE]){
    for (int i = 0; i < SIZE; i++){
        for (int j = 0; j < SIZE; j++){
            int spin = randnum_int(-1,1);
            while(spin == 0){
                spin = randnum_int(-1,1);
            }
           lattice[i][j]=spin;
        }
    }
}

void init_paralel_lattice(int lattice[SIZE][SIZE],int orientation){
    for (int i = 0; i < SIZE; i++){
        for (int j = 0; j < SIZE; j++){
            lattice[i][j] = orientation;
        }
    }
}

void print_lattice(int lattice[SIZE][SIZE]){
    printf("\n");
    for (int i = 0; i < SIZE; i++){
        for (int j = 0; j < SIZE; j++){
            printf("%d ", lattice[i][j]);
        }
        printf("\n");
    }
    printf("\n");
}

void sweep(int lattice[SIZE][SIZE], float T){
    int h0,hs,dH;
    float p,rand;
    for(int i=0;i<SIZE;i++){
        for(int j=0;j<SIZE;j++){

            h0 = H_ij(lattice,i,j);
            lattice[i][j] = -1*lattice[i][j];//Hacemos un cambio 
            hs = H_ij(lattice,i,j);

            dH = hs - h0;

            if(dH>0){ //Si es mayor que cero 
                p = exp(-1*dH/T);
                rand = randnum(0,1);
                if(rand>p){ //Si es mayor que p, no hacemos el cambio
                    lattice[i][j] = -1*lattice[i][j];
                }
            }
            
        }
    }
}

void init_simulation(int start, int max_sweeps, float T){
    
    int lattice[SIZE][SIZE];
    int H_tot;
    char ruta[50];
    if(start == 0){
        sprintf(ruta,"../../Notebooks_Py/Datos/issing_2d_cold.csv");
        init_paralel_lattice(lattice,1);
    }else{
    initialize_lattice(lattice);
    sprintf(ruta,"../../Notebooks_Py/Datos/issing_2d_hot.csv");
    }
    FILE *archivo = fopen(ruta, "w"); 

    fprintf(archivo,"sweep,H,L,T\n");
    H_tot = H(lattice); 
    fprintf(archivo, "%d,%d,%d,%.3f\n",0,H_tot,SIZE,T);

    for(int i =1;i<max_sweeps;i++){
        sweep(lattice,T);
        H_tot = H(lattice); 
        fprintf(archivo, "%d,%d,0,0\n", i,H_tot);
    }

    fclose(archivo);
}

int main(){
    srand(time(NULL));

    int max_sweeps = 200;
    float T=4;
    //0 cold-start, 1 hot-start
    init_simulation(0,max_sweeps,T);
    init_simulation(1,max_sweeps,T);
    printf("Listo :)\n");
    
return 0;
}