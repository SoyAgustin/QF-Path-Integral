#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "../lib/my_math_stats.h"

#define SIZE 128

typedef struct{
    int down;
    int right;
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

    return n;
}

int H_ij(int lattice[SIZE][SIZE], int row, int col){
    neighbor n = get_neighbors(row, col);
    int sum = 0;

    sum += lattice[n.down][col];
    sum += lattice[row][n.right];

    return -1*lattice[row][col]*sum;
}

int H(int lattice[SIZE][SIZE]){
    int sum = 0;
    for (int i = 0; i < SIZE; i++){
        for (int j = 0; j < SIZE; j++){
            sum += H_ij(lattice,i,j);
        }
    }
    return sum;
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

void sweep(int lattice[SIZE][SIZE], int T){
    int h0,hs,dH;
    float p,rand;
    for(int i=0;i<SIZE;i++){
        for(int j=0;j<SIZE;j++){

            h0 = H(lattice);
            lattice[i][j] = -1*lattice[i][j];
            hs = H(lattice);

            dH = hs - h0;

            if(dH>0){
                p = exp(-1*(float)dH/T);
                rand = randnum(0,1);
                if(rand>p){
                    lattice[i][j] = -1*lattice[i][j];
                }
            }
            
        }
    }
}

int main(){
    srand(time(NULL));
    int lattice[SIZE][SIZE];

    initialize_lattice(lattice);
    //print_lattice(lattice);

    //print_lattice(lattice);

    //printf("H: %d\n",H(lattice));
    
    char ruta[50];
    sprintf(ruta,"../../Notebooks_Py/Datos/issing_2d.csv");
    FILE *archivo = fopen(ruta, "w"); 
    fprintf(archivo,"sweep,H\n");

    for(int i =0;i<100;i++){
        sweep(lattice,2);
        int H_tot = H(lattice); 
        //printf("H%d: %d\n",i,H_tot);
        fprintf(archivo, "%d,%d\n", i,H_tot);
    }

    fclose(archivo);
    
return 0;
}