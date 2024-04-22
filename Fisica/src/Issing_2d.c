#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "../lib/my_math_stats.h"

#define SIZE 3

typedef struct{
    int up;
    int down;
    int left;
    int right;
} neighbor;

neighbor get_neighbors(int row, int col){
    neighbor n;

    n.left = (col - 1);
    if (n.left < 0)
    {
        n.left = SIZE - 1;
    }

    n.right = (col + 1);
    if (n.right >= SIZE)
    {
        n.right = 0;
    }

    n.up = (row - 1);
    if (n.up < 0)
    {
        n.up = SIZE - 1;
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

    sum += lattice[n.up][col];
    sum += lattice[n.down][col];
    sum += lattice[row][n.left];
    sum += lattice[row][n.right];

    return -1*lattice[row][col] * sum;
}

int H(int lattice[SIZE][SIZE]){
    int sum = 0;
    for (int i = 0; i < SIZE; i++){
        for (int j = 0; j < SIZE; j++){
            sum += H_ij(lattice, i, j);
        }
    }
    return sum;
}

float p_accept(int dH,int T){
    if(dH<=0){
        return 1.0;
    }else{
        return exp(-dH/T);
    }
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
}

void sweep(int lattice[SIZE][SIZE], int T){
    int h0_ij,hs_ij,dH;
    float p,rand;
    for(int i=0;i<SIZE;i++){
        for(int j=0;j<SIZE;j++){

            h0_ij = H_ij(lattice,i,j);
            //printf("h0: %d\n",h0_ij);
            //printf("lattice[%d][%d]: %d\n",i,j,lattice[i][j]);

            lattice[i][j] = -1*lattice[i][j];
            hs_ij = H_ij(lattice,i,j);
            //printf("hs: %d\n",hs_ij);

            dH = hs_ij - h0_ij;
            //printf("dH:%d\n",dH);

            p = p_accept(dH,T);
            rand = randnum(0,1);
            //printf("p:%f\n",p);
            //printf("rand:%f\n",rand);
            if (p!=1.0 && rand > p){
                lattice[i][j] = -1*lattice[i][j];
            }
            //printf("lattice[%d][%d]: %d\n\n",i,j,lattice[i][j]);
        }
    }
}

int main(){
    srand(316032629);

    int lattice[SIZE][SIZE] = {0};
    int T=4;
    initialize_lattice(lattice);
    print_lattice(lattice);
    printf("%d\n",H(lattice));

   for(int i=0;i<50;i++){
    sweep(lattice, T);
    print_lattice(lattice);
    printf("%d\n",H(lattice));
   }
return 0;
}