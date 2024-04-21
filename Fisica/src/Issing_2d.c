#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "../lib/my_math_stats.h"

#define SIZE 4

/*
int sum_neighbors(int lattice[SIZE][SIZE], int row, int col) {
    int sum = 0;
    
    // Calcular los índices de los vecinos
    int left = (col - 1 + SIZE) % SIZE;
    int right = (col + 1) % SIZE;
    int up = (row - 1 + SIZE) % SIZE;
    int down = (row + 1) % SIZE;
    
    // Sumar los valores de los vecinos
    sum += lattice[up][col]*lattice[row][col];
    sum += lattice[down][col]*lattice[row][col];
    sum += lattice[row][left]*lattice[row][col];
    sum += lattice[row][right]*lattice[row][col];
    
    return sum;
}
*/

int main(){
    srand(316032629);

    int lattice[SIZE][SIZE]={0};
    int cont =0;
    for(int i=0;i<SIZE;i++){
        for(int j=0;j<SIZE;j++){
            /*
            int spin = randnum_int(-1,1);  
            while(spin == 0){
                spin = randnum_int(-1,1);
            }
            */
            lattice[i][j]=cont;
            cont++;
        }
    }
        int left,right,up,down;
        for(int row=0;row<SIZE;row++){
            for(int col=0;col<SIZE;col++){

                left = (col - 1);
                if (left < 0){
                    left = SIZE-1;
                    }
                

                right = (col + 1);
                if(right >= SIZE-1){
                    right = 0;
                }

                up = (row -1);
                if(up<0){
                    up = SIZE-1;
                }
                
                
                down = (row + 1);
                if(down >= SIZE-1){
                    down = 0;
                }
            

                
                
                printf("\t [%d,%d]: %d \t \n",up,col,lattice[up][col]);
                printf("[%d,%d]: %d \t",row,left,lattice[row][left]);
                printf("[%d,%d]: %d",row,col,lattice[row][col]);
                printf("\t [%d,%d]: %d \n",row,right,lattice[row][right]);
                printf("\t [%d,%d]: %d \t\n",down,col,lattice[down][col]);
                
               
                

                printf("\n");
        }
    }

    for(int i=0;i<SIZE;i++){
        for(int j=0;j<SIZE;j++){
            printf("%d ",lattice[i][j]);
        }
        printf("\n");
    }

    return 0;
}