#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "../lib/my_math_stats.h"

#define SIZE 4

typedef struct
{
    int up;
    int down;
    int left;
    int right;
} neighbor;

neighbor get_neighbors(int row, int col)
{
    neighbor n;

    n.left = (col - 1);
    if (n.left < 0)
    {
        n.left = SIZE - 1;
    }

    n.right = (col + 1);
    if (n.right >= SIZE - 1)
    {
        n.right = 0;
    }

    n.up = (row - 1);
    if (n.up < 0)
    {
        n.up = SIZE - 1;
    }

    n.down = (row + 1);
    if (n.down >= SIZE - 1)
    {
        n.down = 0;
    }

    return n;
}

int H_ij(int lattice[SIZE][SIZE], int row, int col)
{
    neighbor n = get_neighbors(row, col);
    int sum = 0;

    sum += lattice[n.up][col];
    sum += lattice[n.down][col];
    sum += lattice[row][n.left];
    sum += lattice[row][n.right];

    return lattice[row][col] * sum;
}

int H(int lattice[SIZE][SIZE])
{
    int sum = 0;
    for (int i = 0; i < SIZE; i++)
    {
        for (int j = 0; j < SIZE; j++)
        {
            sum += H_ij(lattice, i, j);
        }
    }
    return sum;
}

int main()
{
    srand(316032629);

    int lattice[SIZE][SIZE] = {0};
    int cont = 0;
    for (int i = 0; i < SIZE; i++)
    {
        for (int j = 0; j < SIZE; j++)
        {
            /*
            int spin = randnum_int(-1,1);
            while(spin == 0){
                spin = randnum_int(-1,1);
            }
            */
            lattice[i][j] = cont;
            cont++;
        }
    }

    neighbor n;

    for(int row=0;row<SIZE;row++){
    for(int col=0;col<SIZE;col++){

    n = get_neighbors(row,col);

    printf("\t [%d,%d]: %d \t \n", n.up, col, lattice[n.up][col]);
    printf("[%d,%d]: %d \t", row, n.left, lattice[row][n.left]);
    printf("[%d,%d]: %d", row, col, lattice[row][col]);
    printf("\t [%d,%d]: %d \n", row, n.right, lattice[row][n.right]);
    printf("\t [%d,%d]: %d \t\n", n.down, col, lattice[n.down][col]);

    printf("\n");

    }
    }

for (int i = 0; i < SIZE; i++)
{
    for (int j = 0; j < SIZE; j++)
    {
        printf("%d ", lattice[i][j]);
    }
    printf("\n");
}

return 0;
}