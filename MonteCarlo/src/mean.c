#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "../lib/my_math_stats.h"

int main(){
    double valores[100];
    int row=0;
    double promedio;
    double varianza;
    double err;
    
    FILE *file = fopen("../../Notebooks_Py/Datos/randomnumbers.csv", "r");

    char line[1024];
    while (fgets(line, 1024, file)) {
        valores[row++] = atof(line);
    }

    fclose(file);

    promedio=mean(100,valores);
    varianza =var(100,valores);
    err = error(100,valores);
    printf("mean: %f\n var: %f\n err: %f\n",promedio,varianza,err);
    return 0;
}