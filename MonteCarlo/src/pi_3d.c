#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "../lib/my_math_stats.h"

int main() {
    srand(time(NULL));

    double Ac=0.0; 
    double r=1.0;
    double n,x,y,z,d;
    double pi;

    printf("Numero de puntos:");
    scanf("%lf",&n);
    
    for(int i=1;i<=n;i++){
        x = randnum(-r,r);
        y = randnum(-r,r);
        z = randnum(-r,r);
        d = sqrt(x*x+y*y+z*z);
        if(d<=1){Ac++;} 
    }

    pi = 8.0*(3.0/4.0)*(Ac/n);
    printf("pi = %lf\n",pi);

    return 0;
}