#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>


/*Función que genera un número aleatorio entre un valor mínimo y máximo*/
float randnum(float min, float max) {
    float num = (rand()/((float)RAND_MAX));//numero aleatorio entre (0,1)
    num = min+(max-min)*num;//numero aleatorio entre (min,max)
    return num;
}

int main() {
    srand(time(NULL));

    float Ac=0.0; 
    float r=1.0;
    float n,x,y,z,d;
    float pi;

    printf("Numero de puntos:");
    scanf("%f",&n);
    
    for(int i=1;i<=n;i++){
        x = randnum(-r,r);
        y = randnum(-r,r);
        z = randnum(-r,r);
        d = sqrt(x*x+y*y+z*z);
        if(d<=1){Ac++;} 
    }

    pi = 8.0*(3.0/4.0)*(Ac/n);
    printf("pi = %f\n",pi);

    return 0;
}