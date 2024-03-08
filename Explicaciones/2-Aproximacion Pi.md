# Aproximaciones numéricas del número $\pi$

Existen varios métodos numéricos para aproximar el número $\pi$ desde épocas en las que no se tenía tecnología avanzada, conforme al área de estudio de interés nos vamos a centrar en dos formas que forman parte de las llamadas pruebas Monte Carlo. 

## Caso 2 dimensional (2D)

El problema consiste en encontrar una aproximación numérica del número $\pi$, para lograrlo vamos a calcular la razón entre las áreas de un círculo de radio $r=1$ y un cuadrado de lado $l=2r$ tal y como se muestra en la siguiente imagen


El área del círculo se calcula como $A_c=\pi r²$, por otro lado el cuadrado tiene lados de longitud $2r$ y entonces el área del cuadrado se calcula como  $A_s=(2r)²$, calculando el cocicente de las áreas se obtiene que $\frac{A_c}{A_s}=\frac{\pi}{4}$ y por lo tanto se calcula $\pi$ como $\pi=4\frac{A_c}{A_s}$

Numéricamente se pueden generar dos números aleatorios que serán las coordenadas $(x,y)$ en el plano cartesiano, en particular si los dos números aleatorios se encuentran entre los valores 0 y 1 este punto siempre va a estar dentro del cuadrado y en algunas ocaciones puede caer dentro del circulo, esto ultimo ocurre cuando se cumple la condición de que la distancia sea menor que el radio, en términos de coordenadas cartesianas un punto cae dentro de la circunferencia si $d<r=\sqrt{x²+y²}$ .

Si se repite una cantidad de veces lo suficientemente grande este proceso se puede aproximar el área de la circunferencia como el número de puntos que están dentro  y el área del cuadrado se puede aproximar como el número total de puntos.

Una implementación del algoritmo en lenguaje C es la siguiente 
```c
float  randnum(float  min,  float  max)  {
float  num  =  (rand()/((float)RAND_MAX));
num  =  min+(max-min)*num;
return  num;
}

int  main()  {
srand(time(NULL));
float  Ac=0.0;
float  r=1.0;
float  n,x,y,d;
float  pi;

printf("Numero de puntos:");
scanf("%f",&n);

for(int  i=1;i<=n;i++){
	x  =  randnum(-r,r);
	y  =  randnum(-r,r);
	d  =  sqrt(x*x+y*y);
	if(d<=1){Ac++;}
}

pi  =  4.0*(Ac/n);
printf("pi = %f\n",pi);
return  0;
}
``` 

## Caso 3 dimensional (3D)
La extensión al caso de 3 dimensiones aborda el mismo problema que caso anterior pero en lugar de una circunferencia se tiene una esfera y en lugar de un cuadrado se tiene un cubo; en este caso la aproximación se puede calcular mediante el cociente de los volumenes de las dos figuras tal que por un lado el volumen de una esfera es $V_{circ}=\frac{4}{3}\pi r²$ y el volumen del cubo es $V_{cub}=(2r)³$, haciendo los cálculos correspondientes se obtiene que el valor de $\pi$ se calcula como $\pi =8\frac{3}{4}\frac{V_{circ}}{V_{cub}}$

Como es de esperar en el algoritmo hay pocos cambios con respecto a la versión anterior, agregando unicamente un número aleatorio que corresponde con el valor de la dimensión nueva. 

```c
float  randnum(float  min,  float  max)  {
float  num  =  (rand()/((float)RAND_MAX));
num  =  min+(max-min)*num;
return  num;
}

int  main()  {
srand(time(NULL));
float  Ac=0.0;
float  r=1.0;
float  n,x,y,d;
float  pi;

printf("Numero de puntos:");
scanf("%f",&n);

for(int  i=1;i<=n;i++){
	x  =  randnum(-r,r);
	y  =  randnum(-r,r);
	z  =  randnum(-r,r);
 	d  =  sqrt(x*x+y*y+z*z);
	if(d<=1){Ac++;}
}

pi  =  8.0(3.0/4.0)*(Ac/n);
printf("pi = %f\n",pi);
return  0;
}
``` 

## Resultados 
Los siguientes resultados se tomaron corriendo la simulación una única vez por cada cambio en el número de puntos, por lo tanto no pueden tomarse como medida seria de la efectividad del método para los diferentes casos. 
|Número de puntos|2D|3D|
|--|--|--|
|100|3.28|3.36|
|1,000|3.184|3.198|
|10,000|3.1556|3.153|
|100,000|3.14232|3.14784|
|1,000,000|3.1383|3.141812|
|10,000,000|3.141708|3.142553|

