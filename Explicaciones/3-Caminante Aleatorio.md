# Caminante aleatorio

Podemos describir un caminante aleatorio mediante un escenario ficticio en el que una persona (o partícula) puede moverse en una dirección aleatoria en cada instante de tiempo. 
Se pueden hacer simulaciones del caminante aleatorio en $n$ dimensiones, en donde por ejemplo para el caso de una dimensión, la partícula tiene la posibilidad de moverse en un sentido o en otro (adelante o atrás), en el caso de dos dimensiones la partícula puede moverse hacia adelante, atrás, izquierda o derecha, en tres dimensiones se agrega la dimensión de profundidad en la que puede moverse acercandose o alejandose. 
Vemos entonces que hay una relación entre la cantidad de posibilidades para moverse dependiendo de la dimension del espacio en el que se encuentran, se puede generalizar entonces que para cada dimensión el caminante aleatorio tiene 2 posibilidades de movimiento; en el caso general son $2d$ posibilidades en donde $d$ es la dimensión del espacio.

# Caso unidimensional (1D) 

Podemos generar una simulación del caminante aleatorio unidimensional mediante la fórmula de recurrencia
$$x_n=x_{n-1}+\Delta x$$
En donde el valor  $\Delta x$ es una variable aleatoria distribuida unidormemente 


