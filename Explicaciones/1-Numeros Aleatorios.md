# Generador de numeros aleatorios en C

Una de las formas mpas comunes de generar números aleatorios en C es mediante el uso de la función  `rand()` que se incluye en la librería estándar `<stdlib.h>` y que es un generador de números pseudo aleatorios con distribución uniforme. El uso de la función `rand()` va seguida de una función que indica la semilla `srand()` con la que se genera el número aleatorio, en caso de que no se indique esta semilla el argumento por defecto es 1.

Ahora hay que darnos cuenta que  siempre debemos dar una semilla diferente si queremos generar un número aleatorio diferente, esto nos pone en un dilema, ya que necesitamos un número aleatorio para generar un número aleatorio. La solución a este dilema se encuentra con el uso de la función `time()` que se incluye en la librería `<time.h>`; esta función devuelve el tiempo en segundos desde la fecha del 1 de enero de 1970, de modo que entonces la semilla es diferente según la fecha en la que se usa dando entonces a la función `rand()` la posibilidad de generar números aleatorios (en realidad pseudo aleatorios).

El generador de números aleatorios por defecto se encuentra en la posibilidad de generar números aleatorios enteros entre el valor 0 y el valor `RAND_MAX = 32767`, siendo este parámetro parte de la librería `<cstdlib.h>`

Un ejemplo simple de cómo se puede generar un número aleatorio en C es el siguiente:

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main(){
	srand(time(NULL));
	int num = rand();
	prinft("El numero aleatorio es: %d",num);
return 0
}
```
Ahora por comodidad en la mayoría de cálculos nos gustaría trabajar con números aleatorios en un intervalo entre 0 y 1, así que como ya sabemos que el número máximo que se puede obtener es el de la constante `RAND_MAX` se puede reemplazar `rand()` por el valor normalizado `rand()/RAND_MAX` (tomando en cuenta los respectivos cambios de tipo de variable).

Finalmente y para tener la comodidad de poder generar números aleatorios en un intervalo deseado, digamos que esto lo hacemos mediante la función `random` que recibe dos parámetros `max` y `min` y definimos la función tal y como se muestra a continuación. 
```c
float random(float min, float max){
	srand(time(NULL));
	float num = rand()/(float)RAND_MAX;
	num = min+(max-min)*num;
	return num;
}
```
