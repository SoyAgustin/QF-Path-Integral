#ifndef MY_MATH_STATS_H
#define MY_MATH_STATS_H

//Random numbers
double randnum(float min, float max);
int randnum_int(int min,int max);

//Stats
double mean(int N,double x[]);
double var(int N, double x[]);
double error(int N, double x[]);

//Vect
int is_origin(int dim,int array[]);

#endif