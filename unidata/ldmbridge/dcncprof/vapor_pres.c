#include <stdio.h>
#include <math.h>

float T_K = 273.15;
float ec = 6.11;
float Rv = 461.0;
float Lv = (float)2.5e6;

/* T is in Kelvin! */
void VAPOR_PRES(float T,float *e)
{
*e = ec * (float)(exp((Lv/Rv) * ((1./T_K)-(1./(T)))));
}

void t_from_e(float e,float *t)
{
*t = 1. / ( (1./T_K) - ((Rv / Lv) * log(e / ec)) );
}
