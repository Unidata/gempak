
/*
             !!! THIS IS A GENERATED FILE !!!
             !!!  DO NOT ATTEMPT TO EDIT !!!

 int nvprep(int islot, int *iparms)

 Setup and initialize nav module.
  = 0 OK
  =-1 Invalid or missing nav parameters
  =-2 Bad slot or navigation type

 islot = (I) Logical slot number
 iparms = (I) Array of nav parameters

*/

#include <strings.h>
#define NUMNAV 9  
#define NUMOPTIN 3
#define NUMOPTOUT 3

	
int nv1ini(int , int *);
int nv1eas(double,double,double,double*,double*,double*);
int nv1sae(double,double,double,double*,double*,double*);
int nv1opt(int , double *, double *);
int nv1inigmsx_(int *, int *);
int nv1easgmsx_(float*,float*,float*,float*,float*,float*);
int nv1saegmsx_(float*,float*,float*,float*,float*,float*);
int nv1optgmsx_(int *, float *, float *);
int nv1inigoes_(int *, int *);
int nv1easgoes_(float*,float*,float*,float*,float*,float*);
int nv1saegoes_(float*,float*,float*,float*,float*,float*);
int nv1optgoes_(int *, float *, float *);
int nv1inigvar_(int *, int *);
int nv1easgvar_(float*,float*,float*,float*,float*,float*);
int nv1saegvar_(float*,float*,float*,float*,float*,float*);
int nv1optgvar_(int *, float *, float *);
int nv1inimerc_(int *, int *);
int nv1easmerc_(float*,float*,float*,float*,float*,float*);
int nv1saemerc_(float*,float*,float*,float*,float*,float*);
int nv1optmerc_(int *, float *, float *);
int nv1inimsat_(int *, int *);
int nv1easmsat_(float*,float*,float*,float*,float*,float*);
int nv1saemsat_(float*,float*,float*,float*,float*,float*);
int nv1optmsat_(int *, float *, float *);
int nv1inimsgt_(int *, int *);
int nv1easmsgt_(float*,float*,float*,float*,float*,float*);
int nv1saemsgt_(float*,float*,float*,float*,float*,float*);
int nv1optmsgt_(int *, float *, float *);
int nv1inimtst_(int *, int *);
int nv1easmtst_(float*,float*,float*,float*,float*,float*);
int nv1saemtst_(float*,float*,float*,float*,float*,float*);
int nv1optmtst_(int *, float *, float *);
int nv1inips_(int *, int *);
int nv1easps_(float*,float*,float*,float*,float*,float*);
int nv1saeps_(float*,float*,float*,float*,float*,float*);
int nv1optps_(int *, float *, float *);
int nv1inirect_(int *, int *);
int nv1easrect_(float*,float*,float*,float*,float*,float*);
int nv1saerect_(float*,float*,float*,float*,float*,float*);
int nv1optrect_(int *, float *, float *);
int nv2ini(int , int *);
int nv2eas(double,double,double,double*,double*,double*);
int nv2sae(double,double,double,double*,double*,double*);
int nv2opt(int , double *, double *);
int nv2inigmsx_(int *, int *);
int nv2easgmsx_(float*,float*,float*,float*,float*,float*);
int nv2saegmsx_(float*,float*,float*,float*,float*,float*);
int nv2optgmsx_(int *, float *, float *);
int nv2inigoes_(int *, int *);
int nv2easgoes_(float*,float*,float*,float*,float*,float*);
int nv2saegoes_(float*,float*,float*,float*,float*,float*);
int nv2optgoes_(int *, float *, float *);
int nv2inigvar_(int *, int *);
int nv2easgvar_(float*,float*,float*,float*,float*,float*);
int nv2saegvar_(float*,float*,float*,float*,float*,float*);
int nv2optgvar_(int *, float *, float *);
int nv2inimerc_(int *, int *);
int nv2easmerc_(float*,float*,float*,float*,float*,float*);
int nv2saemerc_(float*,float*,float*,float*,float*,float*);
int nv2optmerc_(int *, float *, float *);
int nv2inimsat_(int *, int *);
int nv2easmsat_(float*,float*,float*,float*,float*,float*);
int nv2saemsat_(float*,float*,float*,float*,float*,float*);
int nv2optmsat_(int *, float *, float *);
int nv2inimsgt_(int *, int *);
int nv2easmsgt_(float*,float*,float*,float*,float*,float*);
int nv2saemsgt_(float*,float*,float*,float*,float*,float*);
int nv2optmsgt_(int *, float *, float *);
int nv2inimtst_(int *, int *);
int nv2easmtst_(float*,float*,float*,float*,float*,float*);
int nv2saemtst_(float*,float*,float*,float*,float*,float*);
int nv2optmtst_(int *, float *, float *);
int nv2inips_(int *, int *);
int nv2easps_(float*,float*,float*,float*,float*,float*);
int nv2saeps_(float*,float*,float*,float*,float*,float*);
int nv2optps_(int *, float *, float *);
int nv2inirect_(int *, int *);
int nv2easrect_(float*,float*,float*,float*,float*,float*);
int nv2saerect_(float*,float*,float*,float*,float*,float*);
int nv2optrect_(int *, float *, float *);
int nv3ini(int , int *);
int nv3eas(double,double,double,double*,double*,double*);
int nv3sae(double,double,double,double*,double*,double*);
int nv3opt(int , double *, double *);
int nv3inigmsx_(int *, int *);
int nv3easgmsx_(float*,float*,float*,float*,float*,float*);
int nv3saegmsx_(float*,float*,float*,float*,float*,float*);
int nv3optgmsx_(int *, float *, float *);
int nv3inigoes_(int *, int *);
int nv3easgoes_(float*,float*,float*,float*,float*,float*);
int nv3saegoes_(float*,float*,float*,float*,float*,float*);
int nv3optgoes_(int *, float *, float *);
int nv3inigvar_(int *, int *);
int nv3easgvar_(float*,float*,float*,float*,float*,float*);
int nv3saegvar_(float*,float*,float*,float*,float*,float*);
int nv3optgvar_(int *, float *, float *);
int nv3inimerc_(int *, int *);
int nv3easmerc_(float*,float*,float*,float*,float*,float*);
int nv3saemerc_(float*,float*,float*,float*,float*,float*);
int nv3optmerc_(int *, float *, float *);
int nv3inimsat_(int *, int *);
int nv3easmsat_(float*,float*,float*,float*,float*,float*);
int nv3saemsat_(float*,float*,float*,float*,float*,float*);
int nv3optmsat_(int *, float *, float *);
int nv3inimsgt_(int *, int *);
int nv3easmsgt_(float*,float*,float*,float*,float*,float*);
int nv3saemsgt_(float*,float*,float*,float*,float*,float*);
int nv3optmsgt_(int *, float *, float *);
int nv3inimtst_(int *, int *);
int nv3easmtst_(float*,float*,float*,float*,float*,float*);
int nv3saemtst_(float*,float*,float*,float*,float*,float*);
int nv3optmtst_(int *, float *, float *);
int nv3inips_(int *, int *);
int nv3easps_(float*,float*,float*,float*,float*,float*);
int nv3saeps_(float*,float*,float*,float*,float*,float*);
int nv3optps_(int *, float *, float *);
int nv3inirect_(int *, int *);
int nv3easrect_(float*,float*,float*,float*,float*,float*);
int nv3saerect_(float*,float*,float*,float*,float*,float*);
int nv3optrect_(int *, float *, float *);


int nvprep(int, int *);
int navhandles[3];

int nvprep(int islot, int *iparms)
{

        


      int i;
      int rc;
      static const char *cll = "LL  ";

      static const char *csubs[NUMNAV] = { 
       "GMSX","GOES","GVAR","MERC","MSAT","MSGT","MTST","PS  ",
       "RECT"};


      if(islot < 1 || islot > 3) return(-2);
      rc = 0;

      for (i=0; i < NUMNAV; ++i)
      {
         if(!strncmp(csubs[i], (char*)&iparms[0], 4))
         {
            navhandles[islot-1] = i+1;
            switch(islot) 
            {
 
            case 1:
               rc=nv1ini(1, iparms);
               rc=nv1ini(2, (int*)cll);
               break;
            case 2:
               rc=nv2ini(1, iparms);
               rc=nv2ini(2, (int*)cll);
               break;
            case 3:
               rc=nv3ini(1, iparms);
               rc=nv3ini(2, (int*)cll);
               break;
            default:
              rc = -2;
              break;
            }
            return(rc);
         }
    }   
    return(-2);
}



int nv1ini(int i1, int *i2)
{
   int rc = 0; 


   switch(navhandles[0])
   {
        case 1:
           rc = nv1inigmsx_(&i1, i2);
           break;
        case 2:
           rc = nv1inigoes_(&i1, i2);
           break;
        case 3:
           rc = nv1inigvar_(&i1, i2);
           break;
        case 4:
           rc = nv1inimerc_(&i1, i2);
           break;
        case 5:
           rc = nv1inimsat_(&i1, i2);
           break;
        case 6:
           rc = nv1inimsgt_(&i1, i2);
           break;
        case 7:
           rc = nv1inimtst_(&i1, i2);
           break;
        case 8:
           rc = nv1inips_(&i1, i2);
           break;
        case 9:
           rc = nv1inirect_(&i1, i2);
           break;
	default:
	   rc = -2;
	   break;
   }
   return(rc);
}

int nv2ini(int i1, int *i2)
{
   int rc = 0; 


   switch(navhandles[1])
   {
        case 1:
           rc = nv2inigmsx_(&i1, i2);
           break;
        case 2:
           rc = nv2inigoes_(&i1, i2);
           break;
        case 3:
           rc = nv2inigvar_(&i1, i2);
           break;
        case 4:
           rc = nv2inimerc_(&i1, i2);
           break;
        case 5:
           rc = nv2inimsat_(&i1, i2);
           break;
        case 6:
           rc = nv2inimsgt_(&i1, i2);
           break;
        case 7:
           rc = nv2inimtst_(&i1, i2);
           break;
        case 8:
           rc = nv2inips_(&i1, i2);
           break;
        case 9:
           rc = nv2inirect_(&i1, i2);
           break;
	default:
	   rc = -2;
	   break;
   }
   return(rc);
}

int nv3ini(int i1, int *i2)
{
   int rc = 0; 


   switch(navhandles[2])
   {
        case 1:
           rc = nv3inigmsx_(&i1, i2);
           break;
        case 2:
           rc = nv3inigoes_(&i1, i2);
           break;
        case 3:
           rc = nv3inigvar_(&i1, i2);
           break;
        case 4:
           rc = nv3inimerc_(&i1, i2);
           break;
        case 5:
           rc = nv3inimsat_(&i1, i2);
           break;
        case 6:
           rc = nv3inimsgt_(&i1, i2);
           break;
        case 7:
           rc = nv3inimtst_(&i1, i2);
           break;
        case 8:
           rc = nv3inips_(&i1, i2);
           break;
        case 9:
           rc = nv3inirect_(&i1, i2);
           break;
	default:
	   rc = -2;
	   break;
   }
   return(rc);
}

int nv1eas(double d1, double d2, double d3, 
           double *d4, double *d5, double *d6)
{
   float x1, x2, x3, x4, x5, x6; 
   int rc = 0;

   x1 = (float) d1;
   x2 = (float) d2;
   x3 = (float) d3;


   switch(navhandles[0])
   {
        case 1:
           rc = nv1easgmsx_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 2:
           rc = nv1easgoes_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 3:
           rc = nv1easgvar_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 4:
           rc = nv1easmerc_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 5:
           rc = nv1easmsat_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 6:
           rc = nv1easmsgt_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 7:
           rc = nv1easmtst_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 8:
           rc = nv1easps_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 9:
           rc = nv1easrect_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
	default:
	   rc = -2;
	   break;
   }
   *d4 = (double) x4;
   *d5 = (double) x5;
   return(rc);
}

int nv2eas(double d1, double d2, double d3, 
           double *d4, double *d5, double *d6)
{
   float x1, x2, x3, x4, x5, x6; 
   int rc = 0;

   x1 = (float) d1;
   x2 = (float) d2;
   x3 = (float) d3;


   switch(navhandles[1])
   {
        case 1:
           rc = nv2easgmsx_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 2:
           rc = nv2easgoes_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 3:
           rc = nv2easgvar_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 4:
           rc = nv2easmerc_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 5:
           rc = nv2easmsat_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 6:
           rc = nv2easmsgt_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 7:
           rc = nv2easmtst_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 8:
           rc = nv2easps_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 9:
           rc = nv2easrect_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
	default:
	   rc = -2;
	   break;
   }
   *d4 = (double) x4;
   *d5 = (double) x5;
   return(rc);
}

int nv3eas(double d1, double d2, double d3, 
           double *d4, double *d5, double *d6)
{
   float x1, x2, x3, x4, x5, x6; 
   int rc = 0;

   x1 = (float) d1;
   x2 = (float) d2;
   x3 = (float) d3;


   switch(navhandles[2])
   {
        case 1:
           rc = nv3easgmsx_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 2:
           rc = nv3easgoes_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 3:
           rc = nv3easgvar_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 4:
           rc = nv3easmerc_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 5:
           rc = nv3easmsat_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 6:
           rc = nv3easmsgt_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 7:
           rc = nv3easmtst_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 8:
           rc = nv3easps_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 9:
           rc = nv3easrect_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
	default:
	   rc = -2;
	   break;
   }
   *d4 = (double) x4;
   *d5 = (double) x5;
   return(rc);
}

int nv1sae(double d1, double d2, double d3, 
           double *d4, double *d5, double *d6)
{
   float x1, x2, x3, x4, x5, x6; 
   int rc = 0;

   x1 = (float) d1;
   x2 = (float) d2;
   x3 = (float) d3;


   switch(navhandles[0])
   {
        case 1:
           rc = nv1saegmsx_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 2:
           rc = nv1saegoes_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 3:
           rc = nv1saegvar_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 4:
           rc = nv1saemerc_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 5:
           rc = nv1saemsat_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 6:
           rc = nv1saemsgt_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 7:
           rc = nv1saemtst_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 8:
           rc = nv1saeps_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 9:
           rc = nv1saerect_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
	default:
	   rc = -2;
	   break;
   }

   if( rc == 0) 
   {
      *d4 = (double) x4;
      *d5 = (double) x5;
   }

   return(rc);
}

int nv2sae(double d1, double d2, double d3, 
           double *d4, double *d5, double *d6)
{
   float x1, x2, x3, x4, x5, x6; 
   int rc = 0;

   x1 = (float) d1;
   x2 = (float) d2;
   x3 = (float) d3;


   switch(navhandles[1])
   {
        case 1:
           rc = nv2saegmsx_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 2:
           rc = nv2saegoes_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 3:
           rc = nv2saegvar_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 4:
           rc = nv2saemerc_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 5:
           rc = nv2saemsat_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 6:
           rc = nv2saemsgt_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 7:
           rc = nv2saemtst_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 8:
           rc = nv2saeps_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 9:
           rc = nv2saerect_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
	default:
	   rc = -2;
	   break;
   }

   if( rc == 0) 
   {
      *d4 = (double) x4;
      *d5 = (double) x5;
   }

   return(rc);
}

int nv3sae(double d1, double d2, double d3, 
           double *d4, double *d5, double *d6)
{
   float x1, x2, x3, x4, x5, x6; 
   int rc = 0;

   x1 = (float) d1;
   x2 = (float) d2;
   x3 = (float) d3;


   switch(navhandles[2])
   {
        case 1:
           rc = nv3saegmsx_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 2:
           rc = nv3saegoes_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 3:
           rc = nv3saegvar_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 4:
           rc = nv3saemerc_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 5:
           rc = nv3saemsat_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 6:
           rc = nv3saemsgt_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 7:
           rc = nv3saemtst_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 8:
           rc = nv3saeps_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
        case 9:
           rc = nv3saerect_(&x1, &x2, &x3, &x4, &x5, &x6);
           break;
	default:
	   rc = -2;
	   break;
   }

   if( rc == 0) 
   {
      *d4 = (double) x4;
      *d5 = (double) x5;
   }

   return(rc);
}

int nv1opt(int i1, double *d2, double *d3)
{ 
   float x2[NUMOPTIN];
   float x3[NUMOPTOUT];
   int i;
   int rc = 0;
  
   for(i=0;i<NUMOPTOUT;++i)
   {
      x3[i] = 0.0;
   } 

   for(i=0;i<NUMOPTIN;++i)
   {
      x2[i] = (float) d2[i];
   }


   switch(navhandles[0])
   {
        case 1:
           rc = nv1optgmsx_(&i1, x2, x3);
           break;
        case 2:
           rc = nv1optgoes_(&i1, x2, x3);
           break;
        case 3:
           rc = nv1optgvar_(&i1, x2, x3);
           break;
        case 4:
           rc = nv1optmerc_(&i1, x2, x3);
           break;
        case 5:
           rc = nv1optmsat_(&i1, x2, x3);
           break;
        case 6:
           rc = nv1optmsgt_(&i1, x2, x3);
           break;
        case 7:
           rc = nv1optmtst_(&i1, x2, x3);
           break;
        case 8:
           rc = nv1optps_(&i1, x2, x3);
           break;
        case 9:
           rc = nv1optrect_(&i1, x2, x3);
           break;
	default:
	   rc = -2;
	   break;
   }
   for(i=0;i<NUMOPTOUT;++i)
   {
      d3[i] = (double) x3[i];
   }

   return(rc);
}

int nv2opt(int i1, double *d2, double *d3)
{ 
   float x2[NUMOPTIN];
   float x3[NUMOPTOUT];
   int i;
   int rc = 0;
  
   for(i=0;i<NUMOPTOUT;++i)
   {
      x3[i] = 0.0;
   } 

   for(i=0;i<NUMOPTIN;++i)
   {
      x2[i] = (float) d2[i];
   }


   switch(navhandles[1])
   {
        case 1:
           rc = nv2optgmsx_(&i1, x2, x3);
           break;
        case 2:
           rc = nv2optgoes_(&i1, x2, x3);
           break;
        case 3:
           rc = nv2optgvar_(&i1, x2, x3);
           break;
        case 4:
           rc = nv2optmerc_(&i1, x2, x3);
           break;
        case 5:
           rc = nv2optmsat_(&i1, x2, x3);
           break;
        case 6:
           rc = nv2optmsgt_(&i1, x2, x3);
           break;
        case 7:
           rc = nv2optmtst_(&i1, x2, x3);
           break;
        case 8:
           rc = nv2optps_(&i1, x2, x3);
           break;
        case 9:
           rc = nv2optrect_(&i1, x2, x3);
           break;
	default:
	   rc = -2;
	   break;
   }
   for(i=0;i<NUMOPTOUT;++i)
   {
      d3[i] = (double) x3[i];
   }

   return(rc);
}

int nv3opt(int i1, double *d2, double *d3)
{ 
   float x2[NUMOPTIN];
   float x3[NUMOPTOUT];
   int i;
   int rc = 0;
  
   for(i=0;i<NUMOPTOUT;++i)
   {
      x3[i] = 0.0;
   } 

   for(i=0;i<NUMOPTIN;++i)
   {
      x2[i] = (float) d2[i];
   }


   switch(navhandles[2])
   {
        case 1:
           rc = nv3optgmsx_(&i1, x2, x3);
           break;
        case 2:
           rc = nv3optgoes_(&i1, x2, x3);
           break;
        case 3:
           rc = nv3optgvar_(&i1, x2, x3);
           break;
        case 4:
           rc = nv3optmerc_(&i1, x2, x3);
           break;
        case 5:
           rc = nv3optmsat_(&i1, x2, x3);
           break;
        case 6:
           rc = nv3optmsgt_(&i1, x2, x3);
           break;
        case 7:
           rc = nv3optmtst_(&i1, x2, x3);
           break;
        case 8:
           rc = nv3optps_(&i1, x2, x3);
           break;
        case 9:
           rc = nv3optrect_(&i1, x2, x3);
           break;
	default:
	   rc = -2;
	   break;
   }
   for(i=0;i<NUMOPTOUT;++i)
   {
      d3[i] = (double) x3[i];
   }

   return(rc);
}
