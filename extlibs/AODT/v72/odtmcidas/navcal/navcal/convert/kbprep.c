
/*
             !!! THIS IS A GENERATED FILE !!!
             !!!  DO NOT ATTEMPT TO EDIT !!!

 int kbprep(int islot, struct ARACOM *aracom)

 Setup and initialize cal module.
  = 0 OK
  =-1 Invalid or missing cal parameters
  =-2 Bad slot or calibration type

 islot = (I) Logical slot number
 aracom = (struct ARACOM *) structure for area parameters

*/

#include <strings.h>
#include "aracom.h"
#define NUMCAL 8
#define NUMOPTIN 3
#define NUMOPTOUT 3
        
int kb1ini(int, struct ARACOM *);
int kb1cal(long *, long *, int, int, long *);
int kb1opt(long, long *, long *);
int kb1iniaaa_(char *, char *, int *, int, int);
int kb1calaaa_(int *, int *, int *, int *, int *);
int kb1optaaa_(char *, int *, int *, int);
int kb1inify2_(char *, char *, int *, int, int);
int kb1calfy2_(int *, int *, int *, int *, int *);
int kb1optfy2_(char *, int *, int *, int);
int kb1inigms_(char *, char *, int *, int, int);
int kb1calgms_(int *, int *, int *, int *, int *);
int kb1optgms_(char *, int *, int *, int);
int kb1inigvar_(char *, char *, int *, int, int);
int kb1calgvar_(int *, int *, int *, int *, int *);
int kb1optgvar_(char *, int *, int *, int);
int kb1inimsat_(char *, char *, int *, int, int);
int kb1calmsat_(int *, int *, int *, int *, int *);
int kb1optmsat_(char *, int *, int *, int);
int kb1inimsg_(char *, char *, int *, int, int);
int kb1calmsg_(int *, int *, int *, int *, int *);
int kb1optmsg_(char *, int *, int *, int);
int kb1inimtst_(char *, char *, int *, int, int);
int kb1calmtst_(int *, int *, int *, int *, int *);
int kb1optmtst_(char *, int *, int *, int);
int kb1inivisr_(char *, char *, int *, int, int);
int kb1calvisr_(int *, int *, int *, int *, int *);
int kb1optvisr_(char *, int *, int *, int);
int kb2ini(int, struct ARACOM *);
int kb2cal(long *, long *, int, int, long *);
int kb2opt(long, long *, long *);
int kb2iniaaa_(char *, char *, int *, int, int);
int kb2calaaa_(int *, int *, int *, int *, int *);
int kb2optaaa_(char *, int *, int *, int);
int kb2inify2_(char *, char *, int *, int, int);
int kb2calfy2_(int *, int *, int *, int *, int *);
int kb2optfy2_(char *, int *, int *, int);
int kb2inigms_(char *, char *, int *, int, int);
int kb2calgms_(int *, int *, int *, int *, int *);
int kb2optgms_(char *, int *, int *, int);
int kb2inigvar_(char *, char *, int *, int, int);
int kb2calgvar_(int *, int *, int *, int *, int *);
int kb2optgvar_(char *, int *, int *, int);
int kb2inimsat_(char *, char *, int *, int, int);
int kb2calmsat_(int *, int *, int *, int *, int *);
int kb2optmsat_(char *, int *, int *, int);
int kb2inimsg_(char *, char *, int *, int, int);
int kb2calmsg_(int *, int *, int *, int *, int *);
int kb2optmsg_(char *, int *, int *, int);
int kb2inimtst_(char *, char *, int *, int, int);
int kb2calmtst_(int *, int *, int *, int *, int *);
int kb2optmtst_(char *, int *, int *, int);
int kb2inivisr_(char *, char *, int *, int, int);
int kb2calvisr_(int *, int *, int *, int *, int *);
int kb2optvisr_(char *, int *, int *, int);
int kb3ini(int, struct ARACOM *);
int kb3cal(long *, long *, int, int, long *);
int kb3opt(long, long *, long *);
int kb3iniaaa_(char *, char *, int *, int, int);
int kb3calaaa_(int *, int *, int *, int *, int *);
int kb3optaaa_(char *, int *, int *, int);
int kb3inify2_(char *, char *, int *, int, int);
int kb3calfy2_(int *, int *, int *, int *, int *);
int kb3optfy2_(char *, int *, int *, int);
int kb3inigms_(char *, char *, int *, int, int);
int kb3calgms_(int *, int *, int *, int *, int *);
int kb3optgms_(char *, int *, int *, int);
int kb3inigvar_(char *, char *, int *, int, int);
int kb3calgvar_(int *, int *, int *, int *, int *);
int kb3optgvar_(char *, int *, int *, int);
int kb3inimsat_(char *, char *, int *, int, int);
int kb3calmsat_(int *, int *, int *, int *, int *);
int kb3optmsat_(char *, int *, int *, int);
int kb3inimsg_(char *, char *, int *, int, int);
int kb3calmsg_(int *, int *, int *, int *, int *);
int kb3optmsg_(char *, int *, int *, int);
int kb3inimtst_(char *, char *, int *, int, int);
int kb3calmtst_(int *, int *, int *, int *, int *);
int kb3optmtst_(char *, int *, int *, int);
int kb3inivisr_(char *, char *, int *, int, int);
int kb3calvisr_(int *, int *, int *, int *, int *);
int kb3optvisr_(char *, int *, int *, int);


int kbprep(int, struct ARACOM *);
int calhandles[3];

int kbprep(int islot, struct ARACOM *aracom)
{ 
      int i;
      int rc;

      static const char *csubs[NUMCAL] = { 
       "AAA ","FY2 ","GMS ","GVAR","MSAT","MSG ","MTST","VISR"}; 
 
      if(islot < 1 || islot > 3) return(-2);
      if (aracom->dir[52]==aracom->options[2]) return(0);
      rc = 0;

      for (i=0; i < NUMCAL; ++i)
      {
         if(!strncmp(csubs[i], (char*)&aracom->dir[51], 4))
         {
            calhandles[islot - 1] = i+1;
            aracom->slot = islot;
            switch(islot)
            { 
            case 1:
               rc=kb1ini(1, aracom);
               break;
            case 2:
               rc=kb2ini(1, aracom);
               break;
            case 3:
               rc=kb3ini(1, aracom);
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


int kb1ini(int i1, struct ARACOM *aracom)
{
   int rc = 0;

   aracom->cin = aracom->dir[52];
   aracom->cout = aracom->options[2];

   switch(calhandles[0])
   {
        case 1:
           rc = kb1iniaaa_((char *)&aracom->cin, (char *)&aracom->cout, (int *)aracom->options, 4, 4);
           break;
        case 2:
           rc = kb1inify2_((char *)&aracom->cin, (char *)&aracom->cout, (int *)aracom->options, 4, 4);
           break;
        case 3:
           rc = kb1inigms_((char *)&aracom->cin, (char *)&aracom->cout, (int *)aracom->options, 4, 4);
           break;
        case 4:
           rc = kb1inigvar_((char *)&aracom->cin, (char *)&aracom->cout, (int *)aracom->options, 4, 4);
           break;
        case 5:
           rc = kb1inimsat_((char *)&aracom->cin, (char *)&aracom->cout, (int *)aracom->options, 4, 4);
           break;
        case 6:
           rc = kb1inimsg_((char *)&aracom->cin, (char *)&aracom->cout, (int *)aracom->options, 4, 4);
           break;
        case 7:
           rc = kb1inimtst_((char *)&aracom->cin, (char *)&aracom->cout, (int *)aracom->options, 4, 4);
           break;
        case 8:
           rc = kb1inivisr_((char *)&aracom->cin, (char *)&aracom->cout, (int *)aracom->options, 4, 4);
           break;
        default:
           rc = -2;
           break;
   }
   return(rc);
}

int kb2ini(int i1, struct ARACOM *aracom)
{
   int rc = 0;

   aracom->cin = aracom->dir[52];
   aracom->cout = aracom->options[2];

   switch(calhandles[1])
   {
        case 1:
           rc = kb2iniaaa_((char *)&aracom->cin, (char *)&aracom->cout, (int *)aracom->options, 4, 4);
           break;
        case 2:
           rc = kb2inify2_((char *)&aracom->cin, (char *)&aracom->cout, (int *)aracom->options, 4, 4);
           break;
        case 3:
           rc = kb2inigms_((char *)&aracom->cin, (char *)&aracom->cout, (int *)aracom->options, 4, 4);
           break;
        case 4:
           rc = kb2inigvar_((char *)&aracom->cin, (char *)&aracom->cout, (int *)aracom->options, 4, 4);
           break;
        case 5:
           rc = kb2inimsat_((char *)&aracom->cin, (char *)&aracom->cout, (int *)aracom->options, 4, 4);
           break;
        case 6:
           rc = kb2inimsg_((char *)&aracom->cin, (char *)&aracom->cout, (int *)aracom->options, 4, 4);
           break;
        case 7:
           rc = kb2inimtst_((char *)&aracom->cin, (char *)&aracom->cout, (int *)aracom->options, 4, 4);
           break;
        case 8:
           rc = kb2inivisr_((char *)&aracom->cin, (char *)&aracom->cout, (int *)aracom->options, 4, 4);
           break;
        default:
           rc = -2;
           break;
   }
   return(rc);
}

int kb3ini(int i1, struct ARACOM *aracom)
{
   int rc = 0;

   aracom->cin = aracom->dir[52];
   aracom->cout = aracom->options[2];

   switch(calhandles[2])
   {
        case 1:
           rc = kb3iniaaa_((char *)&aracom->cin, (char *)&aracom->cout, (int *)aracom->options, 4, 4);
           break;
        case 2:
           rc = kb3inify2_((char *)&aracom->cin, (char *)&aracom->cout, (int *)aracom->options, 4, 4);
           break;
        case 3:
           rc = kb3inigms_((char *)&aracom->cin, (char *)&aracom->cout, (int *)aracom->options, 4, 4);
           break;
        case 4:
           rc = kb3inigvar_((char *)&aracom->cin, (char *)&aracom->cout, (int *)aracom->options, 4, 4);
           break;
        case 5:
           rc = kb3inimsat_((char *)&aracom->cin, (char *)&aracom->cout, (int *)aracom->options, 4, 4);
           break;
        case 6:
           rc = kb3inimsg_((char *)&aracom->cin, (char *)&aracom->cout, (int *)aracom->options, 4, 4);
           break;
        case 7:
           rc = kb3inimtst_((char *)&aracom->cin, (char *)&aracom->cout, (int *)aracom->options, 4, 4);
           break;
        case 8:
           rc = kb3inivisr_((char *)&aracom->cin, (char *)&aracom->cout, (int *)aracom->options, 4, 4);
           break;
        default:
           rc = -2;
           break;
   }
   return(rc);
}

int kb1cal(long *pfx, long *dir, int nval, int band, long *buf)
{
   int rc = 0;

   switch(calhandles[0])
   {
        case 1:
           rc = kb1calaaa_((int *)pfx, (int *)dir, &nval, &band, (int *)buf);
           break;
        case 2:
           rc = kb1calfy2_((int *)pfx, (int *)dir, &nval, &band, (int *)buf);
           break;
        case 3:
           rc = kb1calgms_((int *)pfx, (int *)dir, &nval, &band, (int *)buf);
           break;
        case 4:
           rc = kb1calgvar_((int *)pfx, (int *)dir, &nval, &band, (int *)buf);
           break;
        case 5:
           rc = kb1calmsat_((int *)pfx, (int *)dir, &nval, &band, (int *)buf);
           break;
        case 6:
           rc = kb1calmsg_((int *)pfx, (int *)dir, &nval, &band, (int *)buf);
           break;
        case 7:
           rc = kb1calmtst_((int *)pfx, (int *)dir, &nval, &band, (int *)buf);
           break;
        case 8:
           rc = kb1calvisr_((int *)pfx, (int *)dir, &nval, &band, (int *)buf);
           break;
        default:
           rc = -2;
           break;
   }
   return(rc);
}

int kb2cal(long *pfx, long *dir, int nval, int band, long *buf)
{
   int rc = 0;

   switch(calhandles[1])
   {
        case 1:
           rc = kb2calaaa_((int *)pfx, (int *)dir, &nval, &band, (int *)buf);
           break;
        case 2:
           rc = kb2calfy2_((int *)pfx, (int *)dir, &nval, &band, (int *)buf);
           break;
        case 3:
           rc = kb2calgms_((int *)pfx, (int *)dir, &nval, &band, (int *)buf);
           break;
        case 4:
           rc = kb2calgvar_((int *)pfx, (int *)dir, &nval, &band, (int *)buf);
           break;
        case 5:
           rc = kb2calmsat_((int *)pfx, (int *)dir, &nval, &band, (int *)buf);
           break;
        case 6:
           rc = kb2calmsg_((int *)pfx, (int *)dir, &nval, &band, (int *)buf);
           break;
        case 7:
           rc = kb2calmtst_((int *)pfx, (int *)dir, &nval, &band, (int *)buf);
           break;
        case 8:
           rc = kb2calvisr_((int *)pfx, (int *)dir, &nval, &band, (int *)buf);
           break;
        default:
           rc = -2;
           break;
   }
   return(rc);
}

int kb3cal(long *pfx, long *dir, int nval, int band, long *buf)
{
   int rc = 0;

   switch(calhandles[2])
   {
        case 1:
           rc = kb3calaaa_((int *)pfx, (int *)dir, &nval, &band, (int *)buf);
           break;
        case 2:
           rc = kb3calfy2_((int *)pfx, (int *)dir, &nval, &band, (int *)buf);
           break;
        case 3:
           rc = kb3calgms_((int *)pfx, (int *)dir, &nval, &band, (int *)buf);
           break;
        case 4:
           rc = kb3calgvar_((int *)pfx, (int *)dir, &nval, &band, (int *)buf);
           break;
        case 5:
           rc = kb3calmsat_((int *)pfx, (int *)dir, &nval, &band, (int *)buf);
           break;
        case 6:
           rc = kb3calmsg_((int *)pfx, (int *)dir, &nval, &band, (int *)buf);
           break;
        case 7:
           rc = kb3calmtst_((int *)pfx, (int *)dir, &nval, &band, (int *)buf);
           break;
        case 8:
           rc = kb3calvisr_((int *)pfx, (int *)dir, &nval, &band, (int *)buf);
           break;
        default:
           rc = -2;
           break;
   }
   return(rc);
}

int kb1opt(long c1, long *i1, long *i2)
{
   int rc = 0;

   switch(calhandles[0])
   {
        case 1:
           rc = kb1optaaa_((char *)&c1, (int *)i1, (int *)i2, 4);
           break;
        case 2:
           rc = kb1optfy2_((char *)&c1, (int *)i1, (int *)i2, 4);
           break;
        case 3:
           rc = kb1optgms_((char *)&c1, (int *)i1, (int *)i2, 4);
           break;
        case 4:
           rc = kb1optgvar_((char *)&c1, (int *)i1, (int *)i2, 4);
           break;
        case 5:
           rc = kb1optmsat_((char *)&c1, (int *)i1, (int *)i2, 4);
           break;
        case 6:
           rc = kb1optmsg_((char *)&c1, (int *)i1, (int *)i2, 4);
           break;
        case 7:
           rc = kb1optmtst_((char *)&c1, (int *)i1, (int *)i2, 4);
           break;
        case 8:
           rc = kb1optvisr_((char *)&c1, (int *)i1, (int *)i2, 4);
           break;
        default:
           rc = -2;
           break;
   }

   return(rc);
}

int kb2opt(long c1, long *i1, long *i2)
{
   int rc = 0;

   switch(calhandles[1])
   {
        case 1:
           rc = kb2optaaa_((char *)&c1, (int *)i1, (int *)i2, 4);
           break;
        case 2:
           rc = kb2optfy2_((char *)&c1, (int *)i1, (int *)i2, 4);
           break;
        case 3:
           rc = kb2optgms_((char *)&c1, (int *)i1, (int *)i2, 4);
           break;
        case 4:
           rc = kb2optgvar_((char *)&c1, (int *)i1, (int *)i2, 4);
           break;
        case 5:
           rc = kb2optmsat_((char *)&c1, (int *)i1, (int *)i2, 4);
           break;
        case 6:
           rc = kb2optmsg_((char *)&c1, (int *)i1, (int *)i2, 4);
           break;
        case 7:
           rc = kb2optmtst_((char *)&c1, (int *)i1, (int *)i2, 4);
           break;
        case 8:
           rc = kb2optvisr_((char *)&c1, (int *)i1, (int *)i2, 4);
           break;
        default:
           rc = -2;
           break;
   }

   return(rc);
}

int kb3opt(long c1, long *i1, long *i2)
{
   int rc = 0;

   switch(calhandles[2])
   {
        case 1:
           rc = kb3optaaa_((char *)&c1, (int *)i1, (int *)i2, 4);
           break;
        case 2:
           rc = kb3optfy2_((char *)&c1, (int *)i1, (int *)i2, 4);
           break;
        case 3:
           rc = kb3optgms_((char *)&c1, (int *)i1, (int *)i2, 4);
           break;
        case 4:
           rc = kb3optgvar_((char *)&c1, (int *)i1, (int *)i2, 4);
           break;
        case 5:
           rc = kb3optmsat_((char *)&c1, (int *)i1, (int *)i2, 4);
           break;
        case 6:
           rc = kb3optmsg_((char *)&c1, (int *)i1, (int *)i2, 4);
           break;
        case 7:
           rc = kb3optmtst_((char *)&c1, (int *)i1, (int *)i2, 4);
           break;
        case 8:
           rc = kb3optvisr_((char *)&c1, (int *)i1, (int *)i2, 4);
           break;
        default:
           rc = -2;
           break;
   }

   return(rc);
}
