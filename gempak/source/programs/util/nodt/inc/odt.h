#include "geminc.h"

#define HISTORYPATH "./"
#define TOPOPATH "./"
#define LOCALSERVER "TEMP"
#define LOCALAREA 80

typedef int logical;
#define TRUE 1
#define FALSE 0

#define  ABS(x)     ((x) >= 0 ? (x) : -(x))
#ifndef  MAX
#define  MAX(x, y)  (((x) > (y)) ? (x) : (y))    /* use greater of 2 values */
#endif
#ifndef  MIN
#define  MIN(x, y)  (((x) < (y)) ? (x) : (y))    /* use lesser of 2 values  */
#endif
#define  SIGN(x, y) ((x)/(y))*ABS(y)
#define  SIN(x)     (float)( sin( (double)(x) ) )
#define  COS(x)     (float)( cos( (double)(x) ) )
#define  TAN(x)     (float)( tan( (double)(x) ) )
#define  ASIN(x)    (float)( asin( (double)(x) ) )
#define  ACOS(x)    (float)( acos( (double)(x) ) )
#define  ATAN(x)    (float)( atan( (double)(x) ) )
#define  SQRT(x)    (float)( sqrt( (double)(x) ) )
#define  EXP(x)     (float)( exp( (double)(x) ) )

struct irdata {
  int date;
  int time;
  float Traw;
  float Tfinal;
  float CI;
  int scenetype;
  int rule9;
  int rapid;
  float eyet;
  float cloudt;
  float meancloudt;
  float latitude;
  float longitude;
  int land;
  int ringdist;
};

struct odtdata {
  struct irdata IR;
  struct odtdata *nextrec; };

struct ringdata {
  float dist;
  float angle;
  float temp;
  struct ringdata *nextrec;
};

#define maxd 500
#define bufsiz maxd*maxd
#define slots 150

/*
 *  Prototype for odtdrive
 */
  
int odtdrive ( int curdate, int curtime, char *imagefile, float cenlat, 
		float cenlon, char *historyfile, int lodt, int lauto,
		int ldelete, int ldomain, int ldump, int llist, int lrule48, 
		int loverride, char *overridescene, int lwind, int lspotanal, 
		char *fixfil, char *outfil, char *date1, int time1, 
		char *date2, int time2 );

int getdates ( double *, double * );
