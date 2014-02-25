#include "geminc.h"
#include "gemprm.h"

#include <dccmn.h>
#include "grib2.h"

typedef struct nav_list {
	char id[10];
        int gnum;
        char proj[10];
        float angle[3];
        float llt, lln, ult, uln;
	int kx, ky;
	struct nav_list *next;
        } nav_list;

struct nav_list *nav_head=NULL;

void grdnav_init()
{
  int ier, iret, nitm;
  char buf[255], subst[255], *cpos;
  FILE *fp;
  nav_list *q;

  if ( ( fp = cfl_tbop ( "grdnav.tbl", "grid", &ier) ) != NULL ) {
    while ( ier == 0 ) {
      cfl_trln ( fp, sizeof(buf), buf, &ier);
      if  ( ( ier == 0 ) && ( ( q = (nav_list *)malloc( sizeof(nav_list) ) ) != NULL ) ) {

	cpos = cst_split( buf, ' ', sizeof(subst)-1, subst, &iret);
        if ( iret == 0 ) {
	  cst_ncpy(q->id,subst,sizeof(q->id)-1, &iret);
	  cst_ldsp(cpos, cpos, &nitm, &iret);
	}

	cpos = cst_split( cpos, ' ', sizeof(subst)-1, subst, &iret);
        if ( ( iret == 0 ) && ( cpos != NULL ) ) {
          iret = sscanf(subst,"%d",&(q->gnum));
	  cst_ldsp(cpos, cpos, &nitm, &iret);
	}
        else {
          free(q);
          continue;
        }

	cpos = cst_split( cpos, ' ', sizeof(subst)-1, subst, &iret);
        if ( ( iret == 0 ) && ( cpos != NULL ) ) {
	  cst_ncpy(q->proj,subst,sizeof(q->proj)-1, &iret);
	  cst_ldsp(cpos, cpos, &nitm, &iret);
	}
	else {
          free(q);
          continue;
        }

	iret = sscanf(cpos,"%f %f %f %f %f %f %f %d %d",
			&(q->angle[0]), &(q->angle[1]), &(q->angle[2]),
			&(q->llt), &(q->lln), &(q->ult), &(q->uln),
			&(q->kx), &(q->ky) );
        if ( iret == 9 ) {
	   q->next = nav_head;
	   nav_head = q;
        }
        else {
	   free(q);
	}
      }
    }
  }

  
}

/* convenience routine to get grid number for grib2
 */
int ced_num ( g2int *gdtmpl, g2int gdtlen)
{
int kx, ky, try_quick, loglev, numerr, ier;
float angl[3], llt, lln, ult, uln, basic_angle;
static char _proj[] = "CED";
char errgrp[20], errstr[81];
nav_list *q;
static nav_list *lastmatch=NULL;

  if ( gdtlen < 18 ) return(255);

  /*for ( i=0;i<gdtlen;i++)
	printf("gdtmpl %d %d\n",i,gdtmpl[i]);*/

  kx = gdtmpl[7];
  ky = gdtmpl[8];

  /* 1.0e6 assumes gdtmpl[9] = 0 or 1 for ratio of degree to 1.0e6 */
  if ( ( gdtmpl[9] == 0 ) || ( gdtmpl[9] == 1 ) )
     basic_angle = 1.0e6;
  else 
     basic_angle = 1.0e6 * gdtmpl[9] / gdtmpl[10];
  llt = gdtmpl[11] / basic_angle;
  lln = gdtmpl[12] / basic_angle; while ( lln > 180.0 ) lln = lln - 360.0;
  ult = gdtmpl[14] / basic_angle;
  uln = gdtmpl[15] / basic_angle; while ( uln > 180.0 ) uln = uln - 360.0;

  if ( lastmatch != NULL ) {
    q = lastmatch;
    try_quick = 1;
  }
  else {
    q = nav_head;
    try_quick = 0;
  }

  while ( q != NULL ) {
    if ( ( strcmp(_proj,q->proj ) == 0 ) &&
         ( kx == q->kx ) && ( ky == q->ky ) &&
         ( ( G_DIFFT( llt, q->llt, .001) && G_DIFFT( ult, q->ult, .001) ) ||
	   ( G_DIFFT( ult, q->llt, .001) && G_DIFFT( llt, q->ult, .001) ) ) &&
         G_DIFFT( lln, q->lln, .001)   &&
         G_DIFFT( uln, q->uln, .001) ) {

         lastmatch = q;
         return(q->gnum);
    }
    if ( try_quick ) {
      q = nav_head;
      try_quick = 0;
    }
    else
      q = q->next;
  }

  loglev = 2;
  numerr = -1;
  sprintf(errgrp,"%s_GNUM\0",_proj);
  sprintf(errstr,"No GID KXKY [%d %d] LL [%f %f] UR [%f %f]\n",kx,ky,llt,lln, ult, uln);
  dc_wclg(loglev,errgrp,numerr,errstr,&ier);

/*if ( ( gdtmpl[7] == 720 ) && ( gdtmpl[8] == 361 )  &&
     ( gdtmpl[11] == 90000000 ) && ( gdtmpl[12] == 0 ) &&
     ( gdtmpl[14] == -90000000 ) && ( gdtmpl[15] == 359500000 ) &&
     ( gdtmpl[16] == 500000 ) && ( gdtmpl[17] == 500000 ) )
     return ( 4 );*/

return ( 255 );
}

int mer_num ( g2int *gdtmpl, g2int gdtlen)
{
int kx, ky, try_quick;
float angl[3], llt, lln, ult, uln, basic_angle=1.0e6;
static char _proj[] = "MER";
nav_list *q;
static nav_list *lastmatch=NULL;

  if ( gdtlen < 19 ) return(255);

  /*for ( i=0;i<gdtlen;i++)
	printf("gdtmpl %d %d\n",i,gdtmpl[i]);*/

  kx = gdtmpl[7];
  ky = gdtmpl[8];

  llt = gdtmpl[9] / basic_angle;
  lln = gdtmpl[10] / basic_angle; while ( lln > 180.0 ) lln = lln - 360.0;
  ult = gdtmpl[13] / basic_angle;
  uln = gdtmpl[14] / basic_angle; while ( uln > 180.0 ) uln = uln - 360.0;

  if ( lastmatch != NULL ) {
    q = lastmatch;
    try_quick = 1;
  }
  else {
    q = nav_head;
    try_quick = 0;
  }

  while ( q != NULL ) {
    if ( ( strcmp(_proj,q->proj ) == 0 ) &&
         ( kx == q->kx ) && ( ky == q->ky ) &&
         ( ( G_DIFFT( llt, q->llt, .001) && G_DIFFT( ult, q->ult, .001) ) ||
	   ( G_DIFFT( ult, q->llt, .001) && G_DIFFT( llt, q->ult, .001) ) ) &&
         G_DIFFT( lln, q->lln, .001)   &&
         G_DIFFT( uln, q->uln, .001) ) {

         lastmatch = q;
         return(q->gnum);
    }
    if ( try_quick ) {
      q = nav_head;
      try_quick = 0;
    }
    else
      q = q->next;
  }

return ( 255 );
}

int lcc_num ( g2int *gdtmpl, g2int gdtlen)
{
int kx, ky, try_quick, loglev, numerr, ier;
float angl[3], llt, lln, ult=RMISSD, uln=RMISSD;
static char _proj[] = "LCC";
char errgrp[20], errstr[81];
nav_list *q;
static nav_list *lastmatch=NULL;

  if ( gdtlen < 21 ) return(255);

  kx = gdtmpl[7];
  ky = gdtmpl[8];

  llt = gdtmpl[9] / 1.0e6;
  lln = gdtmpl[10] / 1.0e6; while ( lln > 180.0 ) lln = lln - 360.0;

  angl[0] = gdtmpl[18] / 1.0e6;
  angl[1] = gdtmpl[13] / 1.0e6; while ( angl[1] > 180.0 ) angl[1] = angl[1] - 360.0;
  angl[2] = gdtmpl[19] / 1.0e6;

  /*for ( i=0;i<gdtlen;i++)
	printf("gdtmpl %d %d\n",i,gdtmpl[i]);*/


  if ( lastmatch != NULL ) {
    q = lastmatch;
    try_quick = 1;
  }
  else {
    q = nav_head;
    try_quick = 0;
  }

  while ( q != NULL ) {
    /* TODO: need to use either Dx and DY or ult, uln comparison */
    if ( ( strcmp(_proj,q->proj ) == 0 ) && 
	 ( kx == q->kx ) && ( ky == q->ky ) &&
	 ( G_DIFFT( llt, q->llt, .001) )  && 
	 ( G_DIFFT( lln, q->lln, .001) ) ) {
	 lastmatch = q;
	 return(q->gnum);
    }
    if ( try_quick ) {
      q = nav_head;
      try_quick = 0;
    }
    else
      q = q->next;
  }


/*if ( ( gdtmpl[7] == 491 ) && ( gdtmpl[8] == 303 )  &&
     ( gdtmpl[9] == 19943000 ) && ( gdtmpl[10] == 234907000 ) &&
     ( gdtmpl[12] == 40000000 ) && ( gdtmpl[13] == 262000000 ) &&
     ( gdtmpl[14] == 12000000 ) && ( gdtmpl[15] == 12000000 ) )
     return ( 185 );

if ( ( gdtmpl[7] == 185 ) && ( gdtmpl[8] == 129 )  &&
     ( gdtmpl[9] == 12190000 ) && ( gdtmpl[10] == 226541000 ) &&
     ( gdtmpl[12] == 25000000 ) && ( gdtmpl[13] == 265000000 ) &&
     ( gdtmpl[14] == 12190000 ) && ( gdtmpl[15] == 12190000 ) )
     return ( 212 );

if ( ( gdtmpl[7] == 614 ) && ( gdtmpl[8] == 428 )  &&
     ( gdtmpl[9] == 12190000 ) && ( gdtmpl[10] == 226541000 ) &&
     ( gdtmpl[12] == 25000000 ) && ( gdtmpl[13] == 265000000 ) &&
     ( gdtmpl[14] == 12191000 ) && ( gdtmpl[15] == 12191000 ) )
     return ( 218 );*/

  loglev = 2;
  numerr = -1;
  sprintf(errgrp,"%s_GNUM\0",_proj);
  sprintf(errstr,"No GID KXKY [%d %d] LL [%f %f] ANGL [%f %f %f]\n",kx,ky,llt,lln, angl[0],angl[1], angl[2]);
  dc_wclg(loglev,errgrp,numerr,errstr,&ier);

return ( 255 );
}

int str_num ( g2int *gdtmpl, g2int gdtlen)
{
int kx, ky, try_quick, loglev, numerr, ier;
float angl[3], llt, lln, ult=RMISSD, uln=RMISSD;
static char _proj[] = "STR";
char errgrp[20], errstr[81];
nav_list *q;
static nav_list *lastmatch=NULL;

  if ( gdtlen < 18 ) return(255);

  kx = gdtmpl[7];
  ky = gdtmpl[8];

  llt = gdtmpl[9] / 1.0e6;
  lln = gdtmpl[10] / 1.0e6; while ( lln > 180.0 ) lln = lln - 360.0;

  if ( gdtmpl[16] & 64 )
    angl[0] = -90.0;
  else
    angl[0] = 90.0;
  angl[1] = gdtmpl[13] / 1.0e6; while ( angl[1] > 180.0 ) angl[1] = angl[1] - 360.0;
  angl[2] = 0;

  /*for ( i=0;i<gdtlen;i++)
        printf("gdtmpl %d %d\n",i,gdtmpl[i]);*/


  if ( lastmatch != NULL ) {
    q = lastmatch;
    try_quick = 1;
  }
  else {
    q = nav_head;
    try_quick = 0;
  }


  while ( q != NULL ) {
    if ( ( strcmp(_proj,q->proj ) == 0 ) &&
         ( kx == q->kx ) && ( ky == q->ky ) &&
         ( angl[0] == q->angle[0] ) && ( angl[1] == q->angle[1] ) &&
         ( G_DIFFT( llt, q->llt, .001) )  &&
         ( G_DIFFT( lln, q->lln, .001) ) ) {
         lastmatch = q;
         return(q->gnum);
    }
    if ( try_quick ) {
      q = nav_head;
      try_quick = 0;
    }
    else
      q = q->next;
  }


  loglev = 2;
  numerr = -1;
  sprintf(errgrp,"%s_GNUM\0",_proj);
  sprintf(errstr,"No GID KXKY [%d %d] LL [%f %f] ANGL [%f %f %f]\n",kx,ky,llt,lln, angl[0],angl[1], angl[2]);
  dc_wclg(loglev,errgrp,numerr,errstr,&ier);

/*
if ( ( gdtmpl[7] == 377 ) && ( gdtmpl[8] == 237 )  &&
     ( gdtmpl[9] == 44196000 ) && ( gdtmpl[10] == 174759000 ) &&
     ( gdtmpl[12] == 60000000 ) && ( gdtmpl[13] == 203000000 ) &&
     ( gdtmpl[14] == 11945000 ) && ( gdtmpl[15] == 11945000 ) )
     return ( 186 );

if ( ( gdtmpl[7] == 553 ) && ( gdtmpl[8] == 425 )  &&
     ( gdtmpl[9] == 30000000 ) && ( gdtmpl[10] == 187000000 ) &&
     ( gdtmpl[12] == 60000000 ) && ( gdtmpl[13] == 225000000 ) &&
     ( gdtmpl[14] == 11250000 ) && ( gdtmpl[15] == 11250000 ) )
     return ( 242 );*/

return ( 255 );
}

int decode_g2gnum ( gribfield *gfld )
{
int gnum=255;
static int _gnum_init = !0;

  if ( _gnum_init ) {
     grdnav_init();
     _gnum_init = 0;
  }



  switch ( gfld->igdtnum ) {
     case 0:
	gnum = ced_num ( gfld->igdtmpl, gfld->igdtlen);
	break;
     case 10:
	gnum = mer_num ( gfld->igdtmpl, gfld->igdtlen);
	break;
     case 20:
	gnum = str_num ( gfld->igdtmpl, gfld->igdtlen);
	break;
     case 30:
	gnum = lcc_num ( gfld->igdtmpl, gfld->igdtlen);
	break;
   }


  return gnum;

}

