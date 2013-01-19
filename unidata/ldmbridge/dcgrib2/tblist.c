#include <geminc.h>
#include <gemprm.h>

#include <dccmn.h>
#include <na.h>

#include "dcgrib.h"

typedef struct glist {
	int center;
	int subcenter;
	int gridid;
	int modelid;
	char filnam[512];
	int maxgrids;
	struct glist *next;
	} glist;

struct glist *head=NULL;

#define MAXMODS 10
#define NUM_FIELDS 6
#define MAXSTR 132

void tbglist()
/************************************************************************
 * TBGLIST                                                              *
 *                                                                      *
 * This routine reads the gribkey.tbl file and creates a structured	*
 * list of the model center, id fields in order to determine the file	*
 * name template for a given GRIB.					*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * Chiz/Unidata         11/99   Created for dcgrib2			*
 ***********************************************************************/
{
FILE *fp;
int i, iret, ier, numstr;
char bufr[256];
static char **arrptr;
char *centerid, *subc, *modelid, *gridid, *flnam, *maxgds;
int subcenter;
int modlist[MAXMODS], nmod;
static char tfile[]="gribkey.tbl";
static char fcat[]="grid";
struct glist *p, *tail;
static int isinit=1;

if (isinit)
   {
   isinit = 0;
   arrptr = (char **) malloc(sizeof(char *) * NUM_FIELDS);
   for(i=0; i < NUM_FIELDS; i++)
      arrptr[i] = (char *) malloc(MAXSTR);
   centerid = arrptr[0];
   subc = arrptr[1];
   modelid = arrptr[2];
   gridid = arrptr[3];
   flnam = arrptr[4];
   maxgds = arrptr[5];
   }

fp = cfl_tbop ( tfile, fcat, &iret);

while(iret == 0)
   {
   cfl_trln ( fp, sizeof(bufr), bufr, &iret);
   if(iret == 0)
      {
      cst_clst ( bufr, ' ', " ", NUM_FIELDS, MAXSTR, arrptr, &numstr, &ier);

      ier = sscanf(subc,"%d",&subcenter);
      if(ier != 1) subcenter = -1;

      cst_ilst ( modelid, ',', -1, MAXMODS, modlist, &nmod, &ier);
      if(ier != 0)
         {
         nmod = 1;
         modlist[0] = -1;
         }
      for(i=0;i<nmod;i++)
         {
         p = (struct glist*)malloc(sizeof(struct glist));
         sscanf(centerid,"%d",&(p->center));
         p->subcenter = subcenter;
         p->modelid = modlist[i];
         ier = sscanf(gridid,"%d",&(p->gridid));
         if(ier < 1) p->gridid = -1;
         strcpy(p->filnam,flnam);
         sscanf(maxgds,"%d",&(p->maxgrids));
         p->next = NULL;

         if(head == NULL)
            {
            head = p;
            tail = p;
            }
         else
            {
            tail->next = p;
            tail = tail->next;
            }

         }
      }
   }


#ifdef DEBUG
p = head;
while(p != NULL)
   {
   printf("GRIBKEY: %d %d %d %d %s %d\n",
	p->center,p->subcenter,p->modelid,p->gridid,p->filnam,p->maxgrids);
   p = p->next;
   }
#endif


cfl_clos ( fp, &iret);

}

void tbtmpl( int center, int subcenter, int model, int grid, char *filnam,
		int *maxgrids, int *iret)
/************************************************************************
 * TBTMPL                                                               *
 *                                                                      *
 * This routine finds the file name template and maximun number of	*
 * grids for a file, given the identifying PDS values for a GRIB.	*
 * TBGLIST must have already been called.				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * Chiz/Unidata         11/99   Created for dcgrib2                     *
 ***********************************************************************/
{
static int lcntr,lmodl,lgrid;
struct glist *p;
static struct glist *last=NULL;

*iret = -1;
filnam[0] = '\0';

if(last != NULL)
   {
   /* see if we already have the grid information */
   if((lcntr == center)&&(lmodl == model)&&(lgrid == grid))
      { 
      *maxgrids = last->maxgrids;
      strcpy(filnam,last->filnam); *iret = 0;
      return;
      }
   }

p = head;
while(p != NULL)
   {
   if((p->center == center)&&((p->modelid == model)||(p->modelid == -1)))
      {
      if(((p->gridid == -1)||(p->gridid == grid))&&
	 ((p->subcenter == -1)||(p->subcenter == subcenter)))
         {
         last = p; lcntr = center; lmodl = model; lgrid = grid;
         *maxgrids = p->maxgrids;
         strcpy(filnam,p->filnam); *iret = 0;
         return;
         }
      }
   p = p->next;
   }

}


typedef struct subclist {
	int subcenter;
	char *scabb;
	struct subclist *next;
	} subclist;

typedef struct subhead {
	int center;
	struct subclist *top;
	struct subhead *next;
	} subhead;



char *tbsubcenter( int *center, int *subc)
/************************************************************************
 * TBSUBCENTER                                                          *
 *                                                                      *
 * This routine returns the subcenter template for a model center and 	*
 * subcenter pair.							*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * Chiz/Unidata         11/99   Created for dcgrib2                     *
 ***********************************************************************/
{
int iret,inum;
FILE *fp;
char tnam[80],*cpos,line[256],subst[80],errstr[128];
static int lcen=-9999,lsub=-9999;
static char lastsub[80];

static struct subhead *head=NULL;
struct subhead *p;
struct subclist *q;

if((lcen == *center)&&(lsub == *subc))
   return(lastsub);

p = head;
while(p != NULL)
   {
   if(p->center == *center)
      break;
   else
      p = p->next;
   }

if (p == NULL)
   {
   p = (struct subhead *)malloc(sizeof(struct subhead));
   p->center = *center;
   p->next = head;
   p->top = NULL;
   head = p;

   /* 
    * get center's subcenter file name 
    */   

   if ( ( *center > 0 ) && ( *center < 256 ) )
      {
      sprintf(tnam, "%ssubcenters.tbl\0",_nacent.mcabb[*center - 1]);
      fp = cfl_tbop ( tnam, "grid", &iret);
      }
   else
      fp = NULL;

   if(fp != NULL)
      {
      sprintf(errstr,"Opened subcenter file %s\0",tnam);
      dc_wclg (1, "DCGRIB", 0, errstr, &iret);
      while(fgets(line,255,fp) != NULL)
         if(line[0] != '!')
            {
	    if(line[strlen(line)-1] == '\n') line[strlen(line)-1] = '\0';
	    cpos = cst_split(line,' ', 79, subst, &iret);
            if(sscanf(subst,"%d",&inum) == 1)
               {
	       subst[0] = '\0';
               cpos = cst_split(cpos,' ',79,subst,&iret);
	       q = (struct subclist *)malloc(sizeof(struct subclist));
	       q->subcenter = inum;
	       q->scabb = (char *)malloc(strlen(subst)+1);
	       cst_uclc(subst,q->scabb,&iret);
	       q->next = p->top;
	       p->top = q;
               }
            }
      cfl_clos ( fp, &iret );
      }
   else
      {
      sprintf(errstr,"Failed to open subcenter file %s\0",tnam);
      dc_wclg (0, "DCGRIB", -70, errstr, &iret);
      }
   }

/* see if we can find val */

q = p->top;
while (q != NULL)
   {
   if(q->subcenter == *subc)
      {
      lcen = *center; lsub = *subc;
      strcpy(lastsub,q->scabb);
      return (q->scabb);
      }
   q = q->next;
   }

lcen = *center;
lsub = *subc;
sprintf(lastsub,"subc%03d\0",*subc);
return (lastsub);

}
