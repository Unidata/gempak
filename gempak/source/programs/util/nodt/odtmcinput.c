#include "inc/odt.h"
#define NADDARG 320

int  getuserinputs(void);
void stradd(char getstr[], char addstr[], int *last, int len1, int len2);
void mctokn(char *);
void filset(void);
void setfil(void);
char *ckwp(char *, int, char *);
int  ikwp(char *, int, int);

extern int narg; extern char **varg;
extern logical odt,olist,ograph,odel,owind,oautomode,ofirst48,override;
extern logical odump,spotanal,oremote,ographtemp;
extern int     drawCI,drawTW,drawTR,igraph,iword;
extern int     ot1,ot2,idomain,ifixtype,larea;
extern char    hfile[200],listfile[200],fixfile[200],*od1,*od2;
extern char    iout[200],lserver[100];

/* These routines are various McIDAS routines to read and access 
   command line inputs.  
*/

int getuserinputs(void)
/* Obtain user inputs from command line using McIDAS routines.
    Inputs  : none
    Outputs : various global variables
*/
{
  char *ccgraph,*cclist,*ccdel,*ccwind,*ccdomain;
  char *ccauto,*ccfirst48,*ccoverride,*ccdmp,*ccrem;
  char *hfilex,*fixfilex,*listfilex,*lserverx;
  
  /* set defaults for various global variables */
  odt=TRUE;
  ograph=FALSE;
  olist=FALSE;
  odel=FALSE;
  owind=FALSE;
  oautomode=FALSE;
  ofirst48=TRUE; 
  override=FALSE;
  oremote=FALSE;
  spotanal=FALSE;
  od1="\0";
  od2="\0";

  /* call to filset is needed for toks.c to function correctly */
  filset();

  /* obtain history file name and path */
  hfilex=ckwp("HISTORY",1,"ODTDUMP.ODT");
  if(strncmp(hfilex,"ODTDUMP.ODT",11)==0) spotanal=TRUE;
  strncpy (hfile,HISTORYPATH,strlen(HISTORYPATH));
  strncat (hfile,hfilex,strlen(hfilex));

  /* check for REMOTE data keyword */
  ccrem=ckwp("REMOTE",1,"NO");
  if(strncmp(ccrem,"YES",3)==0) {
    oremote=TRUE; 
    lserverx=ckwp("REMOTE",2,LOCALSERVER);  /* local server */
    strncpy(lserver,lserverx,strlen(lserverx));
    larea=ikwp("REMOTE",3,LOCALAREA);    /* local image number */
  }

  /* check for GRAPH output keyword */
  ccgraph=ckwp("GRAPH",1,"NO");
  if(strncmp(ccgraph,"NO",2)!=0) {
    odt=FALSE;
    ograph=TRUE;
    ographtemp=FALSE;
    if(strncmp(ccgraph,"TEM",3)==0) {
      ographtemp=TRUE;
    }
    drawCI=ikwp("PLOT",1,3);  /* CI value color */
    drawTW=ikwp("PLOT",2,4);  /* Final T# value color */
    drawTR=ikwp("PLOT",3,5);  /* Raw T# value color */
    igraph=ikwp("PLOT",4,2);  /* graph boundary color */
    iword =ikwp("PLOT",5,7);  /* graph wording color */
  }

  /* check for LIST output keyword */
  cclist=ckwp("LIST",1,"NO");
  if(strncmp(cclist,"YES",3)==0) {
    odt=FALSE;
    olist=TRUE;
    ccdmp=ckwp("OUTPUT",1,"SCREEN");
    if(strncmp(ccdmp,"FILE",4)==0) {
      listfilex=ckwp("OUTPUT",2,"ODTDUMP.ASC");
      strncpy(listfile,listfilex,strlen(listfilex));
      odump=TRUE;
    }
  }

  /* check for DELETE keyword */
  ccdel=ckwp("DELETE",1,"NO");
  if(strncmp(ccdel,"YES",3)==0) {
    odt=FALSE; 
    odel=TRUE;
  }

  if(odel) {
    /* DATE parameters for DELETE option */
    od1=ckwp("DATE",1,"0000XXX00");
    ot1=ikwp("DATE",2,-1);
    od2=ckwp("DATE",3,od1);
    ot2=ikwp("DATE",4,ot1);
  } else {
    /* DATE parameters for GRAPH/LIST options */
    od1=ckwp("DATE",1,"0000XXX00");
    ot1=ikwp("DATE",2,-1);
    od2=ckwp("DATE",3,"0000XXX00");
    ot2=ikwp("DATE",4,-1);
  }

  /* check for WIND keyword */
  ccwind=ckwp("WIND",1,"NO");
  if(strncmp(ccwind,"YES",3)==0) {
    owind=TRUE;
  }

  /* check for DOMAIN keyword */
  idomain=0;
  ccdomain=ckwp("DOMAIN",1,"ATL");
  if(strncmp(ccdomain,"PAC",3)==0) idomain=1;

  /* check for AUTO keyword */
  ccauto=ckwp("AUTO",1,"NO");
  if(strncmp(ccauto,"YES",3)==0) {
    ifixtype=ikwp("AUTO",2,0);
    oautomode=TRUE;
    fixfilex=ckwp("AUTO",3,"AUTOFIX");
    strncpy(fixfile,fixfilex,strlen(fixfilex));
  }

  /* check for RULE48 keyword */
  ccfirst48=ckwp("RULE48",1,"ON");
  if(strncmp(ccfirst48,"OFF",3)==0) ofirst48=FALSE;

  /* check for OVERRIDE keyword */
  ccoverride=ckwp("OVER",1,"NO");
  if(strncmp(ccoverride,"YES",3)==0) override=TRUE;
  
  return 0;
}

void filset(void)
{
int i=0,j=0;
char addarg[NADDARG];
addarg[0] = '\0' ;
/* for (i=1; i<narg; i++) {stradd(varg[i],addarg,&j,48,NADDARG);} */
for (i=1; i<narg; i++) {stradd(varg[i],addarg,&j,200,NADDARG);}
mctokn(addarg);
setfil();
}
void setfil (void)
{
char *cfile, *cprint, cline[151], wline[1000];
int i, scmp, lcline,lwline=0; 
FILE *fp;
cfile = ckwp("TXT",1,"   ");
scmp = strncmp(cfile,"   ",3);
if (scmp != 0)
{ fp = fopen(cfile,"r");
  while (fgets(cline,140,fp) != NULL)
   {lcline = strlen (cline);
    if (cline[lcline-1] == '\x0a') {cline[lcline-1] = ' '; cline[lcline] = '\0';}
    if (lwline+lcline >= 1000-1) { /*printf("lwline exceeded\n")*/; break;}
    for (i=0; i<lcline; i++) {wline[lwline+i] = cline[i];}
    lwline = lwline + lcline;
   }
  wline[lwline] = '\0';
  mctokn(wline);
  fclose (fp); }
cprint = ckwp("PRT",1,"   ");
scmp = strncmp(cprint,"   ",3);
if (scmp != 0)
 {/*printf("write to file for stdout: %s\n",cprint);*/
  fp = freopen(cprint,"w",stdout);
 }
cprint = ckwp("PRT",2,"   ");
scmp = strncmp(cprint,"   ",3);
if (scmp != 0)
 {/*printf("write to file for stderr: %s\n",cprint);*/
  fp = freopen(cprint,"w",stderr);
 }
}

void stradd (char getstr[], char addstr[], int *last, int len1, int len2)
{
int i,j ;

i = *last ; j = 0 ;
while (getstr[j] != '\0'  && j < (len1-1) && i < (len2-3))
   { addstr[i++] = getstr[j++] ;}
addstr[i++] = ' ' ; addstr[i] = '\0' ;
*last = i ;
}
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
/*struct {int ntok; int nkw; int narr[64]; int ideval;} PARCTL;*/
#define STRNLEN 64
#define NUMTOKS 128
#define PARMLEN 256
struct {char string[STRNLEN];
        int  loctok[NUMTOKS];
        int  lentok[NUMTOKS];
        int  typtok[NUMTOKS];
        int  nkwpar[NUMTOKS];
        int  ntok;
        char parms[PARMLEN];} parstok;
void mctokn (char *);
void tokanl ( int, int *, int *);
char * ckwp(char *, int , char *);

void mctokn (char *ctext)
{
int lctext;  /* length of string ctext */
int ic;      /* index for ctext */
int ct = -1;  /* start of current token */
int cn = -1;  /* length of current token 0 based */
int ntok = -1; /* number of tokens encountered 0 based */
int ltok = -1; /* last key word token encountered (ntok number */
int slen = -1; /* length of string */
int sloc = -1; /* location of string */
int npp = -1;  /* number of positional parameters */
int nkp = -1;  /* number of key word parameters */
int nkw = -1;  /* number of key words */
char c;     /* current character */
int i, j;      /* indexing */
lctext = strlen (ctext);
if (ctext[lctext-1] == '\x0a') {lctext--; ctext[lctext] = '\0';}
for (i=0; i<NUMTOKS; i++)
  {parstok.nkwpar[i] = 0;}
for (ic=0; ic<lctext; ic++)
 {c = ctext[ic]; if (ic < PARMLEN-1) {parstok.parms[ic] = c;parstok.parms[ic+1] = '\0';}
  if (sloc >= 0)
   {cn++; slen++;
    if (c == '}' || c == '\0')
     {   /* printf ("pp: npp %d ntok %d ct %d cn %d\n",npp,ntok,ct,cn); */
      for (i=0,j=ct+1; i<=cn-2&&i<STRNLEN; i++,j++) {parstok.string[i] = ctext[j];}
      parstok.string[STRNLEN-1] = '\0';
      if (ntok < NUMTOKS) {parstok.loctok[ntok] = ct+1;
                           parstok.lentok[ntok] = cn-1;
                           parstok.typtok[ntok] = 4   ;}
     }
   }
  else if (c == ' ')
   {if (ntok == -1) {continue;}    /* skip initial blanks */
    if (cn   == -1) {continue;}    /* skip multiple blanks between tokens */
    if (nkp <  0)      /* found a positional parameter */
     {npp++;
      if (ntok < NUMTOKS) {parstok.loctok[ntok] = ct;
                           parstok.lentok[ntok] = cn+1;
                           parstok.typtok[ntok] = 1   ;}
     }
    else
     {nkp++;  /* found a key word   parameter */
      if (ntok < NUMTOKS) {parstok.loctok[ntok] = ct;
                           parstok.lentok[ntok] = cn+1;
                           parstok.typtok[ntok] = 3   ;
                           parstok.nkwpar[ltok]++     ;}
     }
    ct = -1; cn = -1;     /* reset for new token */
   }
  else if (cn == -1)
   {ntok++; cn++; ct = ic;  /* new token  check for '{' */
    if (c == '{') {sloc = ic; slen++;}
   }
  else if (c == '=' || c == ',')     /* end of keyword token */
   {nkp++; nkw++; cn++;
    if (ntok < NUMTOKS) {parstok.loctok[ntok] = ct;
                         parstok.lentok[ntok] = cn;
                         parstok.typtok[ntok] = 2 ;
                         parstok.nkwpar[ntok] = 0 ;
                         ltok = ntok; }
    cn = -1; ct = -1;    /* reset for new token */
   }
  else {cn++;} /* continuation of token */
 }        /* end of for loop on ic */
 ntok++; parstok.ntok = ntok;  /* change from 0 start to 1 start index */
}

char * ckwp(char *ckw, int itn, char *cdeflt)
{
int i, j, slen, loc, len;
int ntok = parstok.ntok; /* for convience- shorter */
char *p = NULL;
   /*  executables */
slen = strlen(ckw);
for (i=0; i<ntok; i++)
 {if (parstok.typtok[i] != 2) {continue;}
  loc = parstok.loctok[i];
  for (j=0; j<slen; j++)
   {if (parstok.parms[loc+j] != ckw[j]) {goto nextok;} }
    if (itn > parstok.nkwpar[i]) goto out;
    len = parstok.lentok[i+itn];
    loc = parstok.loctok[i+itn];
    p = (char *)malloc(len+1);
    if (p != NULL) {strncpy(p,&parstok.parms[loc],len);
    p[len] = '\0';
         return p;}
 nextok: ;
 }
out:
p = (char *)malloc(strlen(cdeflt)+1);
if (p != NULL) {strcpy(p,cdeflt);}
return p;
}

int ikwp(char *ckw, int itn, int deflt)
{
int i, j, slen, loc, v;
int ntok = parstok.ntok; /* for convience- shorter */
   /*  executables */
slen = strlen(ckw);
for (i=0; i<ntok; i++)
 {if (parstok.typtok[i] != 2) {continue;}
  loc = parstok.loctok[i];
  for (j=0; j<slen; j++)
   {if (parstok.parms[loc+j] != ckw[j]) {goto nextok;} }
    if (itn > parstok.nkwpar[i]) goto out;
    /* len = parstok.lentok[i+itn]; not used */
    loc = parstok.loctok[i+itn];
    if (parstok.parms[loc] == 'X') goto out;  /* allow default */
    sscanf (&parstok.parms[loc],"%d",&v);
    return v;
 nextok: ;
 }
out:
return deflt;
}

int ipp(int itn, int deflt)
{
int i, loc, v;
int nt = 0;  /* number of positional parameters encountered */
int ntok = parstok.ntok; /* for convience- shorter */
   /*  executables */
for (i=0; i<ntok; i++)
 {if (parstok.typtok[i] != 1) {continue;}
  nt++; if (nt < itn) {continue;}
  loc = parstok.loctok[i];
  /* len = parstok.lentok[i]; not used */
  if (parstok.parms[loc] == 'X') goto out;  /* allow default */
  sscanf (&parstok.parms[loc],"%d",&v);
  return v;
 }
out:
return deflt;
}

double dpp(int itn, double deflt)
{
 double v;
 int i, loc;
 int nt = 0;  /* number of positional parameters encountered */
 int ntok = parstok.ntok; /* for convience- shorter */
   /*  executables */
for (i=0; i<ntok; i++)
 {if (parstok.typtok[i] != 1) {continue;}
  nt++; if (nt < itn) {continue;}
  loc = parstok.loctok[i];
  if (parstok.parms[loc] == 'X') goto out;  /* allow default */
  sscanf (&parstok.parms[loc],"%lf",&v);
  return v;
 }
out:
return deflt;
}

char *cpp(int itn, char *deflt)
{
int i, loc, len;
int nt = 0;  /* number of positional parameters encountered */
int ntok = parstok.ntok; /* for convience- shorter */
char *p = NULL;
   /*  executables */
for (i=0; i<ntok; i++)
 {if (parstok.typtok[i] != 1) {continue;}
  nt++; if (nt < itn) {continue;}
  loc = parstok.loctok[i];
  len = parstok.lentok[i];
  if (parstok.parms[loc] == 'X') goto out;  /* allow default */
  p = (char *)malloc(len+1);
    if (p != NULL) {strncpy(p,&parstok.parms[loc],len);
     *(p+len) = '\0'; return p;}
 }
out:
p = (char *)malloc(strlen(deflt)+1);
if (p != NULL) {strcpy(p,deflt);}
return p;
}

int cqfld (char **cbuf, int *len)
{int i;
for (i=0; i<parstok.ntok; i++)
 {if (parstok.typtok[i] != 4) {continue;}
  /* cbuf = &parstok.string[0]; */
  *cbuf = parstok.string;
  /* cbuf = (char *)parstok.loctok[i];*/
  *len = parstok.lentok[i];
  return 1;
 }
return 0;
}

double dkwp(char *ckw, int itn, double deflt)
{
double v;
int i, j, slen, loc;
int ntok = parstok.ntok; /* for convience- shorter */
   /*  executables */
slen = strlen(ckw);
for (i=0; i<ntok; i++)
 {if (parstok.typtok[i] != 2) {continue;}
  loc = parstok.loctok[i];
  for (j=0; j<slen; j++)
   {if (parstok.parms[loc+j] != ckw[j]) {goto nextok;} }
    if (itn > parstok.nkwpar[i]) goto out;
    /* len = parstok.lentok[i+itn]; not used */
    loc = parstok.loctok[i+itn];
    if (parstok.parms[loc] == 'X') goto out;  /* allow default */
    sscanf (&parstok.parms[loc],"%lf",&v);
    return v;
 nextok: ;
 }
out:
return deflt;
}

int nkwp(char *ckw)
{
int i, j, slen, loc;
int ntok = parstok.ntok; /* for convience- shorter */
   /*  executables */
slen = strlen(ckw);
for (i=0; i<ntok; i++)
 {if (parstok.typtok[i] != 2) {continue;}
  loc = parstok.loctok[i];
  for (j=0; j<slen; j++)
   {if (parstok.parms[loc+j] != ckw[j]) {goto nextok;} }
    return parstok.nkwpar[i];
 nextok: ;
 }
return 0;
}
