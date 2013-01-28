/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"
#define NADDARG 320
#define STRNLEN 64
#define NUMTOKS 128
#define PARMLEN 256

int mcidas_getpaths(char *,char *,char *,char *,char *,char *); 
int mcidas_setpaths(char *,char *,char *,char *,char *,char *); 
int mcidas_setgraphoptions(int,int,int,int,int,int,int,int,int,int,int,int);
int mcidas_getinputs(int, char **,
                    logical *,logical *,logical *,logical *,logical *,logical *,
                    logical *,logical *,logical *,logical *,logical *,
                    logical *,logical *,logical *,logical *,
                    int *,int *,int *,int *,int *,int *,
                    int *,int *,int *,int *,int *,int *,int *,int *,
                    int *,int *,
                    float *,
                    char *,char *,char *,char *,char *,char *,char *,char *,char *,
                    char *,char *,char *); 

void mcidas_stradd(char *getstr, char *addstr, int *last, int len1, int len2); 
void mcidas_mctokn(char *);
void mcidas_filset();
void mcidas_setfil();
char *mcidas_ckwp(char *, int, char *);
int  mcidas_ikwp(char *, int, int);
double mcidas_dkwp(char *, int, double);

int narg; char **varg;
/* global variables for McIDAS routines */
extern char *topopath,*histpath,*sstpath,*listpath,*autopath,*datapath; 
int drawCI,drawCIadj,drawTW,drawTN,drawTR,drawTRO,drawTIEa,drawTIEr,igraph,iword;
logical ographtemp,owind;

/* These routines are various McIDAS routines to read and access 
   command line inputs.  
*/
int mcidas_getpaths(char *hpath,char *tpath,char *spath,char *lpath,char *apath,char *dpath) 
{
  char *dpathx; 

  /* obtain history file path name from ODTHISTORY env variable */
  if(getenv("ODTHISTORY")==NULL) {
    strncpy(hpath,getenv("HOME"),strlen(getenv("HOME")));
  } else {
    strncpy(hpath,getenv("ODTHISTORY"),strlen(getenv("ODTHISTORY")));
  }
  if(hpath[strlen(hpath)-1]!='/') {
    strncat(hpath,"/",1);
  }

  /* obtain topography file path name from ODTTOPO env variable */
  if(getenv("ODTTOPO")==NULL) {
    strncpy(tpath,getenv("HOME"),strlen(getenv("HOME")));
  } else {
    strncpy(tpath,getenv("ODTTOPO"),strlen(getenv("ODTTOPO")));
  }
  if(tpath[strlen(tpath)-1]!='/') {
    strncat(tpath,"/",1);
  }

  /* obtain sea surface temperature path name from ODTSST env variable */
  if(getenv("ODTSST")==NULL) {
    strncpy(spath,getenv("HOME"),strlen(getenv("HOME")));
  } else {
    strncpy(spath,getenv("ODTSST"),strlen(getenv("ODTSST")));
  }
  if(spath[strlen(spath)-1]!='/') {
    strncat(spath,"/",1);
  }

  /* obtain listing output file path name from ODTOUTPUT env variable */
  if(getenv("ODTOUTPUT")==NULL) {
    strncpy(lpath,getenv("HOME"),strlen(getenv("HOME")));
  } else {
    strncpy(lpath,getenv("ODTOUTPUT"),strlen(getenv("ODTOUTPUT")));
  }
  if(lpath[strlen(lpath)-1]!='/') {
    strncat(lpath,"/",1);
  }

  /* obtain forecast file path name from ODTTOPO env variable */
  if(getenv("ODTAUTO")==NULL) {
    strncpy(apath,getenv("HOME"),strlen(getenv("HOME")));
  } else {
    strncpy(apath,getenv("ODTAUTO"),strlen(getenv("ODTAUTO")));
  }
  if(apath[strlen(apath)-1]!='/') {
    strncat(apath,"/",1);
  }

  dpathx=(char *)calloc((size_t)200,(size_t)sizeof(char)); 
  /* obtain satellite image file path name using McIDAS command  */
  if(getenv("MCPATH")==NULL) {
    strncpy(dpathx,getenv("HOME"),strlen(getenv("HOME")));
  } else {
    strncpy(dpathx,getenv("MCPATH"),strlen(getenv("MCPATH")));
  }
  strncpy(dpath,dpathx,((int)strchr(dpathx,':')-(int)dpathx));
  if(dpath[strlen(dpath)-1]!='/') {
    strncat(dpath,"/",1);
  }
  free(dpathx);
  dpathx=NULL;

  return 0;

}

int mcidas_setpaths(char *hpath,char *tpath,char *spath,char *lpath,char *apath,char *dpath) 
{

  strcpy(topopath,tpath); 
  topopath[strlen(tpath)]='\0'; 
  strcpy(histpath,hpath); 
  histpath[strlen(hpath)]='\0'; 
  strcpy(sstpath,spath); 
  sstpath[strlen(spath)]='\0'; 
  strcpy(listpath,lpath); 
  listpath[strlen(lpath)]='\0'; 
  strcpy(autopath,apath); 
  autopath[strlen(apath)]='\0'; 
  strcpy(datapath,dpath); 
  datapath[strlen(dpath)]='\0'; 

  return 0;
}

int mcidas_setgraphoptions(int ci,int ciadj,int tf,int tr,int tro,int tfn,int tier,int tiea,int graph,int word,
                           int grapht,int wind)
{
  drawCI=ci; 
  drawCIadj=ciadj;
  drawTW=tf;
  drawTN=tfn;
  drawTR=tr;
  drawTRO=tro;
  drawTIEa=tier;
  drawTIEr=tiea;
  igraph=graph;
  iword=word;
  ographtemp=grapht;
  owind=wind;
  
  return 0;
}

int mcidas_getinputs(int nnarg, char **vvarg,
                    logical *odt,logical *ograph,logical *olist,logical *odel,logical *oland,logical *owind,
                    logical *override,logical *oautomode,logical *ographtemp,logical *odump,logical *ostartstr,
                    logical *oremote,logical *osearch,logical *ocommadd,logical *oatcf,
                    int *ot1,int *ot2,int *indomain,int *ifixtype,int *larea,int *listtype,
                    int *drawCI,int *drawCIadj,int *drawTW,int *drawTR,int *drawTRO,int *drawTN,int *drawTIEr,int *drawTIEa,
                    int *igraph,int *iword,
                    float *osstr,
                    char *hfile,char *listfile,char *fixfile,char *lserver,char *topofile,
                    char *sstfile,char *atcfpath,char *atcftype,char *atcfsrcID,
                    char *comment,char *od1,char *od2)
/* Obtain user inputs from command line using McIDAS routines.
    Inputs  : none
    Outputs : various user command line input variables
    Return  : 121 : OVER and AUTO both set, ignoring OVER keyword
                0 : o.k.
*/
{
  char *ccgraph,*cclist,*ccdel,*ccwind,*ccdomain,*catcf,*catcfsrc,*catcfout;
  char *ccauto,*ccoverride,*ccdmp,*ccrem,*ccland,*ccsearch,*cccommadd;
  char *fixfilex,*listfilex,*lserverx,*sstfilex;
  char *hfilex; 
  char *ptr,*tmpfile,*od1x,*od2x,*commentx;
  int  iok,ixx,iyy,len,found=0,iret=0;
  int  stormID,atcff1,atcff2;
  
  varg=vvarg;
  narg=nnarg;
  /* set defaults for various global variables */
  *odt=TRUE;
  *oland=TRUE;
  *osearch=TRUE;
  *ograph=FALSE;
  *olist=FALSE;
  *odel=FALSE;
  *owind=FALSE;
  *oatcf=FALSE;
  *oautomode=FALSE;
  *override=FALSE;
  *ostartstr=FALSE;
  *oremote=FALSE;
  *ographtemp=FALSE;
  *odump=FALSE;
  *ocommadd=FALSE;

  /* call to filset is needed for toks.c to function correctly */
  mcidas_filset();

  /* determine topography file name and path */
  strcpy(topofile,topopath); 
  strncat(topofile,"TOPOLRES",8);

  /* obtain history file name and path */
  /* will truncate file name to 12 characters total...
     eight for prefix name, with suffix added of ".ODT" */
  tmpfile=mcidas_ckwp("HISTORY",1,"");
  if(strlen(tmpfile)>0) {
    /* append *.ODT suffix, if necessary */
    ptr=tmpfile;
    ixx=A_MIN(8,strlen(tmpfile));
    iyy=0;
    while((!found)&&(iyy<ixx)) {
      if(!strncmp(ptr,".",1)) {
        found=1;
        if(!strncmp(ptr,".ODT",4)) found=2;
      } else {
        ++ptr;
        iyy++;
      }
    }
    hfilex=(char *)calloc((size_t)13,(size_t)sizeof(char));
    if(found==2) {
      strcpy(hfilex,tmpfile); 
      hfilex[strlen(tmpfile)]='\0';
    } else {
      strcpy(hfilex,tmpfile); 
      hfilex[iyy]='\0';
      strncat(hfilex,".ODT",4);
    }
    strcpy(hfile,histpath); 
    strncat(hfile,hfilex,strlen(hfilex));
    free(hfilex);
    hfilex=NULL;
  }

  /* determine SST file name and path */
  sstfilex=mcidas_ckwp("SST",1,"SST.DAT");
  strcpy(sstfile,sstpath); 
  strncat(sstfile,sstfilex,strlen(sstfilex));

  /* check for REMOTE data keyword */
  ccrem=mcidas_ckwp("REMOTE",1,"NO");
  *larea=0000;
  if(strncmp(ccrem,"YES",3)==0) {
    *oremote=TRUE; 
    if(getenv("ODTLSERVER")==NULL) {
      lserverx=mcidas_ckwp("REMOTE",2,"LOCAL");  /* local server */
    } else {
      lserverx=mcidas_ckwp("REMOTE",2,getenv("ODTLSERVER"));  /* local server */
    }
    strncpy(lserver,lserverx,strlen(lserverx));
    if(getenv("ODTLAREA")==NULL) {
      *larea=mcidas_ikwp("REMOTE",3,9999);    /* local image number */
    } else {
      *larea=mcidas_ikwp("REMOTE",3,atoi(getenv("ODTLAREA")));    /* local image number */
    }
  }

  /* check for GRAPH output keyword */
  ccgraph=mcidas_ckwp("GRAPH",1,"NO");
  if(strncmp(ccgraph,"NO",2)!=0) {
    *odt=FALSE;
    *ograph=TRUE;
    *olist=FALSE;
    *odel=FALSE;
    *ographtemp=FALSE;
    *owind=FALSE;
    if(strncmp(ccgraph,"TEM",3)==0) {
      *ographtemp=TRUE;
    }
    if(strncmp(ccgraph,"WIN",3)==0) {
      *owind=TRUE;
    }
    *drawCI=mcidas_ikwp("PLOT",1,1);  /* CI value color */
    *drawCIadj=mcidas_ikwp("PLOT",2,2);  /* Adjusted MSLP value color */
    *drawTW=mcidas_ikwp("PLOT",3,0);  /* Final T# value color */
    *drawTN=mcidas_ikwp("PLOT",4,0);  /* Final T# (3-hr) value color */
    *drawTR=mcidas_ikwp("PLOT",5,0);  /* Adjusted Raw T# value color */
    *drawTRO=mcidas_ikwp("PLOT",6,0);  /* Original Raw T# value color */
    *drawTIEa=mcidas_ikwp("PLOT",7,6); /* TIE Model value color - averaged value */
    *drawTIEr=mcidas_ikwp("PLOT",8,0); /* TIE Model value color - raw value */
    *igraph=mcidas_ikwp("PLOT",9,6);  /* graph boundary color */
    *iword =mcidas_ikwp("PLOT",10,7);  /* graph wording color */
  }

  /* check for ATCF output keyword */
  stormID=mcidas_ikwp("ATCF",1,-1);
  *listtype=stormID;
  if(stormID!=-1) {
    *oatcf=TRUE;
    catcfsrc=mcidas_ckwp("ATCF",2,"XXXX");         /* source ID... such as NHC,PGTW,etc. */
    strncat(atcfsrcID,catcfsrc,strlen(catcfsrc));
    atcff1=(mcidas_ikwp("ATCF",3,0)==0) ? 0 : 1;   /* raw T# output flag - 0=adjRawT#,1=RawT# */
    atcff2=(mcidas_ikwp("ATCF",4,0)==0) ? 0 : 1;   /* final T# output flag - 0=FinalT#,1=3hrFinalT# */
    *listtype=(atcff1*1000)+(atcff2*100)+stormID;  /* + value indicates storm number with various ATCF output flags */
    strcpy(atcfpath,listpath); 
    atcfpath[strlen(listpath)]='\0'; 
  }

  /* check for LIST output keyword */
  cclist=mcidas_ckwp("LIST",1,"NO");
  if(strncmp(cclist,"YES",3)==0) {
    *olist=TRUE;
    *ograph=FALSE;
    *odel=FALSE;
    *odt=FALSE;
    ccdmp=mcidas_ckwp("OUTPUT",1,"SCREEN");
    if(strncmp(ccdmp,"FILE",4)==0) {
      listfilex=mcidas_ckwp("OUTPUT",2,"ODTDUMP.ASC");
      strcpy(listfile,listpath); 
      strncat(listfile,listfilex,strlen(listfilex));
      *odump=TRUE;
    }
  }

  /* check for DELETE keyword */
  ccdel=mcidas_ckwp("DELETE",1,"NO");
  if(strncmp(ccdel,"YES",3)==0) {
    *odt=FALSE; 
    *odel=TRUE;
    *olist=FALSE;
    *ograph=FALSE;
  }

  od1x=mcidas_ckwp("DATE",1,"0000XXX00");
  *ot1=mcidas_ikwp("DATE",2,-1);
  if(*odel) {
    od2x=mcidas_ckwp("DATE",3,od1x);
    *ot2=mcidas_ikwp("DATE",4,*ot1);
  } else {
    od2x=mcidas_ckwp("DATE",3,"0000XXX00");
    *ot2=mcidas_ikwp("DATE",4,-1);
  }
  strcpy(od1,od1x); 
  od1[strlen(od1x)]='\0';
  strcpy(od2,od2x); 
  od2[strlen(od2x)]='\0';

  /* check for DOMAIN keyword */
  *indomain=0;
  ccdomain=mcidas_ckwp("DOMAIN",1,"XXX");
  if(strncmp(ccdomain,"ATL",3)==0) *indomain=1;
  if(strncmp(ccdomain,"PAC",3)==0) *indomain=2;

  /* check for AUTO keyword */
  ccauto=mcidas_ckwp("AUTO",1,"NO");
  if(strncmp(ccauto,"YES",3)==0) {
    *oautomode=TRUE;
    *ifixtype=mcidas_ikwp("AUTO",2,0);
    fixfilex=mcidas_ckwp("AUTO",3,"AUTOFIX");
    catcf=mcidas_ckwp("AUTO",4,"OFCL");
    strcpy(fixfile,autopath); 
    strncat(fixfile,fixfilex,strlen(fixfilex));
    strncat(atcftype,catcf,strlen(catcf));
  }

  /* check for OVERRIDE keyword */
  ccoverride=mcidas_ckwp("OVER",1,"NO");
  if(strncmp(ccoverride,"YES",3)==0) *override=TRUE;
  
  /* check for INITIAL CLASSIFICATION keyword */
  *osstr=(float)mcidas_dkwp("IC",1,1.0);
  if(*osstr>=1.0) *ostartstr=TRUE;

  /* check for LAND INTERACTION OVERRIDE keyword */
  ccland=mcidas_ckwp("LAND",1,"YES");
  if(strncmp(ccland,"NO",2)==0) *oland=FALSE;

  /* check for SEARCH keyword */
  ccsearch=mcidas_ckwp("SEARCH",1,"YES");
  if(strncmp(ccsearch,"NO",2)==0) {
    *osearch=FALSE;
  }

  /* check for COMMENT keyword */
  ccsearch=mcidas_ckwp("COMM",1,"NO");
  if(strncmp(ccsearch,"YES",3)==0) {
    *ocommadd=TRUE;
    *odt=FALSE; 
    *odel=FALSE;
    *olist=FALSE;
    *ograph=FALSE;
    iok=mcidas_cqfld(&commentx,&len);
    ixx=A_MIN(49,strlen(commentx));
    strcpy(comment,commentx);
    comment[ixx]='\0';
  }
  

  /* if OVER and AUTO keywords are both TRUE, disallow OVERRIDE
     functionality and set return value for output message */
  if((*override==TRUE)&&(*oautomode==TRUE)) {
    *override=FALSE;
    iret=121;
  }
  
  return iret;
}

void mcidas_filset(void)
{
int i=0,j=0;
char addarg[NADDARG];
addarg[0] = '\0' ;
/* for (i=1; i<narg; i++) {mcidas_stradd(varg[i],addarg,&j,48,NADDARG);} */
for (i=1; i<narg; i++) {mcidas_stradd(varg[i],addarg,&j,200,NADDARG);}
mcidas_mctokn(addarg);
mcidas_setfil();
}
void mcidas_setfil ()
{
char *cfile, *cprint, cline[151], wline[1000];
int i, scmp, lcline,lwline=0; 
FILE *fp;
cfile = mcidas_ckwp("TXT",1,"   ");
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
  mcidas_mctokn(wline);
  fclose (fp); }
cprint = mcidas_ckwp("PRT",1,"   ");
scmp = strncmp(cprint,"   ",3);
if (scmp != 0)
 {/*printf("write to file for stdout: %s\n",cprint);*/
  fp = freopen(cprint,"w",stdout);
 }
cprint = mcidas_ckwp("PRT",2,"   ");
scmp = strncmp(cprint,"   ",3);
if (scmp != 0)
 {/*printf("write to file for stderr: %s\n",cprint);*/
  fp = freopen(cprint,"w",stderr);
 }
}

void mcidas_stradd (char *getstr, char *addstr, int *last, int len1, int len2) 
{
int i,j ;

i = *last ; j = 0 ;
while (getstr[j] != '\0'  && j < (len1-1) && i < (len2-3))
   { addstr[i++] = getstr[j++] ;}
addstr[i++] = ' ' ; addstr[i] = '\0' ;
*last = i ;
}

struct {char string[STRNLEN];
        int  loctok[NUMTOKS];
        int  lentok[NUMTOKS];
        int  typtok[NUMTOKS];
        int  nkwpar[NUMTOKS];
        int  ntok;
        char parms[PARMLEN];} parstok;

void mcidas_mctokn (char *ctext)
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
     {for (i=0,j=ct+1; i<=cn-2&&i<STRNLEN; i++,j++) {parstok.string[i] = ctext[j];}
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

char *mcidas_ckwp(char *ckw, int itn, char *cdeflt)
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

int mcidas_ikwp(char *ckw, int itn, int deflt)
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

int mcidas_ipp(int itn, int deflt)
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

double mcidas_dpp(int itn, double deflt)
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

char *mcidas_cpp(int itn, char *deflt)
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

int mcidas_cqfld (char **cbuf, int *len)
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

double mcidas_dkwp(char *ckw, int itn, double deflt)
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

int mcidas_nkwp(char *ckw)
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
