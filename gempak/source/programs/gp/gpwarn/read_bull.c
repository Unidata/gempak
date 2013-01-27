#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <clocmn.h>

#include "geminc.h"
#include "gemprm.h"

#include "gpwarn.h"


#ifdef UNDERSCORE
#define gline		gline_
#define gfill		gfill_
#define gscolr		gscolr_
#define gsline		gsline_
#define ti_mdif		ti_mdif_

#define read_bull	read_bull_
#define	text_output	text_output_
#define gpwarn_color_init	gpwarn_color_init_
#endif

/* global time for plotting valid watches */
char valid[13];

char t_outstr[132];

int CLASS;
int PIL;

FILE *fp;

int PRIOR,FUTURE,ADVANCE;

int *outluns, *outnlun;

gpwarn_config_type gpwarn_config[NUMTYP];
 

/*****************************************************************************/
/* 20020912: function for initializing and reading gpwarn color/line attr.   */
/*  - takes array of gpwarn_config_type, sets defaults, and attempts to find */
/*  - and replace values from some gpwarn.config file.                       */
/*  - No return (void).                                                      */
void gpwarn_read_config(gpwarn_config_type *config)
{

   FILE		*ipf = NULL; /* FILE * for existing gpwarn.config reads  */
   char		*cpos = NULL; /* substring pointer */
   char		new_line[256]; /* each new line read from config file    */
   int		i, lbtid, iarr[10], inum, iret;

   static int	defcolrs[NUMTYP][10] = {
	{ 2, 5, 1, 2, 1, 13, 10, 1, 1, 1 },	/* TORNADO */
	{ 14, 5, 1, 2, 1, 13, 10, 1, 1, 1 },	/* SEVERE */
	{ 4, 5, 1, 2, 1, 26, 10, 1, 1, 1 },	/* WINTER */
	{ 6, 5, 1, 2, 1, 24, 26, 1, 1, 1 },	/* SPECIAL */
	{ 7, 5, 1, 2, 1, 29, 10, 1, 1, 1 },	/* NONPRCP */
	{ 21, 5, 1, 2, 1, 19, 18, 1, 1, 1 },	/* FWATCH */
	{ 23, 5, 1, 2, 1, 19, 18, 1, 1, 1 },	/* FWARN */
	{ 21, 5, 1, 2, 1, 19, 18, 1, 1, 1 },	/* FSTATE */
	{ 2, 2, 1, 2, 1, 13, 10, 1, 1, 1 },	/* HURRS */
	{ 5, 2, 1, 2, 1, 1, 1, 1, 1, 2 },	/* SLSTSTM */
	{ 13, 5, 1, 2, 1, 1, 1, 1, 1, 2 },	/* SLSTORN */
	{ 23, 5, 1, 2, 1, 1, 1, 1, 1, 2 }	/* SLSFFW */
	};

	/* 
	 * Set default colors 
	 */
   	for ( i = 0; i < NUMTYP; i++ ) {
	   config[i].current_fill_color = defcolrs[i][0];
	   config[i].current_line_color = defcolrs[i][1];
	   config[i].current_line_type = defcolrs[i][2];
	   config[i].current_line_width = defcolrs[i][3];
	   config[i].current_fill_type = defcolrs[i][4];
	   config[i].expired_fill_color = defcolrs[i][5];
	   config[i].expired_line_color = defcolrs[i][6];
	   config[i].expired_line_type = defcolrs[i][7];
	   config[i].expired_line_width = defcolrs[i][8];
	   config[i].expired_fill_type = defcolrs[i][9];
   	}



	/* 
	 * Open config file, if exists
	 */
	ipf = cfl_tbop ( "gpwarn.config", "unidata", &iret );
	if(iret != 0) return;


	/*
	 * Load config file values
	 */
	while ((fgets(new_line, (sizeof(new_line) - 1), ipf)) != NULL) {
		if (new_line[0] == '#') continue;

		if ((cpos = strchr(new_line, ':')) != NULL) {
			if(new_line[strlen(new_line) - 1] == '\n')
			   new_line[strlen(new_line) - 1] = '\0';
			cpos[0] = '\0';
			if(strcmp(new_line,"TORNADO") == 0)
				lbtid = TORNADO;
			else if (strcmp(new_line,"SEVERE") == 0)
				lbtid = SEVERE;
			else if (strcmp(new_line,"WINTER") == 0)
				lbtid = WINTER;
			else if (strcmp(new_line,"SPECIAL") == 0)
				lbtid = SPECIAL;
			else if (strcmp(new_line,"NONPRCP") == 0)
				lbtid = NONPRCP;
			else if (strcmp(new_line,"FWATCH") == 0)
				lbtid = FWATCH;
			else if (strcmp(new_line,"FWARN") == 0)
				lbtid = FWARN;
			else if (strcmp(new_line,"FSTATE") == 0)
				lbtid = FSTATE;
			else if (strcmp(new_line,"HURRS") == 0)
				lbtid = HURRS;
			else if (strcmp(new_line,"SLSTSTM") == 0)
				lbtid = SLSTSTM;
			else if (strcmp(new_line,"SLSTORN") == 0)
				lbtid = SLSTORN;
			else if (strcmp(new_line,"SLSFFW") == 0)
				lbtid = SLSFFW;
			else
				lbtid = -1;

			if ( lbtid != -1 ) {
			      cpos++;
			      cst_ilst ( cpos, ':', 1, 10, iarr, &inum, &iret);
	                      config[lbtid].current_fill_color = iarr[0];
	                      config[lbtid].current_line_color = iarr[1];
	                      config[lbtid].current_line_type = iarr[2];
	                      config[lbtid].current_line_width = iarr[3];
	                      config[lbtid].current_fill_type = iarr[4];
	                      config[lbtid].expired_fill_color = iarr[5];
	                      config[lbtid].expired_line_color = iarr[6];
	                      config[lbtid].expired_line_type = iarr[7];
	                      config[lbtid].expired_line_width = iarr[8];
	                      config[lbtid].expired_fill_type = iarr[9];
			}
				
		}
	}
	fclose(ipf);
	return;
}
/*****************************************************************************/

int is_valid(char *expires, int *isval)
/**********************************************************************
*  This subroutine determines the time difference between the bulletin
*  expiration time, and the plot valid time (current or user selected).
*  If the bulletin expiration time is not set, the subroutine returns
*  -1, otherwise the return value from ti_mdif is returned.
*  A time difference < 0 signifies the bulletin has expired.
***********************************************************************/
{
int iexpire[5],ivalid[5];
int ier,i,idif;

if(expires[0] == '\0') return(-1);

if(strcmp(expires,"DDHHMM") == 0) return(-1);

sscanf(valid,"%2d%2d%2d/%2d%2d",ivalid,ivalid+1,ivalid+2,ivalid+3,ivalid+4); 
iexpire[0] = ivalid[0];
iexpire[1] = ivalid[1];
ier = sscanf(expires,"%2d%2d%2d",iexpire+2,iexpire+3,iexpire+4);
if(ier < 3)
   {
   printf("Invalid expiration %s\n",expires);
   return(-1);
   }

if((iexpire[2] < 3)&&(ivalid[2] > 26))
   {
   iexpire[1] += 1;
   if(iexpire[1] > 12)
      {
      iexpire[1] = 1;
      iexpire[0] += 1;
      }
   }

if((ivalid[2] < 3)&&(iexpire[2] > 26))
   {
   iexpire[1] -= 1;
   if(iexpire[1] < 1)
      {
      iexpire[1] = 12;
      iexpire[0] -= 1;
      }
   }

ti_mdif(iexpire,ivalid,&idif,&ier);
*isval = idif;
return(ier);
}




void get_map(char *zone, char *expires)
/**********************************************************************
*  This subroutine draws the filled county/zone area for a given
*  identifier. The line and fill colors are predefined for several
*  known types, otherwise a default combination is used. Plotting
*  attributes are determined based on the expiration time of the
*  bulletin (Expired, Valid, Valid but outside disired plot window).
***********************************************************************/
{
int isval;
int i,TYPE;

int iret;

int FCOLOR, OCOLOR, LTYPE, LWIDTH, FTYPE;
int cur_fil, cur_lin, cur_ltype, cur_lwidth, cur_ftyp;
int exp_fil, exp_lin, exp_ltype, exp_lwidth, exp_ftyp;
static int oth_fil = 1, oth_lin = 30;


/* return if can't determine valid time */
if(is_valid(expires,&isval) == -1) return;

/* return if bulletin is expired, and outside plot range */
if((isval < 0)&&(-isval > PRIOR)) return;
if((isval > FUTURE)&&(ADVANCE == 1)) return;


/* determine plot color values */

switch(CLASS)
{
  case TORNADO:
  case SEVERE:
  case WINTER:
  case SPECIAL:
  case NONPRCP:
  case FWATCH:
  case FWARN:
  case FSTATE:
  case HURRS:
	{
	cur_fil = gpwarn_config[CLASS].current_fill_color;
	cur_lin = gpwarn_config[CLASS].current_line_color;
	cur_ltype = gpwarn_config[CLASS].current_line_type;
	cur_lwidth = gpwarn_config[CLASS].current_line_width;
	cur_ftyp = gpwarn_config[CLASS].current_fill_type;
	exp_fil = gpwarn_config[CLASS].expired_fill_color;
	exp_lin = gpwarn_config[CLASS].expired_line_color;
	exp_ltype = gpwarn_config[CLASS].expired_line_type;
	exp_lwidth = gpwarn_config[CLASS].expired_line_width;
	exp_ftyp = gpwarn_config[CLASS].expired_fill_type;
	break;
	}
  default:
	{
	cur_fil = 8;
	cur_lin = 5;
	cur_ltype = 1;
	cur_lwidth = 2;
	cur_ftyp = 1;
	exp_fil = 9;
	exp_lin = 10;
	exp_ltype = 1;
	exp_lwidth = 1;
	exp_ftyp = 1;
	}
}

if (isval < 0) {
   FCOLOR = exp_fil;
   OCOLOR = exp_lin;
   LTYPE = exp_ltype;
   LWIDTH = exp_lwidth;
   FTYPE = exp_ftyp;
}
else if (isval <= FUTURE) {
   FCOLOR = cur_fil;
   OCOLOR = cur_lin;
   LTYPE = cur_ltype;
   LWIDTH = cur_lwidth;
   FTYPE = cur_ftyp;
   }
else {
   FCOLOR = oth_fil;
   OCOLOR = oth_lin;
   LTYPE = 1;
   LWIDTH = 2;
   FTYPE = 1;
}

/* determine bulletin type */
if(zone[2] == 'C')
   TYPE = 0;
else
   if(zone[2] == 'Z')
      {
      TYPE = 1;
      }
   else
      TYPE = -1;


if(TYPE < 0)
   {
   printf("Error in zone string %s\n",zone);
   return;
   }

plot_ugc(TYPE, zone, FCOLOR, OCOLOR, LTYPE, LWIDTH, FTYPE);
}


void do_all(char *zone, int *nzones,zonelist **head)
{
zonelist *zone_list;
int iret,i,bindex;
char srchstr[30],*pos1,*pos2;
extern  CLO_t   clo;

/* read the zone table, if not already */
clo_init(&iret);

bindex = clo_which("ZONE_BNDS");
sprintf(srchstr,"CLASS=%c%c\0",zone[0],zone[1]);

/* find all zones in this state */
for(i=0;i<clo.loc[bindex].bnd.nbnd;i++)
   {
   if(strstr(clo.loc[bindex].bnd.bound[i].info,srchstr) != 0)
      {
      pos1 = strchr(clo.loc[bindex].bnd.bound[i].info,'=');
      zone_list = (zonelist *)malloc(sizeof(zonelist));
      zone_list->zone = (char *)malloc(7);
      strncpy(zone_list->zone,pos1+1,6);
      zone_list->zone[6] = '\0';
      zone_list->next = *head;
      *head = zone_list;
      *nzones = *nzones + 1;
      }
   }

}


void do_range(char *zline, char *lzone, int *cpos, int *nzones, zonelist **head)
/**********************************************************************
*  This routine creates a zone/county entry for each number in the 
*  range zone1>zone2.
***********************************************************************/
{
char zone[20];
int i,zstart,zstop;
zonelist *zone_list;
char *tpos;


if(zline[*cpos] != '>')
   {
   printf("error in zone range %d %s\n",cpos,zline);
   return;
   }

*cpos = *cpos + 1;
zone[0] = '\0';
strncat(zone,lzone,3);

tpos = zline + (*cpos);
if(strncmp(tpos,zone,3) == 0) /* the second number in range is ssZ### */
   *cpos = *cpos + 3;

i = 0;
while((zline[*cpos+i] >= '0')&&(zline[*cpos+i] <= '9'))
   {
   strncat(zone,zline+*cpos+i,1);
   i++;
   }
*cpos = *cpos + i;
sscanf(lzone+3,"%d",&zstart);
sscanf(zone+3,"%d",&zstop);
i = zstart+1;
while((i <= zstop)&&(i < 1000))
   {
   zone[0] = '\0';
   sprintf(zone,"%03d\0",i);
   lzone[3] = '\0';
   strncat(lzone,zone,strlen(zone));
   zone_list = (zonelist *)malloc(sizeof(zonelist));
   zone_list->zone = (char *)malloc( (strlen(lzone)+1)*sizeof(char) );
   strcpy(zone_list->zone,lzone);
   zone_list->next = *head;
   *head = zone_list;
   *nzones = *nzones + 1;
   i++;
   }
}


void get_zones(char *line)
/**********************************************************************
*  This subroutine determines the county/zone identifiers and expiration
*  time from the bulletin identification line, and calls the mapping
*  routine. A linked list is used to store the county/zone ids found
*  in the identifier line. Each county is plotted by traversing the
*  list.
***********************************************************************/
{
int nzones;
char *lpos,zone[20],lzone[20],*zline;
int cpos,llen,i,spos,zstart,zstop,line2;
int FOUND;

char expires[13];

zonelist *zone_list,*head;

lzone[0] = '\0';
expires[0] = '\0'; 
nzones = 0;

zone_list = head = NULL;

llen = strlen(line);
zline = (char *)malloc(llen+1);
zline[0] = '\0';

for(i=0;i<llen;i++)
   if(line[i] >= ' ') strncat(zline,line+i,1);

llen = strlen(zline);

sprintf(t_outstr,"\nzones %s\0",zline);
text_output(outnlun, outluns, t_outstr, strlen(t_outstr) );

cpos = 0; spos = 0; line2 = 0;
while((cpos < llen)||(line2 == 0))
   {
   if(cpos >= llen)
      {
      line2 = -1;
      if((expires[0] == '\0')&&(cpos-spos >= 6)) /* check for no '-' after expiration date */
         {
         FOUND = 0;
         for(i=spos;i<cpos;i++) if((zline[i] < '0')||(zline[i] > '9')) FOUND = 1;
         if(FOUND == 0) strncat(expires,zline+spos,12);
         if(FOUND == 0) printf("Assuming expires %s\n",expires);   
         }
      if(expires[0] == '\0')
         {
         if(cpos-spos > 0) 
            {
            /*printf("probably need to save leftovers %d %s\n",cpos-spos,zline+spos);*/
            strncpy(line,zline+spos,cpos-spos);
            lpos = line+cpos-spos;
            }
         else
            lpos = line;

         cpos = 0; spos = 0;
         if(fgets(lpos,255,fp) == NULL) continue;

         line2 = 0;
         llen = strlen(line);
         free(zline);
         zline = (char *)malloc(llen+1);
         zline[0] = '\0';

         for(i=0;i<llen;i++)
            if(line[i] >= ' ') strncat(zline,line+i,1);
         printf("  + %s\n",zline);
         }
      continue;
      }

   if((zline[cpos] == '-') || (zline[cpos] == '>'))
      {
      zone[0] = '\0';
      if((cpos - spos) < 20)
         strncat(zone,zline+spos,cpos-spos);
      else
         strncat(zone,zline+spos,19);

      if((zone[0] >= 'A')&&(zone[0] <= 'Z')&&(strcmp(zone,"DDHHMM") != 0))
         {
         if(strncmp(zone+2,"ZALL",4) == 0)
            do_all(zone,&nzones,&head);
         else
            {
            lzone[0] = '\0';
            strncat(lzone,zone,strlen(zone));
            zone_list = (zonelist *)malloc(sizeof(zonelist));
            zone_list->zone = (char *)malloc( (strlen(lzone)+1)*sizeof(char) );
            strcpy(zone_list->zone,lzone);
            zone_list->next = head;
            head = zone_list;
            nzones++;
            if(zline[cpos] == '>') 
               do_range(zline,lzone,&cpos,&nzones,&head);
            }
         }
      else
         if(strlen(zone) >= 6)
            {
            /* OK, too many rediculous reports where DDHHMM isn't filled in, 
            eg: zones LAZ024-026-MSZ059>067-072>076-078-079-DDHHMM  or blank!
            We'll catch it in is_valid()!!!*/
            if(strcmp(zone,"DDHHMM") == 0) printf("Invalid expiration time %s\n",zline);
            if((zone[0] < '0')||(zone[0] > '9'))
               {
               printf("Not a valid expiration time\n");
               line2 = -1;
               }
            else
               strncat(expires,zone,12);
            }
         else
            {
            if((lzone[0] != '\0')&&(zone[0] != '\0'))
               {
               lzone[3] = '\0';
               strncat(lzone,zone,strlen(zone));
               zone_list = (zonelist *)malloc(sizeof(zonelist));
               zone_list->zone = (char *)malloc( (strlen(lzone)+1)*sizeof(char) );
               strcpy(zone_list->zone,lzone);
               zone_list->next = head;
               head = zone_list;
               nzones++;
               if(zline[cpos] == '>')
                  do_range(zline,lzone,&cpos,&nzones,&head);
               }
            else
               if(zone[0] != '\0') printf("Improper zone %s\n",zone);
            }

      cpos = cpos + 1;
      while((zline[cpos] == ' ')&&(cpos < llen)) cpos += 1;
      spos = cpos;
      }
   else
      cpos = cpos + 1;
   }

/*if(expires[0] == '\0') printf("No expiration time\n");*/

zone_list = head;
while(head != NULL)
   {
   get_map(head->zone,expires);
   zone_list = head;
   head = head->next;
   free(zone_list->zone);
   free(zone_list);
   }

free(zline);

}

void get_valid(char *wws)
/**********************************************************************
*  This routine returns the time for the plot. Current time is determined
*  from the system time, otherwise the user must provide the yymmdd/hhmm
*  time for the plot.
***********************************************************************/
{
int istarr[5],irtarr[5];
int ier, itype=1;
char sysdt[13];

if((wws[0] >= '0')&&(wws[0] <= '9'))
   { /* assume this will be a GEMPAK time */
   valid[0] = '\0';
   strncat(valid,wws,12);
   }
else
   { /* use the current system clock */
   css_gtim( &itype, valid, &ier);
   }

}


int check_bullet(char *line, int state)
{

if((line[0] < 'A')||(line[0] > 'Z')) return(0); /* first 2 letters must be state */
if((line[1] < 'A')||(line[1] > 'Z')) return(0); /* first 2 letters must be state */
/* allow for STZALL or STZ###-etc */
if(strncmp(line+3,"ALL",3) == 0) 
   {
   if((line[6] == '-')||(line[6] == '>')) return(1);
   }
if((line[3] < '0')||(line[3] > '9')) return(0); /* next 3 letters are fips number */
if((line[4] < '0')||(line[4] > '9')) return(0); /* next 3 letters are fips number */
if((line[5] < '0')||(line[5] > '9')) return(0); /* next 3 letters are fips number */
if((line[6] == '-')||(line[6] == '>')) return(1);
if(state == 2) printf("is this a fips %s?\n",line);

return(0);
}

void templregex (char *instr, char *outstr)
{
int ier;
cst_rpst(instr, "YYYY", "[0-9][0-9][0-9][0-9]", outstr, &ier);
cst_rpst(outstr, "YY", "[0-9][0-9]", outstr, &ier);
cst_rpst(outstr, "MMM", "[A-Za-z][A-Za-z][A-Za-z]", outstr, &ier);
cst_rpst(outstr, "MM", "[01][0-9]", outstr, &ier);
cst_rpst(outstr, "DD", "[0-3][0-9]", outstr, &ier);
cst_rpst(outstr, "HH", "[0-2][0-9]", outstr, &ier);
cst_rpst(outstr, "NN", "[0-6][0-9]", outstr, &ier);
cst_rpst(outstr, "DWK", "[A-Za-z][A-Za-z][A-Za-z]", outstr, &ier);
cst_rpst(outstr, "FFF", "[0-9][0-9][0-9]", outstr, &ier);
cst_rpst(outstr, "FF", "[0-9][0-9]", outstr, &ier);
}

void warn_search()
{
#define MAXSZ	4096
char line[MAXSZ];
char bulletin[81];
int DONE;
int STATE;
int i;
char *cpos,strpart[80];


DONE = 0;

STATE = 0;
while((DONE == 0)&&(fgets(line,255,fp)!=NULL))
   {
   if((STATE > 0)&&(strchr(line,'\001') != 0))
      {
      if(STATE == 2) printf(": %s does not conform\n",bulletin);
      STATE = 0;
      }
   if((STATE == 0) &&
      ((strstr(line,"WUUS") != 0)||(strstr(line,"WFUS") != 0)||
       (strstr(line,"WWUS") != 0)||(strstr(line,"RWUS") != 0)||
       (strstr(line,"WRUS") != 0)||(strstr(line,"WGUS") != 0)||
       (strstr(line,"WMUS") != 0) ))
      {
      CLASS = -1; PIL = -1;
      /* set some basic classes in case PIL is missing */
      if(strstr(line,"WFUS") != 0) CLASS = TORNADO; 
      if(strstr(line,"WUUS") != 0) CLASS = SEVERE; 
      if(strstr(line,"WWUS4") != 0) CLASS = WINTER; 
      if(strstr(line,"WWUS3") != 0) CLASS = SPECIAL; 
      if(strstr(line,"WWUS8") != 0) CLASS = SPECIAL; 
      if(strstr(line,"WWUS7") != 0) CLASS = NONPRCP; 
      if(strstr(line,"RWUS") != 0) CLASS = FWATCH;
      if(strstr(line,"WRUS") != 0) CLASS = FWARN;
      if(strstr(line,"WGUS") != 0) CLASS = FSTATE;
      if(strstr(line,"WWUS31") != 0) CLASS = HURRS;
      STATE = 1;
      bulletin[0] = '\0';
      for(i=0;i<strlen(line);i++)
         if(line[i] >= ' ') strncat(bulletin,line+i,1);
      sprintf(t_outstr,"\n\nbulletin %s \0",bulletin);
      text_output(outnlun, outluns, t_outstr, strlen(t_outstr) );
      
      continue;
      }
   switch(STATE)
      {
      case 1:
         if(line[0] != '\0')
            {
            if(strncmp(line,"WSW",3) == 0) PIL = WINTER;
            if(strncmp(line,"NPW",3) == 0) PIL = NONPRCP;
            if(strncmp(line,"SPS",3) == 0) PIL = SPECIAL;
            if(strncmp(line,"SVR",3) == 0) PIL = SEVERE;
            if(strncmp(line,"TOR",3) == 0) PIL = TORNADO;
            if(strncmp(line,"FLW",3) == 0) PIL = FWARN;
            if(strncmp(line,"FFW",3) == 0) PIL = FWARN;
            if(strncmp(line,"FLA",3) == 0) PIL = FWATCH;
            if(strncmp(line,"FFA",3) == 0) PIL = FWATCH;
            if((PIL != -1)&&(PIL != CLASS))
               CLASS = PIL;
            }
         STATE = 2;
         break;
      case 2:
      case 4:
         if(strlen(line) < 7) continue; /* look for a FIPS line */
         if(((line[2] == 'C')||(line[2] == 'Z'))&&(check_bullet(line,STATE) == 1))
            {
            get_zones(line);
            STATE = 4;
            }
         break;
      }
   
   
   }

}

void gpwarn_color_init()
{
/* read configuration table */
memset((void *)gpwarn_config, 0, sizeof(gpwarn_config));
gpwarn_read_config(gpwarn_config);
}

void read_bull(char *wwtmpl, char *wws, char *wwatt, int *nlun, int *luns, int *ier)
/**********************************************************************
*  This routine is the driver routine. It should be called with the
*  information for the bulletin file and plot attributes. After
*  a valid bulletin is found, the first line of the county/zone
*  identifier is passed to the plotting routine. The file handle for
*  The bulletin file is stored in a global variable in case a second
*  line is used - which must be read if the expiration time is not
*  found on the first line.
***********************************************************************/
{

#define MAXSZ	4096
#define MAXTMPL	100
char line[MAXSZ];
char bulletin[81];
int DONE;
int STATE;
int i;
int iret;
char *cpos,strpart[80];

char path[132],tmplt[132], newtmplt[160], defstr[] = " ";
char filnam[512],*postok;
int tplate, icat, iscat, ifrm, irng, iint, plens, tlens, llen, numstr, numscd;
int maxline=MAXSZ, order=-1;

static char defdir[]="$TEXT_DATA/watch_warn";
static char **arrptr;
static int init=-1, nstrings = 10;

*ier = 0;

if(init == -1)
   {
   init = 0;
   arrptr = (char **) malloc(sizeof(char *) * nstrings);
   for(i=0; i < nstrings; i++)
      arrptr[i] = (char *) malloc(MAXTMPL);
   }

outluns = luns;
outnlun = nlun;

get_valid(wws);

if(wwatt != NULL)
   {
   i = 0;
   cpos = strchr(wwatt,';');
   if(cpos == NULL) cpos = (char *)(wwatt + strlen(wwatt));
   strpart[0] = '\0';
   strncat(strpart,wwatt+i,cpos - wwatt);
   iret = sscanf(strpart,"%d/%d",&PRIOR,&FUTURE);
   if(iret < 2)
      {
      if(iret == 1) 
         FUTURE = PRIOR;
      else
         PRIOR = FUTURE = 1440;
      }
   ADVANCE = 0;
   if(FUTURE < 0)
      {
      FUTURE = -FUTURE;
      ADVANCE = 1;
      }
   
   i = (cpos - wwatt) + 1;
   }



if(strlen(wwtmpl) > 0)
   {
   cst_clst(wwtmpl, ';', defstr,nstrings,MAXTMPL,arrptr, &numstr, &iret);
   for(i=0;i<numstr;i++)
      {
      int ier1, ier2;

      ctb_dtpath ( arrptr[i], path, &ier1 );
      ctb_dttmpl ( arrptr[i], tmplt, &ier2 );

      if ( ( ier1 != 0 ) || ( ier2 != 0 ) )
         {
         fp = cfl_ropn(arrptr[i],defdir,&iret);
         if(iret != 0)
            {
            printf("error opening %s\n",arrptr[i]);
            *ier = -1;
            return;
            }
         else
	    {
            warn_search();
            fclose(fp);
	    }
         }
      else
         {
	 plens = strlen(path);
         templregex ( tmplt, newtmplt ); tlens = strlen(newtmplt);
         cfl_scnd (path, &plens, newtmplt, &tlens, ";", &maxline, &order, line, &llen, &numscd, &iret);

         postok = strtok(line,";");
         while(postok != NULL)
            {
            sprintf(filnam,"%s/%s\0",path,postok);
	    sprintf(t_outstr, "searching filnam %s\0",filnam);
            text_output(outnlun, outluns, t_outstr, strlen(t_outstr) );
	    if((fp = cfl_ropn ( filnam, NULL, &iret)) != NULL)
	       {
               warn_search();
	       fclose(fp);
	       }
	    postok = strtok(NULL,";");
            }
         }
      }
   }
else
   *ier = -1;



}
