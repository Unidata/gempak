#include "geminc.h"
#include "gemprm.h"

/***********************************************************************
 incpccftxt.c
 This module parses the text layout string specified in the		
 setting.tbl and the uattribd.tbl.  The text layout string consists	
 of a list of 3-4 letter mnemonic parameters delimited by either	
 ":"'s or ";"'s. ":"'s are used to delimit text to be placed on the	
 same line. ";"'s are used to delimit text to be placed on multiple	
 lines. The mnemonic parameters are then cross referenced to these	
 elements: tops, growth, probability, and coverage used to create
 the final text string to be used in the text labels.  Symbols can be
 referenced in the text layout string.  Symbol types include Special and
 Weather, denoted by SSYM and WSYM, respectively.  The text layout 
 string is specified in the variable "instr".  The parsed output string
 including symbol references is in "outstrfin".  Reference the CCF
 Proposal/Design Documents for complete details.
 
  CONTENTS:								
    global functions:							
      inc_pccftxt - parse the text layout string.			
    private functions							
      shave -  Trim off leading and trailing "|" and " " (spaces).	

**
 Log:
 L. Hinson/AWC     07/09  Created
                                   
***********************************************************************/

/* Private Functions */ 
static char *shave(char *str, const char *whitespace);

void inc_pccftxt(char *tops, char *growth, char *prob, char *coverage, 
                 char *instr, char *outstrfin, int *iret)
/***********************************************************************
 inc_pccftxt
 This routine parses the text layout string, specified in instr.
 Information used to create the parsed text layout string originates
 from the elements tops, growth, probability, and coverage.  Output in
 outstrfin may include references to symbols.
 
 Input parameters:
   *tops        char    Storm Top Values or Range of Values
   *growth      char    + (increasing), NC (No change), - (decreasing)
   *prob        char    Confidence Level Range in Percentages
   *coverage    char    Coverage Level Range in Percentages
   *instr       char    Text Layout String
 Output parameters:
   *outstrfin   char    Parsed Output Sgtfring.
   *iret        int     Return codes
                        0: normal return
**
 Log:
 L. Hinson      07/09   Created
************************************************************************/
{
  char carr[25][LLMXLN]; char *prmlst[25];
  char **ccfitem;
  char outstr[256];
  char tmpstr[256];
  int strsiz=256;
  char errstr[256];
  char topd[25];
  char *ptr;
  int nparm, numccf, i, j;
  Boolean prmfound;
  float gwtssize;
  int ltop, utop, lrange, urange;
  int lent;
  int ier,ier2;
  
  /*--------------------------------------------------------------------*/
  
  for (i = 0; i < 25; i++) prmlst[i] = carr[i]; 
  *iret = 0;
  strcpy(outstr," ");
  strcpy(outstrfin," ");
  cst_clst(instr, ';', "BLNK", 25, LLMXLN, prmlst, &nparm, &ier);
  if (ier != 0) {
    strcpy(errstr," "); 
    er_wmsg("CST", &ier, errstr, &ier2, 3, strlen(errstr));
    *iret = -19;
    strcpy(errstr,"inc_pccftxt:instr"); 
    er_wmsg("IN", iret, errstr, &ier2, 2, strlen(errstr));
    return;
  } 
  
  ccfitem = (char **) malloc (10 * sizeof(char *) );
  for ( i = 0; i < 10; i++)
    ccfitem[i] = (char *) malloc (strsiz * sizeof(char) );
  for (i = 0; i < nparm; i++) {
    cst_clst(prmlst[i], ':', "BLNK", 10, strsiz, ccfitem, &numccf, &ier);
    if (ier != 0) {
      strcpy(errstr, " ");
      er_wmsg("CST", &ier, errstr, &ier2, 3, strlen(errstr));
      *iret = -19;
      sprintf(errstr,"inc_pccftxt:prmlst[%d]",i);
      er_wmsg("IN", iret, errstr, &ier2, 2, strlen(errstr));
      for (i = 0; i < 10; i++) free(ccfitem[i]);
      free(ccfitem);
      return;
    }
    prmfound = False;
    for (j = 0; j < numccf; j++) {
      if (strcmp(ccfitem[j],"CVG") == 0) {
        strcat(outstr, coverage);
        prmfound=True;
      }
      if (strcmp(ccfitem[j],"CONF") == 0) {
        sscanf(prob,"%d-%d",&lrange,&urange);
        if (urange == 100) {
          strcat(outstr, "HIGH");
        } else {
          strcat(outstr, "LOW");
        }
        prmfound=True;
      }
      if (strcmp(ccfitem[j],"PROB") == 0) {
        strcat(outstr,prob);
        prmfound=True;
      }
      if (strcmp(ccfitem[j],"ETR") == 0) {
        strcat(outstr, tops);
        prmfound=True;
      }
            
      if (strcmp(ccfitem[j],"ETD") == 0) {
        if (strstr(tops,"-")!=NULL) {
          sscanf(tops,"%d-%d",&ltop,&utop);
          sprintf(topd,"%03d",utop);
        } else if (strstr(tops,"+")!=NULL) {
          sscanf(tops,"%d",&utop);;
          sprintf(topd,">%03d",utop);
        } else {
          sprintf(topd,"UNK");
        }               
        strcat(outstr, topd);
        prmfound=True;
      }
      
      if (strcmp(ccfitem[j],"GWTH") == 0) {
        strcat(outstr, growth);
        prmfound=True;
      }
      
      if (strstr(ccfitem[j],"GWTS") != NULL) {
        if (strchr(ccfitem[j],'|') != NULL) {
          sscanf(ccfitem[j]+5,"%f",&gwtssize);
        } else {
          gwtssize = 1.00;
        }
	if ((ptr=strrchr(outstr,'|'))==NULL)
	  ptr=outstr;
	else
	  ptr=strrchr(outstr,'|')+1;
	lent = strlen(ptr);
	if (lent == 0) lent = 1;	  	
        if (strcmp(growth,"+")==0) {
          sprintf(tmpstr,".;SSYM:57/%4.2f/%d/1;",gwtssize,lent+1);
        } else if (strcmp(growth,"-")==0) {
          sprintf(tmpstr,".;SSYM:58/%4.2f/%d/1;",gwtssize,lent+1);
        } else {
          tmpstr[0]='\0';
        }
        strcat(outstr, tmpstr);
        prmfound=True;
      }
      if (strstr(ccfitem[j],"TEXT") != NULL) {
        if (strchr(ccfitem[j],'|') != NULL) {
          sscanf(ccfitem[j]+5,"%s",tmpstr);
          strcat(outstr, tmpstr);
          prmfound=True;
        }
      }
      if (strstr(ccfitem[j],"SSYM") != NULL ||
          strstr(ccfitem[j],"WSYM") != NULL ||
          strstr(ccfitem[j],"IBDR") != NULL) {
          if (strchr(ccfitem[j],'|') !=NULL) {
            *(strchr(ccfitem[j],'|'))=':';            
          }
          strcat(outstr, ccfitem[j]);
          prmfound=True;
      }

      if (strcmp(ccfitem[j],"NIL")==0) {
        strcat(outstr,"NIL");
        prmfound=True;
      }
      if (strcmp(ccfitem[j],"BLNK")==0) {
        strcat(outstr," ");
        prmfound=True;
      }
      if (! prmfound) {
        *iret = -11; /* Parameter is invalid */
        sprintf(errstr,"inc_pccftxt:%s",ccfitem[j]);
        er_wmsg("IN", iret, errstr, &ier, 2, strlen(errstr));
      }
      
    }
    strcat(outstr,"|");
  }
  outstr[strlen(outstr)-1]='\0';
  strcpy(outstrfin, shave(outstr, " |"));
  
  for (i = 0; i < 10; i++)
    free(ccfitem[i]);
  free(ccfitem);
  *iret = 0;
}

/*=====================================================================*/

static char *shave(char *str, const char *whitespace)
/************************************************************************
 *  shave  (From ne_shave in Linux)					*
 *  									*
 *  This routine trims out leading and trailing whitespace characters	*
 *  listed in the variable str.						*
 *  									*
 *  Input/Output parameters:						*
 *  *str        char   Input String					*
 *  Input parameter:							*
 *  *whitespace char   Characters to trim out				*
 *  									*
 *  Returns:								*
 *    Pointer to modified string					*
 *    									*
 **									*
 *  Log:								*
 *  L. Hinson		12/06	Created					*
 ***********************************************************************/
{   
    char *pnt, *ret = str;

/*---------------------------------------------------------------------*/

    while (*ret != '\0' && strchr(whitespace, *ret) != NULL) {
        ret++;
    }

    /* pnt points at the NUL terminator. */
    pnt = &ret[strlen(ret)];

    while (pnt > ret && strchr(whitespace, *(pnt-1)) != NULL) {
        pnt--;
    }

    *pnt = '\0';
    return ret;
}
        
