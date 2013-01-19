#include "geminc.h"
#include "gemprm.h"

/************************************************************************
 * incpgfatxt.c								*
 *									*
 * This module parses the text layout string specified in the		*
 * setting.tbl and the uattribd.tbl.  The text layout string consists	*
 * of a list of 3-4 letter mnemonic parameters delimited by either	*
 * ":"'s or ";"'s. ":"'s are used to delimit text to be placed on the	*
 * same line. ";"'s are used to delimit text to be placed on multiple	*
 * lines. The mnemonic parameters are then cross referenced to these	*
 * elements: status, hazard, tag, forecast hour, dueto (weather), top,	*
 * base, fzltop, fzlbase, and level used to create the final text	*
 * string to be used in the text labels. Symbols can also be referenced	*
 * in the text layout string.  The limitation is that they only be	*
 * referenced once per line, but can be mixed within multiple lines.	*
 * Symbol types include Special, Weather, Turbulence, and Icing,	*
 * denoted by SSYM, WSYM, TSYM, ISYM, respectively.  The text layout	*
 * string is specified in the variable "instr".  The parsed output	*
 * string including symbol references is in "outstrfin". Reference	*
 * the GFA Text Layout Design Document for complete details.		*
 *									*
 * CONTENTS:								*
 *   global functions:							*
 *     inc_pgfatxt - parse the text layout string.			*
 *   private functions							*
 *     shave -  Trim off leading and trailing "|" and " " (spaces).	*
 *     wrapbydelim - Wrap the Weather Text with using the specified	*
 *                   length, and 2 delimiters. (see function details)	*
 **									*
 * Log:									*
 * L. Hinson/AWC	12/06	Created					*
 * B. Yin/SAIC          09/07   Changed MT_OBSC label comparison        *
 * L. Hinson/AWC        11/07   Add "IBDR" mnemonic to TLAYOUT string   *
 * L. Hinson/AWC        12/07   Add "BGFN" mnemonic to TLAYOUT string   *
 * J. Wu/SAIC           06/08   Added "ITS" for MTW intensity           *
 * J. Wu/SAIC           06/08   Added "CVR" and LYR for CLD             *
 * J. Wu/SAIC           06/08   add "CTG", "FRQ" and GR for TS          *
 * B. Yin/SAIC          06/08   Added top/base, coverage for IFR/MVFR   *
 * L. Hinson/AWC        05/09   Added "BFHR", "EFHR" mnemonics          *
 * L. Hinson/AWC        06/09   Remove "OCNL" for MTN OBSC to fix DUE TO*
 *                              wording issue on Labels                 *
 ***********************************************************************/
/* Private Functions */ 
static char *shave(char *str, const char *whitespace);


void inc_pgfatxt(char *status, char *hazard, char *tag, char *cycle, char *fcsthr, 
	   char *dueto, char *top, char *base, char *fzltop,
           char *fzlbse, char *level, char *intensity, char *coverage,
	   char *category, char *frequency, 
	   char *instr, char *outstrfin,
           int *iret)
/************************************************************************
 * inc_pgfatxt 								*
 * This routine parses the text layout string, specified in instr.	*
 * Information used to create the parsed text layout string originates	*
 * from the elements status, hazard, tag, forecast hour, dueto,		*
 * top, base, fzltop, fzlbse, and level.   Output in outstrfin		*
 * may include references to symbols.					*
 * 									*
 * Input parameters:							*
 *   *status     char    Status of NRML, AMD, COR, CNCL, NEW.		*
 *   *hazard     char    Hazard of IFR, MT_OBSC, ICE, FZLVL, M_FZLVL	*
 *                       TURB-HI, TURB-LO, SFC_WND, LLWS		*
 *   *tag        char    Tag Examples: 1W, 2W, 3C, 1E			*
 *   *cycle      char    Cycle Hour: 00, 03, 15, 21
 *   *fcsthr     char    Forecast Hour: 0-6, 0, 3, 6, 9, 12, 3:15	*
 *   *dueto      char    Variable providing hazard details, or WX.	*
 *                       Example on IFR:  CIG BLW 010/ VIS BLW 3SM	*
 *                       PCPN/BR/FG					*
 *   *top        char    Tops used in ICE, Turbulence, Multi-Fzlvl haz.	*
 *   *base       char    Bases in ICE, Turb, Multi-Fzlvl haz.		*
 *   *fzltop     char    Freezing Level Top, used in ICE hazard.	*
 *   *fzlbse     char    Freezing Level Base, used in ICE hazard.	*
 *   *level      char    Used in Freezing Levels and SFC Freezing Levels*
 *   *intensity  char    Intensity used in MTW (MOD)                    *
 *   *coverage   char    Sky coverage for CLD                           *
 *   *category   char    Category for TS (EMBD or OBSC, or blank)       *
 *   *frequency  char    Frequency for TS (OSOL or OCNL )               *
 *   *instr      char    Text Layout String				*
 * 									*
 * Output parameters:							*
 *   *outstrfin  char    Parsed Output String.				*
 *   *iret       int     Return codes					*
 *                       0: normal return				*
 **									*
 * Log:									*
 * L. Hinson		12/06	Created					*
 * L. Hinson             6/07   Added SFHR mnemonic                     *
 * B. Yin/SAIC          09/07   Changed MT_OBSC label comparison        *
 * L. Hinson/AWC        11/07   Add "IBDR" mnemonic to TLAYOUT string   *
 * L. Hinson/AWC        12/07   Add "BGFN" mnemonic to TLAYOUT string   *
 * J. Wu/SAIC           06/08   Added "ITS" for MTW intensity           *
 * J. Wu/SAIC           06/08   Added "CVR" and LYR for CLD             *
 * J. Wu/SAIC           06/08   add "CTG", "FRQ" and GR for TS          *
 * B. Yin/SAIC          06/08   Added top/base, coverage for IFR/MVFR   *
 * L. Hinson/AWC        05/08   Add cycle parameter.                    *
 * L. Hinson/AWC        05/08   Add "ZFHR" & "ZSFH" mnemonic to TLAYOUT *
 *                              string                                  *
 * L. Hinson/AWC        05/09   Add "BFHR" & "EFHR mnemonic to TLAYOUT  *
 *                              string                                  *
 * L. Hinson/AWC        06/09   Remove "OCNL" for MTN OBSC to fix DUE TO*
 *                              wording issue on Labels                 *
 * L. Hinson/AWC        11/09   Add "DueTo" assignment on CLD_TOPS      *
 ***********************************************************************/
{    
  char carr[25][LLMXLN]; char *prmlst[25];
  char **gfaitem;
  char outstr[256];
  int strsiz = 256;
  char errstr[256];
  char work[256], work2[256];
  int nparm, numgfa, i, j, lenlimit;
  Boolean prmfound;
  int hour, minute;
  char sptime[6],regtime[6];
  char fcsthrwrk[7];
  char *ptr;
  int zhour;
  int ier,ier2;

/*---------------------------------------------------------------------*/

  for (i = 0; i < 25; i++) prmlst[i] = carr[i]; 
  *iret = 0;
  strcpy(outstr," ");
  strcpy(outstrfin," ");
  cst_clst(instr, ';', "BLNK", 25, LLMXLN, prmlst, &nparm, &ier);
  if (ier != 0) {
    strcpy(errstr," "); 
    er_wmsg("CST", &ier, errstr, &ier2, 3, strlen(errstr));
    *iret = -19;
    strcpy(errstr,"inc_pgfatxt:instr"); 
    er_wmsg("IN", iret, errstr, &ier2, 2, strlen(errstr));
    return;
  }
  
  gfaitem = (char **) malloc (10 * sizeof(char *) );
  for (i = 0; i < 10; i++)
    gfaitem[i] = (char *) malloc (strsiz * sizeof(char) );
  for (i = 0; i < nparm; i++) {
    work[0]='\0';
    cst_clst(prmlst[i], ':', "BLNK", 10, strsiz, gfaitem, &numgfa, &ier);
    if (ier != 0) {
      strcpy(errstr, " ");
      er_wmsg("CST", &ier, errstr, &ier2, 3, strlen(errstr));
      *iret = -19;
      sprintf(errstr,"inc_pgfatxt:prmlst[%d]",i);
      er_wmsg("IN", iret, errstr, &ier2, 2, strlen(errstr));
      for (i = 0; i < 10; i++) free(gfaitem[i]);
      free(gfaitem);
      return;
    }
    prmfound=False;
    for (j = 0; j < numgfa; j++) {
      if (strcmp(gfaitem[j],"STA") == 0) {
        if (strcmp(status,"NRML") != 0) {
          strcat(outstr,status);
        }
        prmfound=True;
      }
      if (strcmp(gfaitem[j],"SFHR") == 0) {
        if (strstr(fcsthr,":")!=NULL) {
          sscanf(fcsthr,"%d:%d",&hour,&minute);
        } else {
          sscanf(fcsthr,"%d",&hour);
          minute = 0;
        }
        if (!(hour % 3 == 0 && minute == 0)) { /* Is this a special? */
          sprintf(sptime,"%d:%02d",hour,minute);
          strcat(outstr,sptime);
        }
        prmfound=True;
      }
      if (strcmp(gfaitem[j],"ZSFH") == 0) {
        if (strstr(fcsthr,":")!=NULL) {
          sscanf(fcsthr,"%d:%d",&hour,&minute);
        } else {
          sscanf(fcsthr,"%d",&hour);
          minute = 0;
        }
        zhour = (hour + atoi(cycle)) % 24;
        if (!(hour % 3 == 0 && minute == 0)) { /* Is this a special? */
          sprintf(sptime,"%d:%02d",zhour,minute);
          strcat(outstr,sptime);
        }
        prmfound=True;
      
      }
      if (strcmp(gfaitem[j],"ZFHR") == 0) {
        if (strstr(fcsthr,"-")==NULL) {
          if (strstr(fcsthr,":")!=NULL) {
            sscanf(fcsthr,"%d:%d",&hour,&minute);
          } else {
            sscanf(fcsthr,"%d",&hour);
            minute = 0;
          }
          zhour = (hour + atoi(cycle)) % 24;
          sprintf(regtime,"%d:%02d",zhour,minute);
          strcat(outstr, regtime);
        } else {
          strcat(outstr, fcsthr);
        }
        prmfound=True;
      }
      if (strcmp(gfaitem[j],"FHR") == 0) {
        strcat(outstr, fcsthr);
        prmfound=True;
      }
      if (strcmp(gfaitem[j],"BFHR") == 0) {
        fcsthrwrk[0]='\0';
        if ((ptr=strstr(fcsthr,"-"))!=NULL) {
          strncpy(fcsthrwrk,fcsthr,ptr-fcsthr+1);
          fcsthrwrk[ptr-fcsthr+1]='\0';
        } else {
          strcpy(fcsthrwrk,fcsthr);
        }
        strcat(outstr,fcsthrwrk);
        prmfound=True;
      }
      if (strcmp(gfaitem[j],"EFHR") == 0) {
        fcsthrwrk[0]='\0';
        if((ptr=strstr(fcsthr,"-"))!=NULL) {
          sscanf(ptr+1,"%s",fcsthrwrk);
        }
        strcat(outstr,fcsthrwrk);
        strcat(outstr,"Z");
        prmfound=True;
      }
      if (strcmp(gfaitem[j],"TAG") == 0) {
        strcat(outstr, tag);
        prmfound=True;
      }
      if (strcmp(gfaitem[j],"HZD")==0) {
        if (strcmp(hazard, "IFR") == 0) {
          if (strstr(dueto,"CIG") != NULL &&
            strstr(dueto,"VIS") != NULL) {
            strcpy(work,"IFR");
          } else if (strstr(dueto,"CIG") != NULL) {
            strcpy(work,"IFR_CIG");
          } else if (strstr(dueto,"VIS") != NULL) {
            strcpy(work,"IFR_VIS");
          } else {
            strcpy(work,"IFR");
          }
        } else if (strcmp(hazard,"FZLVL") == 0) {
          sprintf(work,"0%c:",176);  /* degree symbol is ascii dec 176 */
        } else if (strcmp(hazard,"M_FZLVL") == 0) {
          sprintf(work,"0%c", 176);
        } else {
          strcpy(work,hazard);
        }
        if (strlen(work) > 0) {
          strcat(outstr, work);
        }
        prmfound=True;
      }
      if (strstr(gfaitem[j],"VIS") != NULL) {

	if ( strstr( dueto, "VIS BLW 3SM" ) )
	   strcat( outstr, "VIS BLW 3SM" );
	else if ( strstr( dueto, "VIS 3-5SM" ) )
	   strcat( outstr, "VIS 3-5SM" );

	prmfound=True;
      }
      if ( strstr(gfaitem[j],"BLW") != NULL) {

	if ( strstr( dueto, "CIG BLW 010" ) )
	   strcat( outstr, "BLW 010" );
	else if ( strstr( dueto, "CIG 010-030" ) )
	   strcat( outstr, "BLW 030" );
					        
	/* Remove the extra '|' */
      if ( outstr[ strlen(outstr) - 1 ] == '|' )
         outstr[ strlen(outstr) - 1 ] = '\0';

         prmfound=True;
      }
      if (strstr(gfaitem[j],"WX") != NULL) {
	if (strcasecmp(hazard,"IFR") == 0 ||
	    strcasecmp(hazard,"MVFR") == 0 ) {
	  if (strstr(dueto,"VIS") != NULL) {
	     strcpy(work, strstr(dueto,"SM")+3);
          } else {
            strcpy(work," ");
          }
        } else if (strcmp(hazard,"MT_OBSC") == 0) {
          if (strstr(dueto, "MTNS OBSC") != NULL) {
            strcpy(work, dueto+13);
          } else {
            strcpy(work," ");
          }
        } else if (strcmp(hazard,"CLD") == 0 || strcmp(hazard,"CLD_TOPS")==0) {
          if (strlen(dueto) > 0 ) {
             strcpy(work, dueto);
          }
          else {
             strcpy(work,"");
             outstr[strlen(outstr)-1]='\0';
          }
        } else if (strcmp(hazard,"TS") == 0) {
          if (strlen(dueto) > 0 ) {
             strcpy(work, dueto);
          }else {
            strcpy(work," ");
          }

        }


        if (strstr(gfaitem[j],"WX|") != NULL) {
          lenlimit = atoi(gfaitem[j]+3);
          /* wrapbydelim('/','|', lenlimit, work, work2, &ier); */
          cst_wrap(work, "/", &lenlimit, "|", "", work2, &ier); 
          strcpy(work,work2);
        }

        strcat(outstr,work);
        prmfound=True;
      }

      if (strstr(gfaitem[j],"SSYM") != NULL ||
          strstr(gfaitem[j],"WSYM") != NULL ||
          strstr(gfaitem[j],"TSYM") != NULL ||
          strstr(gfaitem[j],"ISYM") != NULL ||
          strstr(gfaitem[j],"IBDR") != NULL ||
          strstr(gfaitem[j],"BGFN") != NULL) {
          if (strchr(gfaitem[j],'|') !=NULL) {
            *(strchr(gfaitem[j],'|'))=':';            
          }
          strcat(outstr, gfaitem[j]);
          prmfound=True;
      }
      if (strcmp(gfaitem[j],"TOP") == 0) {
        strcat(outstr, top);
	/* Remove the extra '|' */
	if ( outstr[ strlen(outstr) - 1 ] == '|' )
	   outstr[ strlen(outstr) - 1 ] = '\0';
        prmfound=True;
      }
      if (strcmp(gfaitem[j],"BSE") == 0) {
        if (strstr(hazard,"TURB") != NULL) {
          strcpy(work, base);
        } else if (strstr(hazard,"ICE") != NULL) {
          if (strcmp(base,"FZL") != 0) {
            strcpy(work, base);
          } else {
            sprintf(work,"%s/%s",fzltop, fzlbse);
          }
        } else {
          strcpy(work,base);
        }

        strcat(outstr, work);
        prmfound=True;
      }
      if (strcmp(gfaitem[j],"LVL") == 0) {
        strcat(outstr,level);
        prmfound=True;
      }
      if (strcmp(gfaitem[j],"ITS") == 0) {
        strcat(outstr,intensity);
        prmfound=True;
      }
      if (strcmp(gfaitem[j],"CVR") == 0) {
        strcat(outstr,coverage);
        if ( ( strcasecmp( hazard, "IFR" ) == 0 ||
               strcasecmp( hazard, "MVFR" ) == 0 ) &&
             strstr( dueto, "CIG" ) != NULL ) {
           if ( strlen( coverage ) == 0 )
              strcat( outstr, "CIG" );
           else
              strcat( outstr, " CIG" );
        }
        /* Remove the extra '|' */
        if ( outstr[ strlen(outstr) - 1 ] == '|' )
           outstr[ strlen(outstr) - 1 ] = '\0';
        prmfound=True;
      }
      if (strcmp(gfaitem[j],"CTG") == 0) {
        strcat(outstr, category);
        prmfound=True;

        if ( strlen(category) == 0 ) {
           outstr[strlen(outstr) - 1] = '\0';
        }
      }
      if (strcmp(gfaitem[j],"FRQ") == 0) {
        strcat(outstr, frequency);
        prmfound=True;
      }
      if (strcmp(gfaitem[j],"BLNK")==0) {
        strcat(outstr," ");
        prmfound=True;
      }
      if (strcmp(gfaitem[j],"NIL")==0) {
        strcat(outstr,"NIL");
        prmfound=True;
      }
      if (! prmfound) {
        *iret = -11; /* Parameter is invalid */
        sprintf(errstr,"inc_pgfatxt:%s",gfaitem[j]);
        er_wmsg("IN", iret, errstr, &ier, 2, strlen(errstr));
      }            
    }
    strcat(outstr,"|");
  }

  outstr[strlen(outstr)-1]='\0';
  strcpy(outstrfin, shave(outstr, " |"));

  for (i = 0; i < 10; i++)
    free(gfaitem[i]);
  free(gfaitem);
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
