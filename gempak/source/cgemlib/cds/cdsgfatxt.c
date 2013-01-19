#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "cds.h"

#define FONT_TYPE	2
#define TEXT_FONT	1
#define TEXT_SIZE	1
#define TEXT_WIDTH	1

void
cds_gfatxt(const VG_DBStruct *el, VG_DBStruct *txt_el, int *iret)
/*****************************************************************************
 * cds_gfatxt
 *
 * Fill in a given VG_DBStruct with the information from the GFA element
 * necessary to create the text box to display for the element
 *
 * Input parameters:
 *  *el         VG_DBStruct     GFA VG object to process
 *
 * Output parameters:
 *  *txt_el     VG_DBStruct     Temporary VG object to create for the text
 *  *iret       int     Return code
 *                      0 = Function successful
 *                     -4 = Invalid pointer in arguments
 *                     -5 = Problem in Parsing Text Layout String
 *                      3 = "NIL" found in Text Layout String
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 * B. Yin/SAIC          06/06   change 3 lines text to 2 lines. 	
 * S.Danz/AWC		08/06	Initialize strings to empty in case they aren't
 * 				set later
 * B. Yin/SAIC		10/06	remove FL from the text box.
 * L. Hinson/AWC        12/06   Major revision to support GFA text
 *                              Labels via settings in settings.tbl
 *                              or uattribd.tbl
 * J. Wu/SAIC		05/07	initialize ALL strings to avoid garbage value
 * J. Wu/SAIC           06/08   add intensity for MTW
 * J. Wu/SAIC           06/08   add coverage/LYR for CLD
 * J. Wu/SAIC           06/08   add category/frequency/GR for TS
 * B. Yin/SAIC          06/08   add coverage for IFR/MVFR
 * L. Hinson/AWC        06/08   Get the cycle through tag TAG_GFA_CYCLE and
                                pass through inc_pgfatxt
 *****************************************************************************/
{
    float   blat, blon;
    char    tag[32], topstr[32], botstr[32], tmpstr[32];
    char    areatypestr[24], value[32], level[32];
    char    intensity[32], coverage[32];
    char    category[32], frequency[32];
    char    hazid[ 8 ], cycle[8], fhrs[ 32 ], haztag[ 32 ];
    int     ii, len, top, bot, idx, ier;
    char    status[10], fzltop[5], fzlbot[5], type[50], textLayoutStr[256];
/*---------------------------------------------------------------------*/

    if (!el || !txt_el) {
        *iret = -4;
        return;
    }
    
    *iret = 0;

    tag[ 0 ]		= '\0';
    topstr[ 0 ]		= '\0';
    botstr[ 0 ] 	= '\0';
    tmpstr[ 0 ] 	= '\0';    
    areatypestr[ 0 ]	= '\0';
    value[ 0 ] 		= '\0';    
    level[ 0 ]		= '\0';
    intensity[ 0 ]      = '\0';
    coverage[ 0 ]       = '\0';
    category[ 0 ]       = '\0';
    frequency[ 0 ]      = '\0';
    hazid[ 0 ]		= '\0';
    cycle[ 0 ]          = '\0';
    fhrs[ 0 ]		= '\0';
    haztag[ 0 ]		= '\0';
    status[ 0 ]		= '\0';
    type[ 0 ]		= '\0';
    fzltop[ 0 ] 	= '\0';
    fzlbot[ 0 ] 	= '\0';
    textLayoutStr[ 0 ]	= '\0';

    cvg_getFld ( el, TAG_GFA_LAT, value, &ier );
    blat = atof ( value );

    cvg_getFld ( el, TAG_GFA_LON, value, &ier );
    blon = atof ( value );
         
    sprintf ( tag, "<vg_type>%d<vg_class>%d", SPTX_ELM, CLASS_TEXT );
    cvg_t2v ( tag, txt_el, &ier );    

    txt_el->hdr.maj_col            = el->hdr.maj_col;
    txt_el->elem.spt.info.sptxtyp  = 16;  /* Bounded, backfilled box */
    txt_el->elem.spt.info.lat      = blat;
    txt_el->elem.spt.info.lon      = blon;
    txt_el->elem.spt.info.offset_x = 0;
    txt_el->elem.spt.info.offset_y = 0; 
    txt_el->elem.spt.info.rotn     = 0;
 
    cds_getinx( (VG_DBStruct *) el, &idx, &ier);
    
    if ( ier >= 0 ) {            
      if ( cdsUattr[idx].info.gfa->textLayout[0]=='\0' ) {
        cvg_getFld ( el, TAG_GFA_TXTLYT, textLayoutStr, &ier);
      } else {
        strcpy(textLayoutStr, cdsUattr[idx].info.gfa->textLayout);
      }
      if ( strstr(textLayoutStr,"NIL") != NULL) {
        *iret = 3;
        return;
      }
      if ( cdsUattr[idx].info.gfa->info.txtcol == 0 ) {
        cvg_getFld ( el, TAG_GFA_TXTCLR, value, &ier );
        if ( ier >= 0 ) {
          txt_el->elem.spt.info.txtcol = atoi(value);
        }
      } else {
        txt_el->elem.spt.info.txtcol = cdsUattr[idx].info.gfa->info.txtcol;
      }

      /*
       *  Set the text size and font
       */
      if ( cdsUattr[idx].info.gfa->info.ithw == 0 ) {
        cvg_getFld ( el, TAG_GFA_TXTHW, value, &ier );
        if ( ier >= 0 ) {
          txt_el->elem.spt.info.ithw = atoi(value);
        }
      } else {
        txt_el->elem.spt.info.ithw = cdsUattr[idx].info.gfa->info.ithw;
      }
      if ( cdsUattr[idx].info.gfa->info.itxfn == 0 ) {
        cvg_getFld ( el, TAG_GFA_TXTFN, value, &ier );
        if ( ier >= 0 ) {
          txt_el->elem.spt.info.itxfn = atoi(value);
        }
      } else {
        txt_el->elem.spt.info.ithw = cdsUattr[idx].info.gfa->info.itxfn;
      }
      if ( fabs(cdsUattr[idx].info.gfa->info.sztext) < 0.01 ) {  /* sztext == 0.0? */
        cvg_getFld ( el, TAG_GFA_TXTSZ, value, &ier );
        if ( ier >= 0 ) {
          txt_el->elem.spt.info.sztext = atof(value);
        }
      } else {
        txt_el->elem.spt.info.sztext = cdsUattr[idx].info.gfa->info.sztext;
      }
      if ( cdsUattr[idx].info.gfa->info.iwidth == 0 ) {
        cvg_getFld ( el, TAG_GFA_TXTWDTH, value, &ier );
        if ( ier >= 0 ) {
          txt_el->elem.spt.info.iwidth = atoi(value);
        }
      } else {
        txt_el->elem.spt.info.iwidth = cdsUattr[idx].info.gfa->info.iwidth;
      }
      if ( cdsUattr[idx].info.gfa->info.ialign == 0 ) {
        cvg_getFld ( el, TAG_GFA_TXTALGN, value, &ier );
        if ( ier >= 0 ) {
          txt_el->elem.spt.info.ialign = atoi(value);
        }
      } else {
        txt_el->elem.spt.info.ialign = cdsUattr[idx].info.gfa->info.ialign;
      }
      
      
      cvg_getFld ( el, TAG_GFA_AREATYPE, areatypestr, &ier );    

      cvg_getFld ( el, TAG_GFA_TOP, value, &ier );
      top = cvg_getFlghtLvl ( value );

      if ( top >= 180 ) {
          sprintf ( topstr, "%d", top );        
      }
      else if ( ( top >= 0 ) && ( strcasecmp ( value, "xxx" ) != 0 ) ) {
          sprintf ( tmpstr, "%d", top );            
          cst_padString ( tmpstr, '0', 0, 3, topstr );
      }
      else if (  strcasecmp ( value, "xxx" ) == 0 ) {
          strcpy ( topstr, "xxx" );            
      }
      else {
          strcpy ( topstr, "" );            
      }

      /*
       *  Bottom flight level is xxx if no value, SFC if 0, and the numeric
       *  equivalent in all other cases.
       */  
      cvg_getFld ( el, TAG_GFA_BOTTOM, value, &ier );    
      if ( strlen( value) <= (size_t)0 ) {
          strcpy ( botstr, "" );
          bot = -1;
      }
      else if ( strcasecmp ( value, "xxx") == 0 ){
          strcpy ( botstr, "xxx" );            
          bot = 0;
      }
      else if ( strcasecmp( value, "SFC") == 0 ) {
          strcpy ( botstr, "SFC" );            
          bot = 0;
      }
      else if ( strcasecmp( value, "FZL") == 0 ) {

          strcpy ( botstr, "FZL" );            
          bot = 0;
          cvg_getFld (el, TAG_GFA_FZL_TOP, value, &ier );
          if ( ier == 0) {
            strcpy(fzltop, value);
          } else {
            strcpy(fzltop, "UNK");
          }
          cvg_getFld (el, TAG_GFA_FZL_BOTTOM, value, &ier );
          if ( ier == 0) {
            strcpy(fzlbot, value);
          } else {
            strcpy(fzlbot, "UNK");
          }
      }
      else {
          bot = cvg_getFlghtLvl( value );

          if ( bot >= 180 ) {
              sprintf ( botstr, "%d", bot );        
          }
          else {
              sprintf ( tmpstr, "%d", bot );            
              cst_padString ( tmpstr, '0', 0, 3, botstr );
          }
      }


      if ( top >= 0 || bot >= 0 ) {
          sprintf ( tag, "\n%s/%s", topstr, botstr );            
      }
    }
    ctb_gfagid( areatypestr, hazid, &ier);
    cvg_getFld ( el, TAG_GFA_CYCLE, cycle, &ier );
    if (ier < 0) {
      strcpy(cycle,"0");
    }
    cvg_getFld ( el, TAG_GFA_FCSTHR, fhrs, &ier );
    cvg_getFld ( el, TAG_GFA_TAG, haztag, &ier );
    cvg_getFld ( el, TAG_GFA_STATUS, status, &ier );
    cvg_getFld ( el, "DUE TO", type, &ier);
    if (ier < 0) {
      cvg_getFld (el, "Type", type, &ier);
    }

    if ( strcasecmp ( areatypestr, "FZLVL" ) == 0 ) {

       cvg_getFld ( el, "Level", level, &ier );    
    }

    if ( strcasecmp ( areatypestr, "MTW" ) == 0 ) {
       cvg_getFld ( el, "Intensity", intensity, &ier );
    }

    if ( strcasecmp ( areatypestr, "CLD" ) == 0 ) {
        cvg_getFld ( el, "Type", type, &ier );
        cvg_getFld ( el, "Coverage", coverage, &ier );
    }

    if ( strcasecmp ( areatypestr, "TS" ) == 0 ) {
        cvg_getFld ( el, "Type", type, &ier );
        cvg_getFld ( el, "Category", category, &ier );
        cvg_getFld ( el, "Frequency", frequency, &ier );
    }
    else if ( strcasecmp ( areatypestr, "IFR" ) == 0 ||
              strcasecmp ( areatypestr, "MVFR" ) == 0 ) {
        cvg_getFld ( el, "Coverage", coverage, &ier );
    }

    /* This is a new C parsing function to parse text layout string*/
    inc_pgfatxt( status, areatypestr, haztag, cycle, fhrs, type, topstr, botstr, 
	   fzltop, fzlbot, level, intensity, coverage, category, frequency, 
	   textLayoutStr, txt_el->elem.spt.text, &ier );        
    if ( ier != 0 ) {
      *iret = -5; /* CDS: Problem in Parsing Text Layout String */
      return;
    }
             
    len = strlen ( txt_el->elem.spt.text );   
    for ( ii = 0; ii < len; ii++ ) {
        if ( txt_el->elem.spt.text[ii] == '_' ) {
            txt_el->elem.spt.text[ii] = ' ';         
        }
    }
    return;
}
