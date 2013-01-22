#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "vgtag.h"
#include "drwids.h"
#include <stdlib.h>


int obj_match ( VG_DBStruct *el, int vg_class, int vg_type,
		int obj_type );

int reg_match ( VG_DBStruct *el, char region );

int stype_match ( VG_DBStruct *el, int subtype );

int areatype_match ( VG_DBStruct *el, char *area );

int mod_cycle ( VG_DBStruct *el, char *cycle_in, char *cycle_out );

int mod_fcsthr ( VG_DBStruct *el, char *fcsthr_in, char *fcsthr_out );
 
int set_status ( VG_DBStruct *el, char *status );

void set_blocking ( VG_DBStruct *el, int blocking );

void set_fonttype ( VG_DBStruct *el, int fonttype );

void set_color ( VG_DBStruct *el, int major_color, int minor_color,
		 int fill_color );

void set_line ( VG_DBStruct *el, int old_vg_type,
		 int old_line_type, int new_vg_type, int new_line_type );

int in_fcsthrrange (VG_DBStruct *el, char *fcsthrrange_in);

int timeValidOvrHrRange(char *value, int hour1, int hour2);

int DoIt ( char *in_file, char *out_file, int vg_class, int vg_type,
	   int obj_type, int major_color, int minor_color,
	   int fill_color, int blocking, int fonttype, int delete, int gfa,
	   int old_vg_type, int old_line_type, int new_vg_type, int new_line_type,
	   char region, char *cycle_in, char *cycle_out, char *fcsthr_in, 
           char *fcsthrrange_in, char *fcsthr_out, char *status,
           char *areaType, int subtype );

void Usage ( void );

/******************************************************************************
   vgmod.c                                                              

   This program will identify and modify the attributes of objects in a 
   VGF file.  It can also be used to delete objects in a VGF file that
   meet specific criteria.
 
  CONTENTS:
   obj_match()      Identifies if this is the VGF object we are after
   GFA match functions:
     reg_match()      Identifies if there is a match on region, via TAG_GFA_TAG
     areatype_match() Identifies if there is a match on the AREA TYPE (hazard), 
                      via TAG_GFA_AREATYPE
     stype_match()    Identifies if there is a match on the subtype, via
                      TAG_GFA_SUBTYPE
     mod_cycle()      Identifies if there is a match on the cycle.  If so,
                      modify it to the requested cycle, via TAG_GFA_CYCLE.
     mod_fcsthr()     Identifies if there a match on the forcast hour. If so,
                      modify it to the requested forecast hour, via 
                      TAG_GFA_FCSTHR
     in_fcsthrrange() Identifies if the forecast hour in TAG_GFA_FCSTHR is
                      within range of the specified forecast hours.
     timeValidOvrHrRange() Internal function used to determine if the string
                           representation of time is between hour 1
                           (inclusive) & hour 2 (exclusive)
     set_status()     Identifies if there is a match on the specified status
                      in TAG_GFA_STATUS. If so, set the status to "NRML",
                      except on cancels "CAN". 
                      
   set_blocking()   Sets blocking on or off on VGF object.
   set_fonttype()   Set fonttype to software or hardware on VGF object.
   set_color()      Set the major/minor/fill color on VGF object.
   set_line()       Change a line object.
   DoIt()           Major function to sequentially go through each VGF object
                    and conditionally test for matches.
   Usage()          Print Usage Statement
   main()           Parse Out arguments passed to program, and call DoIt().
                                                                       
 **                                                                    
  Log:                                                                 
  S.W.Danz/AWC 8/98        Created                                         
  J. Lewis & L. Hinson/AWC 2006-2008 Add GFA match checking                                  
  L. Hinson/AWC            06/08     Documented 
  L. Hinson/AWC            05/09     Add -g switch to remove non GFA objects 
  J. Lewis                 03/10     Add -l switch to change line type                             
 *****************************************************************************/

int obj_match ( VG_DBStruct *el, int vg_class, int vg_type, int obj_type )
/*****************************************************************************
 obj_match
 
 This function identifies whether this is the VGF object we are after by
 class, type, and object type.
 
 obj_match (el, vg_class, vg_type, obj_type)
   
 Returns
   1 if there is a match, 0 otherwise.
**
Log:  S.W. Danz/AWC 8/98    Created
*******************************************************************************/ 
{
    int status = 0;

    if (obj_type == -1) {
        status = 1;
    } else {
        switch ( el->hdr.vg_class ) {
            case  CLASS_FRONTS:
                status = (el->elem.frt.info.fcode == obj_type);
            break;
            
            case CLASS_WATCHES:
                status = (el->elem.wbx.info.w_type == obj_type);
            break;

            case CLASS_LINES:
                switch (el->hdr.vg_type) {
                    case SPLN_ELM:
                        status = (el->elem.spl.info.spltyp == obj_type);
                    break;

                    case LINE_ELM:
                        status = (el->elem.lin.info.lintyp == obj_type);
                    break;
                }
            break;

            case CLASS_SYMBOLS:
                status = ((int) el->elem.sym.data.code[0] == obj_type);
            break;

            case CLASS_TEXT:
                switch (el->hdr.vg_type) {
                    case TEXT_ELM:
                    case TEXTC_ELM:
                        status = 1;
                    break;

                    case SPTX_ELM:
                        status = (el->elem.spt.info.sptxtyp == obj_type);
                    break;
                }
            break;
            case CLASS_MET:
                switch (el->hdr.vg_type) {
                    case GFA_ELM:
                        status = 1;
                    break;
                }
	    break;

            case CLASS_WINDS:
                status = (el->elem.wnd.info.wndtyp  == obj_type);
            break;

            case CLASS_ANY:
                status = 1;
            break;

            case CLASS_COMSYM:
                status = 1;
            break;

            case CLASS_PRODUCTS:
                status = 1;
            break;

            case CLASS_TRACKS:
                status = 1;
            break;

            case CLASS_CIRCLE:
                status = 1;
            break;
        }
    }

    status = (status && 
                ((vg_class == -1) || (el->hdr.vg_class == vg_class)) &&
                ((vg_type == -1) || (el->hdr.vg_type == vg_type)));

    return status;
}

/*========================================================================*/

int reg_match ( VG_DBStruct *el, char region )
/*****************************************************************************
 reg_match

 This function determines whether the region defined in the TAG_GFA_TAG tag of 
 the el structure matches the region passed to the function.  Values for region
 may be the characters, 'E', 'C', or 'W'.

 Returns
  1 if there is a match, 0 otherwise.
**
Log:  J. Lewis/AWC 2006     Created
******************************************************************************/
{
    char value[5];
    int status;
    int iret;
 
    status = 0;

    cvg_getFld ( el, TAG_GFA_TAG, value, &iret );
    if ( strchr ( value, region ) ) status = 1;

    return status;
}

/*========================================================================*/

int areatype_match ( VG_DBStruct *el, char *areaType )
/*****************************************************************************
 areatype_match
  This function determines whether the hazard type defined in TAG_GFA_AREATYPE
  tag matches that of the areaType value passed to the function. 
  
 Returns
  1 if there is a match, 0 otherwise.
**
Log:  L. Hinson/AWC 2007    Created
*****************************************************************************/
{
    int iret;
    char work[25];  
    cvg_getFld ( el, TAG_GFA_AREATYPE, work, &iret );
    return (strcmp(work,areaType) == 0);
}       
/*========================================================================*/

int stype_match ( VG_DBStruct *el, int subtype )
/*****************************************************************************
 stype_match
  This function determines whether the subtype derived from the 
  TAG_GFA_SUBTYPE tag matches that of the subtype value passed to the function.
  This function derives the subtype through by taking the tag value (tmp) and 
  finding the modulus 10 value of it (tmp % 10) .  Subtype values of 0, 1, 2, 
  3, or 4 are possible, where 0 is a snapshot, 1 is a user drawn smear, 2 is a
  system generated smear, 3 is a user drawn outlook, and 4 is a system 
  generated outlook.
  
  Returns
   1 if there is a match, 0 otherwise.
**
Log: J. Lewis/AWC 2006    Created
******************************************************************************/
{
    char value[5];
    int tmp;
    int type;
    int status;
    int iret;

    status = 0;

    cvg_getFld ( el, TAG_GFA_SUBTYPE, value, &iret );
    type = atoi(value);
    tmp = type%10;
    if ( tmp == subtype ) status = 1;   

    return status;
}

/*========================================================================*/

int mod_fcsthr ( VG_DBStruct *el, char *fcsthr_in, char *fcsthr_out )
/*****************************************************************************
 mod_fcsthr
 
 This function determines whether the forecast hour defined in the 
 TAG_GFA_FCSTHR tag of the el structure matches the forecast hour (fcsthr_in) 
 passed to the function. If there is a match, and the value of fcsthr_out is 
 different than that of fcsthr_in, the cvg_setFld function is called to modify
 the value of the forecast hour tag with that of fcsthr_out.  The function also
 sets the tag TAG_GFA_SNAPSHOTHRS, if there is a match and the forecast hour is
 a hyphenated time.  Attributes separated by ";" are modified to values 6-hours
 earlier than their previous values.
 
 Returns
   1 if there is a match, 0 otherwise.
**
Log:  J. Lewis/AWC 2006-2007   Created
******************************************************************************/
{
    char value[10];
    char snaps[10];
    char new_snaps[10];
    char * ctmp;
    int tmp;
    int sflag, status;
    int iret;
    
    sflag = 0;    
    status = 0;

    cvg_getFld ( el, TAG_GFA_FCSTHR, value, &iret );
/*
 *  This is to find the normal 0-hr and 3-hr snapshots
 */
    if ( strcmp( fcsthr_in,fcsthr_out ) == 0 ) {
        if ( strcmp( value,fcsthr_in ) == 0 ) {
            status = 1;
	} else {
/*
 *      This section is for identifying the "other" snapshots earlier
 *      than the 6-hr.  They contain a ":" and look like "5:00", "3:20", etc.
 *      First, look for the token ":"
 */
            if ( strchr (value, ':') != NULL ) {
/*
 *      Next, find the value preceeding the token.
 */
                ctmp = strtok( value, ":" );
                if (strcmp ( ctmp, fcsthr_in ) == 0) {
                    status = 1;
		}
            }
        }
/*
 *      If the input fcst hours are not the same then the request is to change
 *      the records with a "fcsthr_in" value to "fcsthr_out" value.
 */
    } else if ( strcmp ( value, fcsthr_in ) == 0 ) {
	if ( strchr (value, '-') != NULL ) sflag = 1;
        sprintf ( value, "%s", fcsthr_out );
        cvg_setFld ( el, TAG_GFA_FCSTHR, value, &iret );
        status = 1;
/*
 *	If the GFA_SMEARHRS element exists, we have to change those values as well.
 */
	if ( sflag ) {
	    cvg_getFld ( el, TAG_GFA_SNAPSHOTHRS, snaps, &iret );

	    ctmp = strtok ( snaps, ";");
	    new_snaps[0] = '\0';
	    while ( ctmp != NULL ) {
		tmp = atoi(ctmp);
		tmp = tmp - 6;
		if ( tmp >= 0) {
		    sprintf (ctmp,"%d",tmp);
	            strcat  (new_snaps,ctmp);
		    strcat  (new_snaps,";");
		}

		ctmp = strtok ( NULL, ";" );
	    }

            cvg_setFld ( el, TAG_GFA_SNAPSHOTHRS, new_snaps, &iret );

	}
    }
    return status;
}

int in_fcsthrrange (VG_DBStruct *el, char *fcsthrrange_in)
/*****************************************************************************
 in_fcsthrrange
 
 This function determines whether the forecast hour defined in the 
 TAG_GFA_FCSTHR tag of the el structure is in range of the specified forecast
 hours in fcsthrrange_in.   This function assumes that the format of 
 fcsthrrange_in takes the form of fcsthr1-fcsthr2, where fcsthr1 is of type 
 integer, and fcsthr2 is of type integer, and hyphen (-) separates the two 
 times.  Given a forecast hour of x which may be expressed as integer, or time
 (HH:MM), the function defines that  forecast hour x is in range if this 
 condition is met:  fcsthr1 <= x < fcsthr2.  This function calls 
 timeValidOvrHrRange() to test the condition.
 
 Returns
  1 if the forecast hour is in range, 0 otherwise.
**
Log. L. Hinson/AWC 2007     Created
*******************************************************************************/
{
  char value[6];
  int status;
  int hour1, hour2 = 0;
  int iret;
  
  status = 0;

  cvg_getFld ( el, TAG_GFA_FCSTHR, value, &iret );
  if ( strstr(fcsthrrange_in,"-") == NULL ) {
    hour1 = atoi(fcsthrrange_in );
    hour2 = hour1;
  } else {
    sscanf(fcsthrrange_in,"%d-%d",&hour1,&hour2);
  }
  if (timeValidOvrHrRange(value,hour1,hour2)) {
    status = 1;
  }
  return status;
}
    

int timeValidOvrHrRange(char *value, int hour1, int hour2)
/*****************************************************************************
 timeValidOvrHrRange
 This function converts the string representation of time specified in value, 
 and converts it to a float [realtime] for comparisons to hour1 and hour2.   
 Value may be a simple integer, or a HH:MM representation.  If the value 
 contains a ":" it is broken down to its hour and minutes components, then 
 converted to a float [realtime]. The comparison:

 if ((float) hour1 <= realtime && realtime < (float) hour2) is evaluated. 
 
 Returns
   1 if the above condition evaluates to true, 0 otherwise.
**
Log:  L. Hinson/AWC 2007    Created
******************************************************************************/
{
  int hour, minute;
  int status;
  float realtime;
  status = 0;
  if (strstr(value,":") != NULL) {
    sscanf(value,"%d:%d",&hour,&minute);
    realtime = hour+minute/60.0;
  } else {
    realtime = atof(value);
  }
  if ( (float) hour1 <= realtime && realtime < (float) hour2) {
    /* (hour1 <= hour < hour2) ? */
    status = 1;
  }
  if ( fabs(realtime - (float) hour1) < .01) {   
    /* value = hour?, if no range given? */
    status = 1;
  }
  return status;
}

/*========================================================================*/

int mod_cycle ( VG_DBStruct *el, char *cycle_in, char *cycle_out )
/*****************************************************************************
 mod_cycle
 
 This function determines whether the cycle type defined in the 
 TAG_GFA_CYCLE tag matches that of the cycle_in value passed to
 the function.  If there is a match, and provided that cycle_out
 is different than cycle_in, the cvg_setFld function is called to
 modify the value of the cycle tag with that of cycle_out. 
 
 Returns
  1 if there is a match, 0 otherwise.
**
Log: L. Hinson/AWC   6/08    Created
*****************************************************************************/
{
  char value[4];
  int status = 0;
  int iret = 0;
  
  cvg_getFld ( el, TAG_GFA_CYCLE, value, &iret );
  if ( iret == 0 ) {
    if ( strcmp ( value, cycle_in ) == 0 ) {
      status = 1;
      if ( strcmp ( cycle_in, cycle_out) != 0 ) {
        sprintf ( value, "%s", cycle_out );
        cvg_setFld ( el, TAG_GFA_CYCLE, value, &iret );
      }
    }
  }
  return status;
}  

        
/*========================================================================*/
int set_status ( VG_DBStruct *el, char *status )
/*****************************************************************************
 set_status
 
 This function determines whether the status defined in the TAG_GFA_STATUS tag 
 matches that of the status value passed to the function.   If so, it sets the
 status to NRML, except on Cancelled (CAN) GFA objects.
 
 Returns
  1 if there is a match, 0 otherwise.
**
Log:  J. Lewis/AWC 2007    Created
******************************************************************************/
{
    char value[5];
    int match;
    int iret;

    match = 0;
    cvg_getFld ( el, TAG_GFA_STATUS, value, &iret );
    if ( strstr ( value, status ) != '\0' ) {
	match = 1;
        if ( strstr ( value, "CAN" ) == '\0' ) {
	    sprintf ( value, "%s", "NRML" );
            cvg_setFld ( el, TAG_GFA_STATUS, value, &iret );
	}
    }
    return match;
}
/*========================================================================*/

void set_blocking ( VG_DBStruct *el, int blocking )
/*****************************************************************************
 set_blocking
 
 This routine sets the blocking on (1) or off (0) on special text elements
 (SPTX_ELM)
 
**
Log:  S. Danz/AWC 1998        Created
*****************************************************************************/
{
    if (blocking < 0)
        return;

    switch ( el->hdr.vg_class ) {
        case  CLASS_FRONTS:
            /* N.A. */
        break;
        
        case CLASS_WATCHES:
            /* N.A. */
        break;

        case CLASS_LINES:
            /* N.A. */
        break;

        case CLASS_SYMBOLS:
            el->elem.sym.info.width = blocking *
	    			800 + (el->elem.sym.info.width % 100);
        break;

        case CLASS_TEXT:
            switch (el->hdr.vg_type) {
                case TEXT_ELM:
                case TEXTC_ELM:
                    /* N.A. */
                break;

                case SPTX_ELM:
                    if (blocking) {
                        if (el->elem.spt.info.sptxtyp == 0) {
                            el->elem.spt.info.sptxtyp = 5;
                        } else if (el->elem.spt.info.sptxtyp == 3) {
                            el->elem.spt.info.sptxtyp = 4;
                        } else if (el->elem.spt.info.sptxtyp == 10) {
                            el->elem.spt.info.sptxtyp = 11;
                        }
                    } else {
                        if (el->elem.spt.info.sptxtyp == 5) {
                            el->elem.spt.info.sptxtyp = 0;
                        } else if (el->elem.spt.info.sptxtyp == 4) {
                            el->elem.spt.info.sptxtyp = 3;
                        } else if (el->elem.spt.info.sptxtyp == 11) {
                            el->elem.spt.info.sptxtyp = 10;
                        }
                    }
                break;
            }
        break;

        case CLASS_WINDS:
            if (el->hdr.vg_type != HASH_ELM) {
                el->elem.wnd.info.wndtyp = 112 + blocking * 2;
            }
        break;

        case CLASS_ANY:
            /* N.A. */
        break;

        case CLASS_COMSYM:
            el->elem.sym.info.width = blocking *
	    			800 + (el->elem.sym.info.width % 100);
        break;

        case CLASS_PRODUCTS:
            /* N.A. */
        break;

        case CLASS_TRACKS:
            /* N.A. */
        break;

        case CLASS_CIRCLE:
            /* N.A. */
        break;
    }
}

/*=====================================================================*/

void set_fonttype ( VG_DBStruct *el, int fonttype )
/*****************************************************************************
 set_fonttype
 
 This routine sets the fonttype on Text or Special Text elements to either a 
 SW (0) or HW (1) fonttype.

**
Log:  S. Danz/AWC 1998       Created
******************************************************************************/
{
    if (fonttype < 0)
        return;

    switch ( el->hdr.vg_class ) {
        case CLASS_TEXT:
            switch (el->hdr.vg_type) {
                case TEXT_ELM:
                case TEXTC_ELM:
                    el->elem.txt.info.ithw = 1 + fonttype;
                break;

                case SPTX_ELM:
                    el->elem.spt.info.ithw = 1 + fonttype;
                break;
            }

        default:
            /* N.A. */
        break;
    }
}

/*======================================================================*/

void set_color ( VG_DBStruct *el, int major_color, int minor_color, 
							int fill_color )
/*****************************************************************************
 set_color
 
 This routine sets the major, minor, or fill colors on VGF objects.
 
**
Log:  S. Danz/AWC 1998        Created
*****************************************************************************/
{
    if (major_color != -1) {
        el->hdr.maj_col = major_color;
    }

    if (minor_color != -1) {
        el->hdr.min_col = minor_color;
    }

    if ((el->hdr.vg_class == CLASS_TEXT) && (el->hdr.vg_type == SPTX_ELM)) {
        if (major_color != -1) {
            el->elem.spt.info.txtcol = major_color;
        }

        if (minor_color != -1) {
            el->elem.spt.info.lincol = minor_color;
        }

        if (fill_color != -1) {
            el->elem.spt.info.filcol = fill_color;
        }
    }
}

/*==========================================================================*/

/*==========================================================================*/

void set_line ( VG_DBStruct *el, int old_vg_type, 
		int old_line_type, int new_vg_type, int new_line_type )

/***************************************************************************
 set_line

 This routine changes the vg type and line type for line objects.

****************************************************************************/
{
     int	nn, ii, recsz;
     VG_DBStruct tmp_el;
	
     nn = 0;
     recsz = 0;

     if (old_line_type == 0)
	 return;

     if ((el->hdr.vg_type == old_vg_type) && (el->elem.lin.info.lintyp == old_line_type)) {
	    
         nn = el->elem.lin.info.numpts;
/*
 *	Save off the lat/lon points.
 */
	 for(ii = 0; ii < nn; ii++) {
  	     tmp_el.elem.lin.latlon[ii] = el->elem.lin.latlon [ii];
             tmp_el.elem.lin.latlon[ii+nn] = el->elem.lin.latlon [ii+nn];
	 }

	 for(ii = 0; ii < nn; ii++) {
             el->elem.spl.latlon[ii] = tmp_el.elem.lin.latlon [ii];
             el->elem.spl.latlon[ii+nn] = tmp_el.elem.lin.latlon [ii+nn];
	 }
/*
 *	Check the new line type and set attributes.
 */
	 switch (new_vg_type) {
	     case SPLN_ELM:
	         recsz = ( (sizeof(float) * 2 * nn) + sizeof(VG_HdrStruct) +
                            sizeof(SpLineInfo) );
		 el->hdr.recsz = recsz;
		 el->hdr.vg_type = new_vg_type;
		 el->elem.spl.info.spltyp = new_line_type;
		 el->elem.spl.info.splwid = 2;
		 el->elem.spl.info.splsiz = 0.3;

	     break;
	  }
   
	}
}
/***************************************************************************/

int DoIt ( 
    char    *in_file, 
    char    *out_file, 
    int     vg_class,
    int     vg_type,
    int     obj_type,
    int     major_color,
    int     minor_color,
    int     fill_color,
    int     blocking,
    int     fonttype,
    int     delete,
    int     gfa,
    int     old_vg_type,
    int     old_line_type,
    int     new_vg_type,
    int     new_line_type,
    char    region,
    char    *cycle_in,
    char    *cycle_out,
    char    *fcsthr_in,
    char    *fcsthrrange_in,
    char    *fcsthr_out,
    char    *status,
    char    *areatype,
    int     subtype
)
/*****************************************************************************
 DoIt
 
 This function cycles through all objects embedded in the VGF file.  It
 identifies whether the VGF object is of the vg_class, vg_type, and obj_type
 described through the -o switch.  Depending on whether other match criteria
 are to be tested, the routine will either modify specific attributes of the
 VGF object or delete the VGF object.
 
 Match criteria being tested:
   region match  via function reg_match()
   subtype match via function stype_match()
   forecast hour match via function mod_fcsthr()
   in forecast hour range match via function in_fcsthrrange()
   status match via function set_status()
   area type (hazard type) match via function areatype_match()
     NOTE:  An area type match used in tandem with either a forecast hour/
     in-forecast-hour range match evaluates as a combined condition: that 
     both the area type and (in)-forecast-hour match conditions are both
     satisfied.
 Specific attributes being modified: 
   forecast hour - via function mod_fcsthr
   status - via function set_status
   color - via function set_color
   blocking - via function set_blocking
   fonttype - via function set_fonttype
   line - via function set_line
   
 **
 Log:   S. Danz/AWC                1998       Created
        J. Lewis & L. Hinson/AWC   2006-2007  Modified for GFA
        L. Hinson                  05/09      Add -g switch to remove non-GFA objects
	J. Lewis                   03/2010    Add -l switch to change line type
******************************************************************************/
{
    int         iret;
    int         readpos;
    int         wrtpos;
    long        in_size;
    int         match;
    int         matchareaset = 0, matchhrset = 0, matchhrrangeset = 0;
    char        tmp_name[PATH_MAX];
    int         loc;
    /* FILE        *out_fp; */
    VG_DBStruct el;

    cvg_crvgf(out_file, &iret);

    readpos = 0;
    wrtpos = 0;
    cfl_inqr(in_file, NULL, &in_size, tmp_name, &iret);
    if (iret != 0) {
        iret = -1;
        return (iret);
    }
    while (!iret && (readpos < in_size)) {
        cvg_rdrec(in_file, readpos, &el, &iret);
        if (!iret) {
            readpos += el.hdr.recsz;
            if (el.hdr.vg_type == 22) {
              cvg_writefD( &el, wrtpos, el.hdr.recsz, out_file, &loc, &iret);
		wrtpos += el.hdr.recsz;
  
	    } else if (el.hdr.delete == 0) {
                match = obj_match(&el, vg_class, vg_type, obj_type);
                
                if (match) {
                  
                  matchhrset = matchareaset = 0;

                  if (region != '\0') match = reg_match(&el, region);

	          if (subtype != 0) match = stype_match(&el, subtype);
                  
                  if (cycle_in[0] != '\0') match = mod_cycle(&el, cycle_in, cycle_out);

		  if (fcsthr_in[0] != '\0') matchhrset = match = mod_fcsthr(&el, fcsthr_in, fcsthr_out);

                  if (fcsthrrange_in[0] != '\0') matchhrrangeset = match = in_fcsthrrange(&el, fcsthrrange_in);

		  if (status[0] != '\0') match = set_status(&el, status);

                  if (areatype[0] != '\0') matchareaset = match = areatype_match(&el, areatype);

                  /* This conditionally changes color on specified area type & specified hour or range
                     of hours only. */
                  if (matchareaset) {
                    if (fcsthr_in[0] != '\0')
                      match = matchhrset;
                    if (fcsthrrange_in[0] != '\0')
                      match = matchhrrangeset;
                  }
                  
                }
                
                if (match && !delete) {
                    set_color(&el, major_color, minor_color, fill_color);
                    set_blocking(&el, blocking);
                    set_fonttype(&el, fonttype);
		    set_line(&el, old_vg_type, old_line_type, new_vg_type, new_line_type);

		}

		if ((match && !delete) || (!match && !gfa)) {
                    cvg_writefD( &el, wrtpos, el.hdr.recsz, out_file, &loc, &iret);
                    wrtpos += el.hdr.recsz;
                }
            }
        }
    }

    return iret;
}

/*==============================================================================*/

void Usage ( void )
/*****************************************************************************
 Usage
 
 This routine displays a usage statement of this program.
 
**
Log:   S. Danz/AWC                1998       Created
       J. Lewis & L. Hinson/AWC   2006-2007  Modified for GFA
       L. Hinson                  05/09      Add -g switch to remove non-GFA
                                             objects.
******************************************************************************/                       
{
    printf("Usage: vgmod < -o vg_class:vg_type:obj_type [-c maj:min] [-b #]file [file...] ...>\n" );
    printf("       -o : Object to modify.  The : are required, however a\n");
    printf("            blank field matches all objects.\n" );
    printf("       -l : Change vg type and line type of line objects.\n" );
    printf("       -c : Change color to maj:min:fill.  If a value is\n" );
    printf("            blank that part of the color remains unchanged.\n" );
    printf("       -b : Blocking (if applicable). 0 - off, 1 - on, -1 - Don't Change.\n" );
    printf("       -d : Delete matching objects from file. 0 - off, 1 - on.\n" );
    printf("       -f : Font type flag (if applicable). 0 - SW, 1 - HW, -1 - Don't Change.\n" );
    printf("       -g : Keep only GFA elements.\n");
    printf("       -t : GFA_TAG element. E - East, C - Central, W - West\n" );
    printf("       -y : GFA_CYCLE. Change a polygon cycle (old:new).  When used with\n");
    printf("            the -d switch, deletes the matching object.\n" ); 
    printf("       -h : GFA_FCSTHR. Change a polygon fcsthr (old:new).  When used with\n" );
    printf("            the -d switch, deletes the matching object.\n" );
    printf("       -r : GFA_FCSTHR Range.  Change a polygon over the specified forecast hour\n" );
    printf("            range(s) (hr1-hr2) \n" );
    printf("       -p : GFA_SUBTYPE. 0 - snapshot, 1 - user drawn smear, 2 - system generated\n" );
    printf("            smear, 3 - user drawn outlook, 4 - system generated outlook.\n" );
    printf("       -s : GFA_STATUS (NRML, AMD, CAN, NEW, COR)\n" );
    printf("       -a : GFA_AREATYPE (IFR, MT_OBSC, TURB-HI, TURB-LO, SFC_WND, \n" );
    printf("            LLWS, ICE, FZLVL, M_FZLVL)\n");
    printf("\n");
    printf("The file given is moved to <file>.bak and a new file is generated\n");
}

/*============================================================================*/

int main ( int argc, char *argv[] )
/*****************************************************************************
 main()
 
 This module loads the command line arguments, and initializes the variables
 used in testing match criteria accordingly.  It sets up a temporary work
 VGF file and calls the primary function - DoIt - to do much of the work in 
 cycling through the VGF objects and determining whether match criteria have 
 been met.

**
Log:    S. Danz/AWC                1998       Created
        J. Lewis & L. Hinson/AWC   2006-2007  Modified for GFA
******************************************************************************/
{
    int     opt = 0;
    int     vg_class;
    int     vg_type;
    int     obj_type;
    int     major_color;
    int     minor_color;
    int     fill_color;
    int     blocking;
    int     delete;
    int     fonttype;
    int     readable;
    int     action;
    int     result;
    int     gfa;
    int     old_vg_type;
    int     old_line_type;
    int     new_vg_type;
    int     new_line_type;
    char    cycle_in[4];
    char    cycle_out[4];
    char    fcsthr_in[10];
    char    fcsthrrange_in[12];
    char    fcsthr_out[10];
    char    region;
    int     subtype;
    char    status[10];
    char    *arg;
    char    *sep;
    char    out_name[PATH_MAX];
    char    tmp_name[PATH_MAX];
    char    areatype[25];
    FILE    *outfile;
    
    gfa    = 0;
    old_vg_type = 0;
    old_line_type = 0;
    new_vg_type = 0;
    new_line_type = 0;
    action = 0;
    delete = 0;
    vg_class = vg_type = obj_type = -1;                
    major_color = minor_color = -1;                
    blocking = -1;                
    fonttype = -1;                
    region = '\0';
    cycle_in[0] = cycle_out[0] = '\0';
    fcsthr_in[0] = fcsthr_out[0] = '\0';
    fcsthrrange_in[0] = '\0';
    status[0] = '\0';
    areatype[0] = '\0';
    subtype = 0;
    optind = 1;
    while (optind < argc) {
        opt = getopt(argc, argv, ":o:l:t:y:h:r:g:a:s:p:b:c:d:f:z");
        switch (opt) {
            case 'o':
                vg_class = vg_type = obj_type = -1;                
                arg = optarg;

                sep = strchr(arg, ':');
                if (sep) *sep = '\0';
                sscanf(arg, "%d", &vg_class);
                if (sep) {
                    arg = sep+1;
                    sep = strchr(arg, ':');
                    if (sep) *sep = '\0';
                    sscanf(arg, "%d", &vg_type);
                }
                if (sep) {
                    arg = sep+1;
                    sscanf(arg, "%d", &obj_type);
                }
            break;

/*      TAG_GFA_TAG element */
            case 't':
                sscanf(optarg, "%c", &region);
            break;
            
/*      TAG_GFA_CYCLE element */
            case 'y':
                arg = optarg;
                sep = strchr(arg, ':');
                if (sep) *sep = '\0';
                sscanf(arg, "%s", cycle_in);
                if (sep) {
                  arg = sep+1;
                  sscanf(arg, "%s", cycle_out);
                }
            break;
            
/*      TAG_GFA_FCSTHR element */
	    case 'h':
                arg = optarg;

                sep = strchr(arg, ':');
                if (sep) *sep = '\0';
                sscanf(arg, "%s", fcsthr_in);

                if (sep) {
                   arg = sep+1;
                   sscanf(arg, "%s", fcsthr_out);
                }
            break;
           
           case 'r':
             arg = optarg;
             sscanf(arg, "%s", fcsthrrange_in);
           break;
           
/*      GFA ELEMENT */
            case 'g':
                sscanf(optarg, "%d", &gfa);
            break;
           
/*	TAG_GFA_SUBTYPE */
	    case 'p':
		sscanf(optarg, "%d", &subtype);
	    break;
            
/*      TAG_GFA_AREATYPE */            
            case 'a':
              strcpy(areatype,optarg);
            break;

/*	TAG_GFA_STATUS element */
	    case 's':
		sscanf(optarg, "%s", status);
	    break;


            case 'b':
                sscanf(optarg, "%d", &blocking);
            break;

            case 'd':
                sscanf(optarg, "%d", &delete);
            break;

            case 'f':
                sscanf(optarg, "%d", &fonttype);
            break;

	    case 'l':
		arg = optarg;

		sep = strchr(arg, ':');
		if (sep) *sep = '\0';
		sscanf(arg, "%d", &old_vg_type);

		if (sep) {
		    arg = sep+1;
		    sep = strchr(arg, ':');
		    if (sep) *sep = '\0';
		    sscanf(arg, "%d", &old_line_type);
		}
		if (sep) {
		    arg = sep+1;
                    sep = strchr(arg, ':');
                    if (sep) *sep = '\0';
		    sscanf(arg, "%d", &new_vg_type);
		}
		if (sep) {
                    arg = sep+1;
                    sep = strchr(arg, ':');
                    if (sep) *sep = '\0';
                    sscanf(arg, "%d", &new_line_type);
                }
            break;

            case 'c':
                major_color = minor_color = -1;                
                arg = optarg;

                sep = strchr(arg, ':');
                if (sep) *sep = '\0';
                sscanf(arg, "%d", &major_color);
                if (sep) {
                    arg = sep+1;
                    sep = strchr(arg, ':');
                    if (sep) *sep = '\0';
                    sscanf(arg, "%d", &minor_color);
                }
                if (sep) {
                    arg = sep+1;
                    sscanf(arg, "%d", &fill_color);
                }
            break;

            case ':':
                fprintf(stderr, "ERROR: Missing argument for -%c\n", optopt);
                Usage();
                exit (-1);
            break;
                          
            default:
                action = 1;
                sprintf(out_name,"%s.tmp", argv[optind]);
                readable = access(argv[optind], R_OK);
                if (readable == 0)
                    outfile = fopen(out_name, "w");
                else
                    outfile = NULL;

                if (outfile) {
                    fclose(outfile);
                    result = DoIt(
                            argv[optind],
                            out_name,
                            vg_class,
                            vg_type,
                            obj_type,
                            major_color,
                            minor_color,
                            fill_color,
                            blocking,
                            fonttype,
                            delete,
                            gfa,
			    old_vg_type,
			    old_line_type,
			    new_vg_type,
			    new_line_type,
			    region,
                            cycle_in,
                            cycle_out,
                            fcsthr_in,
                            fcsthrrange_in,
                            fcsthr_out,
 			    status,
                            areatype,
			    subtype                            
                        );
                    if (result) {
                        fprintf(stderr, 
                            "ERROR: Received GEMPAK error %d processing %s, exiting...\n", 
                            result, argv[optind]
                        );
                        if (access(out_name, R_OK)) {
                            unlink(out_name);
                        }

                        exit (-1);
                    } else {
                        sprintf(tmp_name,"%s.bak", argv[optind]);
                        rename(argv[optind], tmp_name);
                        rename(out_name, argv[optind]);
                    }
                } else {
                    if (readable != 0) {
                        fprintf(stderr, 
                            "Warning: Could not read %s, skipping...\n", 
                            argv[optind]
                        );
                    } else {
                        fprintf(stderr, 
                            "Warning: Could not create output file for %s, skipping...\n", 
                            argv[optind]
                        );
                    }
               }
               optind++;

            break;
        }
    }

    if (!action) {
        Usage();
    }

    return(0);
}


