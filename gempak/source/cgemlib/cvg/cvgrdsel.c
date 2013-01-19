#include "cvgcmn.h"
#include "pgprm.h"


void cvg_rdsel ( char *fname, int fpos, float x, float y, int *hotpt, 
			float *distance, VG_DBStruct *el, int *iret )
/************************************************************************
 * cvg_rdsel								*
 *									*
 * This function reads in a record from a vector graphics file only	*
 * based upon the contents of the Graphics Infor record, which		*
 * specifies the requested element number, and the file name.		*
 *									*
 * cvg_rdsel ( fname,fpos,x, y, hotpt, distance, el, iret )		*
 *									*
 * Input parameters:							*
 *	*fname		char 		File name to read from		*
 *	fpos		int		File position selected		*
 *	x		float		X coordinate of point to locate *
 *	y		float		Y coordinate of point to locate	*
 *									*
 * Output parameters:							*
 *	*hotpt		int		Index to closest point		*
 *      *distance	float		distance to closest point	*
 *	*el		VG_DBStruct	Pointer to VG record structure	*
 *	*iret		int		Return code			*
 *					  1 = distance to hotpt         *
 *						exceeds tie-in		*
 *					 -1 = error opening VG file	*
 *					 -2 = error closing VG file	*
 *					-13 = error reading VG header 	*
 *					-14 = error reading VG element 	*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	11/96		Created				*
 * D. Keiser/GSC	 1/97		Clean up			*
 * E. Wehner/EAi	2/97	cgr_distance -> cgr_dist		*
 * D.W.Plummer/NCEP	 9/97	Changes for new vgstruct header file	*
 * E. Wehner/EAi	 9/97	Remove grinfo, use parms for fname	*
 * F. J. Yen/NCEP	11/97	Replace " " with NULL in invocation of	*
 *				cfl_inqr.				*
 * G. Krueger/EAI	 1/98	Ignore non-fatal read warnings.		*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * E. Safford/GSC	10/98	use WORK_FILE & use cvg_rdrec for read  *
 * T. Piper/GSC		10/98	Prolog update				*
 * E. Safford/GSC	02/01	add tie-in distance check & return code *
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 ***********************************************************************/
{
    int 	ier, np;
    float	dist;
    float	dx[MAXPTS], dy[MAXPTS];
    char	grp[4];
    char	reqfil[133];

/*---------------------------------------------------------------------*/

    *iret  =  0;
    dist   = *distance = -99.0F;

    np = 1;
    strcpy(grp, "CVG");

    if (!fname) {
        strcpy(reqfil, work_file); 
    }
    else {
	strcpy(reqfil, fname);
    }

    cvg_rdrec (reqfil, fpos, el, iret);

    if ( *iret < 0 ) {
        el    = (VG_DBStruct *) NULL;
	hotpt = NULL;
    }

    /*
     *  Locate the nearest point & return as hotpt. 
     */
    if ( ( x > 0.0F ) && ( y > 0.0F ) ) {

        /*
         *  Convert the element vertices to D coords then check distance
	 *  to the line segment.
         */
    	cvg_todev( el, &np, dx, dy, &ier);
        cgr_dist(np, dx, dy, x, y, &dist, hotpt, &ier);
        *distance = dist; 
    }

    /*
     *  If distance exceeds vertex tiein, return warning
     */
    if ( dist > VERTEX_TIEIN ) {
        *iret = 1;
    } 
      

}
