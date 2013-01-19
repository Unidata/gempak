#include "geminc.h"
#include "gemprm.h"

typedef struct
{
    char	sat_id[21];
    char	imgtyp[9];
    int		minpix;
    int		maxpix;
    int		satnum;
    int		ichnnl;
    int		imdpth;
    char	lutfil[15];
} Imgtbl_t;

#define	IMGTYP_TBL	"imgtyp.tbl"

void im_rtbl ( int *iret )
/************************************************************************
 * im_rtbl								*
 *									*
 * This subroutine returns information about the image from the image	*
 * type table file.							*
 *									*
 * void im_rtbl ( *iret )						*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 +1 = No entry found for image	*
 *					 -7 = Couldn't open imtyp table	*
 **									*
 * Log:									*
 * J. Cowie/COMET	 2/95						*
 * J. Cowie/COMET	 4/95	Changed calling sequence		*
 * J. Cowie/COMET	10/95	Set min/max pixvals & default cmlutf	*
 * D. Keiser/GSC	12/95	Changed FL_TOPN to FL_TBOP		*
 * J. Cowie/COMET	 1/97	Changed IMGDEF common variable names	*
 * J. Cowie/COMET	12/97	Replaced imsorc check with imradf,	*
 *				removed immode, use imdpth for imraw	*
 * Chiz/Unidata          5/00   Add check for ** in table jptyp		*
 * T. Piper/SAIC	09/06	Re-wrote in 'C'; only read table once	*
 ***********************************************************************/
{
    int			found, ier, ier1, ier2, imax, imdpth, imin,
			imradf, imsorc, imtype, ip1, ip2, lens,
			loglev=2;
    char		chnnl[7], chnnl1[7], *cptr, *grp="IM",
			line[128], lutfil[16];
    size_t		ii;
    static int		nr;
    static FILE		*fptr=NULL;
    static Imgtbl_t	*imgtbl=NULL;
/*---------------------------------------------------------------------*/
/*
 *  If table not yet read into memory; open the image type table file.
 */
    if ( fptr == NULL ) {
	fptr = cfl_tbop ( IMGTYP_TBL, "sat", &ier );
	if ( ier != 0 ) { 
	    *iret = -7;
	    er_lmsg (&loglev, grp, iret, IMGTYP_TBL, &ier1,
				strlen(grp), strlen(IMGTYP_TBL));
	    return;
	}
/*
 *  Load the structure.
 */
	cfl_tbnr (fptr, &nr, &ier );
	if ( ier != 0 || nr == 0 ) {
	    *iret = -7;
	    er_lmsg (&loglev, grp, iret, IMGTYP_TBL, &ier1,
				strlen(grp), strlen(IMGTYP_TBL));
	    cfl_clos(fptr, &ier);
	    return;
	}
	G_MALLOC(imgtbl, Imgtbl_t, nr, "im_rtbl:  imgtbl");
	for (ii = 0; ii < (size_t)nr; ii++ ) {
/*
 *  Read the next record.
 */
	    cfl_trln ( fptr, 127, line, &ier );
	    if ( ier == 0 ) {
		sscanf(line,"%20c %s %i %i %i %s %i %s",
			imgtbl[ii].sat_id, imgtbl[ii].imgtyp,
			&imgtbl[ii].minpix, &imgtbl[ii].maxpix,
			&imgtbl[ii].satnum, chnnl,
			&imgtbl[ii].imdpth, imgtbl[ii].lutfil);
		st_null(imgtbl[ii].sat_id, imgtbl[ii].sat_id,
			&lens, &ier, 20, 21);
		cst_numb(chnnl, &imgtbl[ii].ichnnl, &ier );
/*
 *  If not a standard number, check to see if it contains **.
 */
                if ( ier != 0 ) {
		    cptr = strstr(chnnl, "**" );
		    if ( cptr != NULL ) { 
			strcpy(chnnl1, chnnl);	
			chnnl1[cptr - chnnl] = '\0';
			cst_numb(chnnl1, &ip1, &ier1);
			cst_numb(cptr+2, &ip2, &ier2);
			if ( ( ier1 == 0 ) && ( ier2 == 0 ) ) {
			    imgtbl[ii].ichnnl = pow(ip1, ip2);
		        }
			else {
			    imgtbl[ii].ichnnl = IMISSD;
			}
		    }
		}
	    }
	}  /*  end of 'for (ii = 0; ii < nr; ii++ )'  */
	cfl_clos(fptr, &ier);
    }  /*  end of 'if ( *fptr == NULL )'  */

/*
 *  See if this is our image type.
 */
    ii = 0;
    found = G_FALSE;
    im_qimg(&imradf, &imsorc, &imtype, &imdpth, &ier);
    while ( !found && ii < (size_t)nr ) {
	if  ( ( imsorc == imgtbl[ii].satnum ) && 
     	      ( imtype == imgtbl[ii].ichnnl ) && 
     	      ( imdpth == imgtbl[ii].imdpth ) ) {
	    im_smgd(imgtbl[ii].sat_id, imgtbl[ii].imgtyp,
			&imgtbl[ii].minpix, &imgtbl[ii].maxpix,
			imgtbl[ii].lutfil, &ier,
			strlen(imgtbl[ii].sat_id),
			strlen(imgtbl[ii].imgtyp),
			strlen(imgtbl[ii].lutfil)); 
	    found = G_TRUE; 
	    *iret = G_NORMAL;
	}
	ii++;
    }
/*
 *  Check that a match was found.  If not, set pixel value
 *  range for radar and sat.
 */
    if ( ! found ) { 
	imin = 0;
	if ( imradf == 1 ) {
	    imax = 15;
	    strcpy(lutfil, "osf_ref16.tbl");
	    im_smgd(NULL, NULL, &imin, &imax, lutfil, &ier, 0, 0, strlen(lutfil));
	}
	else {
	    imax = 255;
	    strcpy(lutfil, "GRAY");
	    im_smgd(NULL, NULL, &imin, &imax, lutfil, &ier, 0, 0, strlen(lutfil));
	}
	*iret = 1;
    }
}
