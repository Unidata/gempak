#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "pgprm.h"
#include "drwids.h"

#define NOT_INFILE	0
#define INFILE1		1
#define INFILE2		2

struct vgsav
{
	char	vg_type;
	char	vg_class;
	char	grptyp;
	int	grpnum;
	int	offset;
	int	fileflag;
};
	
struct vgsav _vgfSav[MAX_EDITABLE_ELEMS];

FILE   *_ifp;

void sfcsplt_crtvgf ( char *ifname, char *ofname, int isize, int vgfsvflg, 
			int numele, int *iret);
void sfcsplt_setgfileflg ( int inumele, int numlab, int *labgnums, 
			int num2lab, int *lab2gnums, int *iret );
void sfcsplt_setvgsav ( char *vgfilnam, int size, int *numlab, 
			int labgnums[], int *num2lab, int lab2gnums[], 
			int *numele, int *anyfile, int *iret );


/************************************************************************
 *									*
 * sfcsplt.c								*
 *									*
 * This module contains the main routine and functions of sfcsplt.	*
 * Sfcsplt creates a VGF for AFOS/AWIPS containing only hash marks,	*
 * fronts, trofs, drylines, and squall lines.  Also, tropical cyclone	*
 * symbols and associated group elements are included.  A second vgf	*
 * named fronts2.vgf is also created containing (1) fronts, trofs,	*
 * drylines, squall lines and all associated groups, (2) hash marks,	*
 * (3) high and low symbols and associated groups, and (4) tropical	*
 * cyclone symbols and associated groups.				*
 *									*
 * CONTENTS:								*
 *	main()		     	main program				*
 *	sfcsplt_crtvgf()	creates vg file of file flagged elements*
 *	sfcsplt_setgfileflg()  	sets fileflag of label grp text elements*
 *	sfcsplt_setvgsav()   	reads VGF and sets the structure _vgfSav*
 *									*
 * COMMAND LINE:							*
 * 	sfcsplt vgfilnam vgfpath					*
 *		vgfilnam	vg file name				*
 *		vgfpath		path of vg file including last /	*
 *				  (optional, if current path)		*
 *									*
 * EXAMPLE:								*
 *      sfcsplt sfcmmddhh.vgf						*
 *									*
 *          will create from sfcmmddhh.vgf a front vgf named		*
 *	    frontmmddhh.vgf for AFOS/AWIPS and a second front vgf named *
 *	    fronts2.vgf for internal use in the current directory.	*
 *									*
 ***********************************************************************/

/*=====================================================================*/

int main ( int argc, char **argv )
/************************************************************************
 * main 								*
 *									*
 * This is the main program of sfcsplt.					*
 *                                                                      *
 * main (argc, argv)							*
 * Input parameters:                                                    *
 *	argc		int      	Number of parameters		*
 *	argv		char**   	Parameter array of command line *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *									*
 **									*
 * Log:									*
 * F. J. Yen/NCEP	 9/98	Created					*
 * F. J. Yen/NCEP	 2/99	Handle label grptyp & SQLN. Remove file2*
 * F. J. Yen/NCEP	 3/99	Created second vgf named fronts2.vgf	*
 * F. J. Yen/NCEP	 5/99	Removed sgi86 warning messages.		*
 * A. Hardy/GSC          1/01   Changed _ifp from int -> FILE           *
 * D.W.Plummer/NCEP	 3/01	Added call to ces_gtrtbl		*
 ***********************************************************************/
{
    int		iret, ier, loglev, wrtflg;
    int		numele, inxtime, numlab, num2lab, anyfile, vgfsvflg;
    int		labgnums[MAX_EDITABLE_ELEMS];
    int		lab2gnums[MAX_EDITABLE_ELEMS];
    long	size;
    char	nfname[256], mmddhh[11], grp[4];
    char	vgfilnam[LLSCRN] = "\0";
    char	frontfilnam[LLSCRN] = "\0";
    char	frontbasenam[] = "front";
    char	fronts2filnam[] = "fronts2.vgf";
/*---------------------------------------------------------------------*/
    iret = 0;
    ier = 0;
    size = 0;

    /*
     *  Read group type table into ces structure.
     */
    ces_gtrtbl ( &ier );

    /*
     *  First check for proper number of input variables
     *  and then inquire if the vg file exists and get size
     */
    if ( argc <= 1 )  {
	printf("The command line for running %s is:\n", argv[0] );
	printf("    %s  vgfile  vgfpath\n", argv[0] );
	printf("        vgfile    Vector Graphics File (VGF) name\n" );
	printf("        vgfpath   path of vg files including ending /\n" );
	printf("                    (optional, if current path)\n" );
	exit (0);
    }
    else if ( argc == 2 ) {
        cfl_inqr ( argv[1], NULL, &size, nfname, &ier );
    }
    else {
	if (argv[2][strlen(argv[2]) - 1] != '/') {
	    printf (" path must end with /\n");
	exit (0);
	}
        cfl_inqr ( argv[1], argv[2], &size, nfname, &ier );
    }
    if ( ier == -1 ) {
	printf (" VGF file %s does not exist\n",argv[1]); 
	exit (2);
    }

    /*
     * Form first VG file name using path if given:
     */
    if (argc == 3) {
	strcpy (vgfilnam, argv[2]);
	strcpy (frontfilnam, argv[2]);
    }
    strcat(vgfilnam, argv[1]);
    strcat(frontfilnam, frontbasenam);
    /*
     * Get month, day, and hour from input VG filename to build
     * first output file name.
     */
    inxtime = strlen(vgfilnam) - 10;
    cst_ncpy (mmddhh, &vgfilnam[inxtime], 10, &ier);
    strcat (frontfilnam, mmddhh);

    /*
     * Open input VG file
     */
    loglev = 0;
    _ifp = 0;
    wrtflg = G_FALSE;
    cvg_open (vgfilnam, wrtflg, &_ifp, &ier);
    if (  (ier != 0) || ( _ifp == NULL) ) {
        strcpy (grp, "CVG");
	er_lmsg ( &loglev, grp, &ier, vgfilnam, &ier, strlen(grp),
			strlen(vgfilnam) );
        exit (0);
    }

    /*
     * Read the VG file and set the vgsav structure (Phase 1),
     * and the LABEL group number arrays:  labgnums and lab2gnums.
     */
    sfcsplt_setvgsav (vgfilnam, (int)size, &numlab, labgnums, 
	 	&num2lab, lab2gnums, &numele, &anyfile, &iret);

    if ( iret == 0 ) {

	if ( (numlab + num2lab) != 0 ) {
	    /*
	     * Set the file flag in the vgsav structure for
	     * additional elements in the LABEL group type 
	     * (Phase 2). 
	    */
	    sfcsplt_setgfileflg ( numele, numlab, labgnums,
		    num2lab, lab2gnums, &iret);
	}

	if ( anyfile != 1 ) {
	    printf ("No elements found for AFOS/AWIPS file\n");
	}
	else {
	    /*
	     * Create first front vgf (for AFOS/AWIPS).
	     */
            vgfsvflg = INFILE1;
	    sfcsplt_crtvgf ( vgfilnam, frontfilnam, (int)size, vgfsvflg,
		    numele, &ier);
	    if (ier == 0) {
		printf ("%s created\n", frontfilnam);
	    }
	}

	if ( anyfile == 0 ) {
	    printf ("No elements found for fronts2.vgf\n");
	}
	else {
	    /*
	     * Create fronts2.vgf.
	     */
            vgfsvflg = INFILE2;
	    sfcsplt_crtvgf ( vgfilnam, fronts2filnam, (int)size, vgfsvflg,
		    numele, &ier);
	    if (ier == 0) {
		printf ("%s created\n", fronts2filnam);
	    }
	}
    }
    /*
     * Close input vg file.
     */
    cvg_clos (_ifp, &ier);
    return 0;

}

/*=====================================================================*/

void sfcsplt_setgfileflg ( int numele, int numlab, int *labgnums, 
				int num2lab, int *lab2gnums, int *iret )
/************************************************************************
 * sfcsplt_setgfileflg                                                  *
 *                                                                      *
 * This function sets the fileflag for additional elements (CLASS_TEXT) *
 * for group type LABEL in the _vgfSav structure.  It is set to INFILE2	*
 * if it should be in file 2 only.  It is set to INFILE1 if it should	*
 * be in both files 1 and 2.  (File 1 is a subset of file 2.)		*
 *                                                                      *
 * void sfcsplt_setgfileflg ( numele, numlab, labgnums,	num2lab,	*
 * 		lab2gnums, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *	numele		int		Number of elements set in vgsav	*
 *	numlab		int		Number of elements in labgnums	*
 *	*labgnums	int		Array of grp no. for trp cyclone*
 *	num2lab		int		Number of elements in lab2gnums	*
 *	*lab2gnums	int		Array of grp no. for frnts/cntrs*
 *                                                                      *
 * Output parameters:                                                   *
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * F. J. Yen/NCEP	 2/99	Created					*
 * F. J. Yen/NCEP	 3/99	Renamed sfcsplt_setgaawflg. Add new file* 
 * F. J. Yen/NCEP	 5/99	Removed unnecessary arguement.		*
 * D.W.Plummer/NCEP	 3/01	Chg call from cpg_fndgrp to ces_gtgid	*
 ***********************************************************************/
{
    int		k, m, kret, igrptyp;
    char	labelgrptyp;
/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Get group type for LABEL
     */
    ces_gtgid ("LABEL", &igrptyp, &kret);
    labelgrptyp = (char) igrptyp;

    /*
     * Set fileflag for CLASS_TEXT elements
     */
    for ( k = 0; k < numele; k++) {
	if ( _vgfSav[k].vg_class == CLASS_TEXT ) {
	    if ( _vgfSav[k].grptyp == labelgrptyp ) {
		for ( m = 0; m < numlab; m++ ) {
		    if ( _vgfSav[k].grpnum == labgnums[m] ) {
			/*
			 * Set fileflag to INFILE1 for file 1 and 2 
			 */
	    		_vgfSav[k].fileflag = INFILE1;
			break;
		    }
		}
		for ( m = 0; m < num2lab; m++ ) {
		    if ( _vgfSav[k].grpnum == lab2gnums[m] ) {
			/*
			 * Set fileflag to INFILE2 for file 2 only
			 */
	    		_vgfSav[k].fileflag = INFILE2;
			break;
		    }
		}
	    }
	}
    }
}

/*=====================================================================*/

void sfcsplt_setvgsav ( char *vgfilnam, int size, int *numlab, 
			int labgnums[], int *num2lab, int lab2gnums[], 
			int *numele, int *anyfile, int *iret )
/************************************************************************
 * sfcsplt_setvgsav                                                     *
 *                                                                      *
 * This function reads the VG file and sets the _vgfSav structure for	*
 * phase 1.								*
 *                                                                      *
 * void sfcsplt_setvgsav ( vgfilnam, size, numlab, labgnums, num2lab,	*
 *			   lab2gnums, numele, anyfile, iret )		* 
 *                                                                      *
 * Input parameters:                                                    *
 *	*vgfilnam	char		vg filename			*
 *	size		int		size of VG file			*
 *                                                                      *
 * Output parameters:                                                   *
 *	*numlab		int		Number in array labgnums	*
 *	labgnums[]	int		Array of grp no. for trp cyclone*
 *	*num2lab	int		Number in array lab2gnums	*
 *	lab2gnums[]	int		Array of grp no. for frnts/cntrs* 
 *	*numele		int		Number of elements set in vgsav	*
 *	*anyfile	int		Flag for any elements in file	* 
 *	*iret		int		Return code			*
 *				 	 -1 = Error reading VG element	*
 *				 	 -2 = Error reading VG header	*
 *									*
 **									*
 * Log:									*
 * F. J. Yen/NCEP	 9/98	Created					*
 * F. J. Yen/NCEP	 2/99	Handle new label grptyp and add SQLN	*
 * F. J. Yen/NCEP	 3/99	Update for creation of fronts2.vgf	* 
 * A. Hardy/GSC          1/01   Changed _ifp from int -> FILE           *
 * D.W.Plummer/NCEP	 3/01	Chg call from cpg_fndgrp to ces_gtgid	*
 ***********************************************************************/
{

    int		ier, joffset, flag, kret;
    int		igrptyp;
    char	tropgrptyp, labelgrptyp, frntgrptyp;
    char	highgrptyp, lowgrptyp;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/
    *iret = 0;
    *numlab = 0;
    *num2lab = 0;
    *numele = 0;
    *anyfile = 0;
    ier = 0;
    joffset = 0;
    /*
     * Get group types 
     */
    ces_gtgid ("TROPICL", &igrptyp, &kret);
    tropgrptyp = (char) igrptyp;
    ces_gtgid ("LABEL", &igrptyp, &kret);
    labelgrptyp = (char) igrptyp;
    ces_gtgid ("FRONT", &igrptyp, &kret);
    frntgrptyp = (char) igrptyp;
    ces_gtgid ("HIGH", &igrptyp, &kret);
    highgrptyp = (char) igrptyp;
    ces_gtgid ("LOW", &igrptyp, &kret);
    lowgrptyp = (char) igrptyp;
    
    while (joffset < size) {
	cvg_rdhdr (vgfilnam, _ifp, joffset, size, &el, &flag, &ier);

	if (ier != 0 ) {
	    printf ("\n***ERROR reading header joffset=%i\n", joffset);
	    *iret = -2;
	    return;
	}
	_vgfSav[*numele].vg_type  = el.hdr.vg_type;
	_vgfSav[*numele].vg_class = el.hdr.vg_class;
	_vgfSav[*numele].grptyp   = el.hdr.grptyp;
	_vgfSav[*numele].grpnum   = el.hdr.grpnum;
	_vgfSav[*numele].offset   = joffset;
	_vgfSav[*numele].fileflag = NOT_INFILE;

	if ( el.hdr.grptyp == tropgrptyp ||
		    el.hdr.vg_type == HASH_ELM ) {
	    _vgfSav[*numele].fileflag = INFILE1;
	    *anyfile = 1;
	}
	else if ( el.hdr.grptyp == highgrptyp ||
		    el.hdr.grptyp == lowgrptyp ) {
	    _vgfSav[*numele].fileflag = INFILE2;
	    if ( *anyfile != 1 ) {
		*anyfile = 2;
	    }
	}
	else if ( el.hdr.grptyp == frntgrptyp ) {
	    if (el.hdr.vg_class == CLASS_FRONTS ||
		    el.hdr.vg_type == LINE_ELM) {
		_vgfSav[*numele].fileflag = INFILE1;
		*anyfile = 1;
	    }
	    else {
		_vgfSav[*numele].fileflag = INFILE2;
		if ( *anyfile != 1 ) {
                    *anyfile = 2;
		}
            }
	}
	else if ( el.hdr.vg_class == CLASS_FRONTS ) {
	    _vgfSav[*numele].fileflag = INFILE1;
	    *anyfile = 1;
	    if ( el.hdr.grptyp == labelgrptyp ) {
	        lab2gnums[(*num2lab)++] = el.hdr.grpnum;
	    }
	    else {
		printf (" Front is not FRONT nor LABEL group type\n");
	    }
	}
	else if ( el.hdr.vg_type == LINE_ELM ) {
	    cvg_rdele ( &el, joffset, el.hdr.recsz, _ifp, &ier );
	    if ( ier < 0 ) {
		printf ("\n***ERROR reading element joffset=%i\n",joffset);
		*iret = -1;
		return;
	    }
	    if ( el.elem.lin.info.lintyp == 8 ||
		    el.elem.lin.info.lintyp == 9 ) {
		_vgfSav[*numele].fileflag = INFILE1;
		*anyfile = 1;
		if ( el.hdr.grptyp != labelgrptyp ) {
		    printf (" SQUALL line is not FRONT nor LABEL"
				" group type\n");
		}
		else {
		    lab2gnums[(*num2lab)++] = el.hdr.grpnum;
		}
	    }
	}
	else if ( el.hdr.vg_type == SPSYM_ELM ) {
	    cvg_rdele ( &el, joffset, el.hdr.recsz, _ifp, &ier );
	    if ( ier < 0 ) {
		printf ("\n***ERROR reading element joffset=%i\n",joffset);
		*iret = -1;
		return;
	    }
	    if ( el.elem.sym.data.code[0] >= 24.99 &&
		    el.elem.sym.data.code[0] <= 28.01 ) {
		/*
		 * Tropical Cyclone symbol
		 */
		_vgfSav[*numele].fileflag = INFILE1;
		*anyfile = 1;
	        if ( el.hdr.grptyp != labelgrptyp ) {
	            printf (" Tropical CYCLONE is not TROPICL nor "
				"LABEL group type\n");
	        }
	        else {
 	            labgnums[(*numlab)++] = el.hdr.grpnum;
	        }
	    }
	    else if ( el.elem.sym.data.code[0] >= 11.99 &&
			el.elem.sym.data.code[0] <= 13.01 ) {
		/*
		 * High/Low symbol
		 */
		_vgfSav[*numele].fileflag = INFILE2;
		if ( *anyfile != 1 ) {
		    *anyfile = 2;
		}
	        if ( el.hdr.grptyp != labelgrptyp ) {
	            printf (" High/Low symbol is not HIGH nor "
				"LOW nor LABEL group type\n");
	        }
	        else {
 	            lab2gnums[(*num2lab)++] = el.hdr.grpnum;
	        }
	    }
	}
	(*numele)++;
	joffset += el.hdr.recsz;
    }
}
/*=====================================================================*/

void sfcsplt_crtvgf ( char *ifname, char *ofname, int isize, 
				int vgfsvflg, int numele, int *iret )
/************************************************************************
 * sfcsplt_crtvgf							*
 *                                                                      *
 * This function creates VG file based on flag vgfsvflg (1 for file 1	*
 * (AFOS/AWIPS) vgf and 2 for file2 (fronts2.vgf)			*
 *                                                                      *
 * void sfcsplt_crtvgf ( ifname, ofname, isize, vgfsvflg, numele, iret )*
 *                                                                      *
 * Input parameters:                                                    *
 *	*ifname		char		Input VG file name		*
 *	*ofname		char		Output VG file name		*
 *	isize		int		Size in bytes of input file	*
 *	vgfsvflg	int		VGF Front flag (file 1 or 2)	*
 *	numele		int		Number of elements set in vgsav	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*iret		int		Return code			*
 *				 	 -1 = Error opening VG file	*
 *				 	 -2 = Error closing VG file	*
 *					 -3 = Error creating VGF file
 *					 -4 = No input/output filename	*
 *					 -5 = Error writing to VG file	*
 *					 -6 = Error reading VG file	*
 *									*
 **									*
 * Log:									*
 * F. J. Yen/NCEP	 9/98	Created					*
 * F. Y. Yen/NCEP	 3/99	Update for fronts2.vgf. Always overwrite*	
 * A. Hardy/GSC          1/01   Changed _ifp from int -> FILE           *
 ***********************************************************************/
{
    int		    ier, ier1, ofpos, loglev, j;
    int		    wrtflg, flag, nbin;
    FILE	    *ofp;
    char	    grp[4];
    unsigned char   *buffer;

    VG_DBStruct	    el;
/*---------------------------------------------------------------------*/

    *iret = 0;
    loglev = 0;

    if ( !ifname || !ofname ) {
	*iret = -4;
	return;
    }

    /*
     * Create output VG file.  Always overwrite file if it exists.
     */

    cvg_crvgf (ofname, &ier);
    if (ier < 0) {
	ier = -15;
	strcpy (grp, "CVG");
	er_lmsg ( &loglev, grp, &ier, ofname, &ier1, strlen(grp),
                        strlen(ofname) );
	*iret = -3;
	return;
    }

    wrtflg = G_TRUE;
    cvg_open (ofname, wrtflg, &ofp, &ier);

    if ( (ier < 0) || (ofp == NULL) ) {
	strcpy (grp, "CVG");
	er_lmsg ( &loglev, grp, &ier, ofname, &ier1, strlen(grp),
			strlen(ofname) );
	*iret = -1;
	return;
    }

    ofpos  =  sizeof(FileHeadType) + sizeof(VG_HdrStruct);

    for ( j = 0; j <= numele-1; j++ ) {

	if ( _vgfSav[j].fileflag > 0 &&
		_vgfSav[j].fileflag <= vgfsvflg ) {
 
	    /*
	     * Read header
	     */

	    cvg_rdhdr (ifname, _ifp, _vgfSav[j].offset, 
			isize, &el, &flag, &ier);
		if (ier != 0){
		    strcpy (grp, "CVG");
		    er_lmsg ( &loglev, grp, &ier, ifname, &ier1, strlen(grp),
			    strlen(ifname) );
		    *iret = -6;
		    return;
		}
	    /*
	     * If the number of bytes is valid, continue.
	     */
	    if ( (el.hdr.recsz > 0) && (el.hdr.recsz < 65536) ) {

		if ( el.hdr.delete == 0 ) {
		  /*
		   *  Copy element directly
		   */
		  buffer = (unsigned char *)malloc(el.hdr.recsz);
		  cfl_seek (_ifp, _vgfSav[j].offset, 0, &ier);
		  cfl_read (_ifp, el.hdr.recsz, buffer, &nbin, &ier);
		  cfl_seek (ofp, ofpos, 0, &ier);
		  cfl_writ (ofp, el.hdr.recsz, buffer, &ier);
		  free (buffer);

		  ofpos += el.hdr.recsz;

		  if (ier != 0) {
		    strcpy (grp,"CFL");
        	    er_lmsg( &loglev, grp, &ier, ofname, &ier1, strlen(grp),
                                strlen(ofname));
		    *iret = -5;
		  }
		}
	    }
	}
    }
    cvg_clos (ofp, &ier);
    if ( ier != 0 ) {
        strcpy (grp, "CVG");
        er_lmsg( &loglev, grp, &ier, ifname, &ier1, strlen(grp),
                                strlen(ifname));
	*iret = -2;
    }
}
