#include "nimcmn.h"

static int compare ( const void *a, const void *b );

void nim_flnm ( const char *imgtyp, const char *imginf,
		 char *basetm, char *endtim, int mrange,
		 int intrvl, Boolean bflag, Boolean bauto, char *path,
		 char ***namarr, char ***timarr, int *ntimes, int *iret )
/************************************************************************
 * nim_flnm								*
 *									*
 * This routine returns an array of file names given the image info.	*
 * The type of image, SAT or RAD, and the image info, which is the	*
 * directory of the data, are used to locate the image data files on 	*
 * disk. For satellite images, the info is the part of the directory	*
 * path that defines the source/geog_area/channel. For radar images,	*
 * the info is lcl_or_natl/area/type.					*
 *									*
 * void nim_flnm ( imgtyp, imginf, basetm, endtim, mrange, intrvl,	*
 *		   bflag, bauto, path, namarr, timarr, nfound, iret )	*
 *									*
 * Input parameters:							*
 *      *imgtyp         const char      Type of image                   *
 *      *imginf         const char      Directory information           *
 *	*basetm		char		Base time			*
 *	*endtim		char		End time of range		*
 *	mrange		int		Minutes in time range		*
 *	intrvl		int		Minutes in time interval	*
 *	bflag		Boolean		Reference time flag		*
 *	bauto		Boolean		Auto update flag		*
 *									*
 * Output parameters:							*
 *	*path		char		Full directory path		*
 *	***namarr	char		Array of all file names		*
 *	***timarr	char		Array of all file times		*
 *	*ntimes		int		Number of files/times returned	*
 *	*iret		int		Return code			*
 *					  -3 = no files found		*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 8/99	Created					*
 * S. Jacobs/NCEP	 2/00	Use the path and templ from datatype.tbl*
 * S. Jacobs/NCEP	 7/00	Moved error check to after FL_SCND call	*
 * S. Jacobs/NCEP	 8/00	Added check: len of tmpl = len of file	*
 * S. Jacobs/NCEP	 3/01	Increased file template to 48 chars	*
 * S. Jacobs/NCEP	 9/01	Ignore the non-date part of the template*
 * A. Hardy/SAIC         2/02   Changed call FL_SCND			*
 * T. Lee/SAIC		 6/02	Handled wild card template; return ipos	*
 * T. Lee/SAIC		 9/04	Replaced FL_TMPL with CTB_DTGET		*
 * A. Hardy/NCEP	11/04   Added calls to ST_RNUL			*
 * m.gamazaychikov/SAIC 12/04   Added ion flag to CTB_DTGET CS          *
 * m.gamazaychikov/SAIC 01/06   Changed ctmpl string length to MXTMPL   *
 * m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
 * T. Piper/SAIC	04/07	Re-wrote in 'C'; dynamic arrays		*
 * T. Piper/SAIC	10/07	Check value of nt before cti_tmln	*
 * F. J. Yen/NCEP	 4/08	Add bin mins and mstrct to CTB_DTGET CSC*
 ***********************************************************************/
{
    long istar;
    int ic, is, nf, ir, ii, kk, ion, ihb, iha, idtmch, iorder, lens;
    int imb, ima, mstrct;
    int ier, ier1, ier2, itarr[5], jtarr[5], lenf, lent, mdif, ndif, nt;
    int idir=-1, idelrt=-1, nfiles; 
    size_t timlen=DTTMS_SIZE;
/*
 *  The following dimensions are based upon the sizes in the datatype.tbl
 *  plus one for the null character; then rounded up to the neartest
 *  multiple of four.
 */
    char *pstar, **t_names=NULL, **t_times=NULL, **tarr=NULL;
    char cpath[28], ctmpl[MXTMPL], tmplt[MXTMPL];
    dattm_t strtim;
    nmlst_t tfil;
    struct dirent **dnlist=NULL;
/*---------------------------------------------------------------------*/
/*
 *  Construct the directory name from the image data attributes.
 */
    ctb_dtget ( (char*)imgtyp, cpath, ctmpl, &ic, &is, &nf, &ir, &ii,
                 &ion, &ihb, &imb, &iha, &ima, &mstrct, &idtmch, &ier );
    strcpy ( path,  cpath );
    strcat ( path,   "/"  );
    strcat ( path, imginf );
/*
 *  Scan the directory.
 */
    iorder = 1;
    cfl_scnt ( path, ctmpl, iorder, &dnlist, &nfiles, &ier );
/*
 *  If there are no files, there is an error.
 */
    if ( nfiles > 0 ) {
/*
 *  Construct the full file name template for this directory.
 */
	pstar = strchr(ctmpl, '*' );
	if ( pstar == NULL ) {
/*  No '*' was found in the template.   */
	    strcpy(tmplt, ctmpl);
	}
	else {
/*  An '*' was found in the template.  */
	    istar = ( pstar - ctmpl );
	    if ( istar == 0 ) {
/*  An '*' was found at the beginning of the template.  */
		strcpy(tmplt, (ctmpl+2));
	    }
	    else {
		cst_ncpy(tmplt, ctmpl, istar, &ier);
	    }
	}
/*
 *  Convert each file name to a time.
 */
	cst_lstr(tmplt, &lent, &ier );
        cst_lstr(dnlist[0]->d_name, &lenf, &ier );
	G_MALLOC(tarr, char*, nfiles, "nim_flnm:  tarr");
	for ( ii = 0; ii < nfiles; ii++ ) {
	    G_CALLOC(tarr[ii], char, timlen, "nim_flnm:  tarr[ii]");
	    if ( pstar == NULL ) {
	        cfl_mdat(dnlist[ii]->d_name, tmplt, "YYMMDD/HHNN",
							tarr[ii], &ier );
	    }
	    else {
		if ( istar == 0 ) {
		    strcpy(tfil, dnlist[ii]->d_name+(lenf-lent));
	    	}
	        else {
		    cst_ncpy(tfil, dnlist[ii]->d_name, lent, &ier);
	        }
		cfl_mdat(tfil, tmplt, "YYMMDD/HHNN", tarr[ii], &ier );
	    }
	}

/*
 *   Sort the times.
 */
	qsort( (void*)tarr, (size_t)nfiles, sizeof(char*), compare ); 
	cti_yyyy(nfiles, tarr, tarr, &ier);

/*
 *  Compute the start time of the range from the end time
 *  and the number of minutes.
 */
        ti_ctoi ( endtim, itarr, &ier, strlen(endtim) );
        ti_subm ( itarr, &mrange, jtarr, &ier );
        ti_itoc ( jtarr, strtim, &ier, sizeof(strtim) );
	st_null ( strtim, strtim, &lens, &ier, sizeof(strtim), sizeof(strtim) );
/*
 *  Find the number of times to return.
 */
        nt = 0;
	for ( ii = 0; ii < nfiles; ii++ ) {
	    ti_diff ( strtim, tarr[ii], &mdif, &ier1, strlen(strtim), timlen-1 );
            ti_diff ( tarr[ii], endtim, &ndif, &ier2, timlen-1, strlen(endtim));
	    if ( ( ier1 == 0 ) && ( ier2 == 0 ) ) {
                if ( ( mdif <= 0 ) && ( ndif <= 0 ) ) {
		    G_REALLOC(t_names, char*, nt+1, "nim_flnm:  t_names");
		    G_MALLOC(t_names[nt], char, (strlen(dnlist[ii]->d_name)+1),
							"nim_flnm:  t_names[nt]");
		    strcpy(t_names[nt], dnlist[ii]->d_name);
		    G_REALLOC(t_times, char*, nt+1, "nim_flnm:  t_times");
		    G_MALLOC(t_times[nt], char, (strlen(tarr[ii])+1),
							"nim_flnm:  t_times[nt]");
                    strcpy(t_times[nt], tarr[ii]);
		    nt++;
		}
	    }
	    G_FREE(tarr[ii], char);
	    free (dnlist[ii]);
	}
	G_FREE(tarr, char*);
	if ( dnlist != NULL ) free (dnlist);
/*
 *  Return requested times based on time range and interval.
 */
	if ( nt > 0 ) {
	    cti_tmln ( t_times, nt, mrange, intrvl, idir, (int)bflag,
			(int)bauto, basetm, endtim, &idelrt, t_times, ntimes, iret );
	    if ( *ntimes < nt ) {
		for (kk=*ntimes; kk<nt; kk++) {
		    G_FREE(t_names[kk], char);
		    G_FREE(t_times[kk], char);
	        }
	    }
	    qsort( (void*)t_times, (size_t)*ntimes, sizeof(char*), compare );
	    cti_yyyy(*ntimes, t_times, t_times, &ier );
	    *namarr = t_names;
	    *timarr = t_times;
	    *iret = G_NORMAL;
	}
	else {
	    *ntimes = 0;
	    *iret = -3;
	}
    }
    else {
	*ntimes = 0;
	*iret = -3;
    }
}

/*====================================================================*/

static int compare ( const void *a, const void *b )
{
    return  strcmp( *(char **)a, *(char **)b );
}
