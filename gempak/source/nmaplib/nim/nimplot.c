#include "nimcmn.h"

void nim_plot ( Pixmap pixmap, int index, char garea[], 
		char panel[], dttms_t dattim, dattm_t endtim, int mrange,
		int intrvl, int match, int minute, int ititl, int *iret)
/************************************************************************
 * nim_plot								*
 *									*
 * This routine plots the data associated with the specified index to 	*
 * the specified pixmap and panel for the given time.			*
 *									*
 * nim_plot ( pixmap, index, garea, panel, time, endtim, mrange, intrvl,*
 *	      match, minute, ititl, iret )				*
 *									*
 * Input parameters:							*
 *	pixmap		Pixmap		Pixmap for plotting the data	*
 *	index		int		Index to data info arrays	*
 *	garea[]		char		GEMPAK garea input		*
 *	panel[]		char		Panel location for the data	*
 *	dattim		dttms_t		Date/time for the data		*
 *	endtim		dattm_t		End time of range		*
 *	mrange		int		Minutes in time range		*
 *	intrvl		int		Minutes in time interval	*
 *	match		int		Flag for time match scheme	*
 *	minute		int		Number of min diff for match	*
 *	ititl		int		Title line			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					   -1 = invaid index value	*
 *									*
 **									*
 * S. Jacobs/NCEP	 6/99	Created					*
 * S. Law/GSC		01/00	added call to xscpxm			*
 * S. Law/GSC		06/00	MAXIMG -> MAXTMPLT			*
 * T. Piper/SAIC	12/01	properly typed endtim from dttms_t	*
 * T. Lee/SAIC		08/03	added time interval to calling sequence	*
 * T. Piper/SAIC	04/07	Incorporated nim_dspl functionality	*
 ***********************************************************************/
{
    int		ii, ier, idrpfl, ipos, nfound, ntimes;
    int		jclr, jtxfn, jtxhw, jtxwid, jbrdr, jrrotn, jjust;
    int		i_tmp, i_one=1, i_two=2, i_brdr=111;
    float	f_one=1.0, ztxt;
    char	**namarr=NULL, **timarr=NULL, imfile[FILE_FULLSZ];
    char	basetm[]=" ", path[LLPATH];
    char	ttlstr[80], clrbar[80];
    Boolean	bflag=FALSE, bauto=FALSE;
/*---------------------------------------------------------------------*/
/*
 *  Initialize output parameters.
 */
    *iret = 0;

/*
 *  Check for a valid index value.
 */
    if  ( ( image[index].type[0] != CHNULL )  &&
	( index >=  0 &&  index <= MAXTMPLT ) ) {

/*
 *  Check for a valid maximum number of times.
 */
        if ( mrange > 0 ) {

/*
 *  Get all file names for the given image type.
 */
            nim_flnm ( image[index].type, image[index].info,
                        basetm, endtim, mrange, intrvl, bflag,
                        bauto, path, &namarr, &timarr, &nfound, iret );
	    if ( *iret == G_NORMAL ) {
		if ( nfound > 0 ) {

/*
 *  Find the proper data date/time to plot.
 */
                    ntimes = nfound;  /*  Do not allow nfound to be changed!  */
                    cti_mtch ( match, (char*)dattim, (const char **)timarr,
					ntimes, minute, &ipos, &ier );
                    if  ( ipos >= 0 ) {

/*
 *  Set the output file name to the matched name.
 */
                        strcpy(imfile, path);
                        strcat(imfile, "/");
                        strcat(imfile, namarr[ipos]);

/*
 *  Set the requested pixmap.
 */
			xscpxm (pixmap, &ier);

/*
 *  Set the display panel.
 */
			gg_panl ( panel, &ier, strlen(panel) );
			gg_maps ( image[index].type, garea, imfile, &idrpfl,
					&ier, strlen(image[index].type),
					strlen(garea), strlen(imfile));
/*
 *  Drop the image.
 */
			im_drop ( &ier );
			if ( ( strncmp(image[index].info, "Global", 6) == 0 && 
				strstr(image[index].info, "Pol") != NULL ) || 
				( ((strncmp(namarr[ipos], "LI", 2) == 0 || 
				strncmp(namarr[ipos], "TP", 2) == 0) && 
				(strstr(namarr[ipos], "Conus") != NULL && 
				strstr(image[index].info,"SDPI") != NULL))) ) { 
			    strcpy(clrbar, "1/H/UL/0.05;0.05/0.90");
			    im_cbar ( clrbar, &ier, strlen(clrbar) );
			}
			else {
			    strcpy(clrbar, "1/V/LL/0.0;0.05/0.90");
			    im_cbar ( clrbar, &ier, strlen(clrbar) );
			}
/*
 *  Create and plot the title string.
 */
			gqcolr  ( &jclr, &ier );
			gqtext  ( &jtxfn, &jtxhw, &ztxt, &jtxwid, &jbrdr,
						&jrrotn, &jjust, &ier );

			i_tmp = 31;
			gscolr  ( &i_tmp, &ier );
			i_tmp = 21;
			gstext  ( &i_tmp, &i_two, &f_one, &i_one, &i_brdr,
						&i_one, &i_one, &ier );

			gg_sttl ( ttlstr, &ier, sizeof(ttlstr) );
			gg_wstr ( ttlstr, &ititl, &ier, sizeof(ttlstr) );

			gscolr  ( &jclr, &ier );
			gstext  ( &jtxfn, &jtxhw, &ztxt, &jtxwid, &jbrdr,
						&jrrotn, &jjust, &ier );
		    }
                    else {
                        *iret = +1;
                    }
                }
                else {
                    *iret = -3;
                }
            }
            for (ii=0; ii < nfound; ii++) {
                G_FREE(namarr[ii], char);
                G_FREE(timarr[ii], char);
            }
            if ( namarr != NULL ) G_FREE(namarr, char*);
            if ( timarr != NULL ) G_FREE(timarr, char*);

        }
        else {
            *iret = -2;
        }
    }
    else {
        *iret = -1;
    }
}
