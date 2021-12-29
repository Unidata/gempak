#include "nwx_cmn.h"

/************************************************************************
 * nwxp_txtw.c                                                          *
 *                                                                      *
 * This module provides some utilities associated with the text window. *
 *                                                                      *
 * CONTENTS:                                                            *
 *      txtw_dttmSet()  set date/time info in text window.        	*
 *	txtw_prnt()	print the input text to system's default printer*
 ***********************************************************************/

/*=====================================================================*/

void txtw_prnt( char *text, int *iret ) 
/************************************************************************
 * txtw_prnt                                                            *
 *                                                                      *
 * This routine sends the input text string to the default system       *
 * printer.								*
 *                                                                      *
 * txtw_prntbCb ( text, iret )                                      	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *text           char 	text to print				* 
 * Output parameters:							*
 *	*iret		int	return code				*
 *				  0 = Normal				*
 **                                                                     *
 * E. Safford/SAIC	12/07	extracted from txtw_prntbCb()		*
 ***********************************************************************/
{
int             ier_fwrite, ier1;

char            filnam[73], report[ REPMAX ]; 
FILE            *fpout;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;
/*
 * Open the temporary file.
 */
        strcpy( filnam, "/tmp/nwx.prnt" );
        fpout = cfl_wopn( filnam, &ier1 );
        if  ( ier1 != 0 ) {
	    *iret = -1;
	    return;
	}
	fprintf ( fpout, "\n" );

/*
 * Write the text to the temporary file, or get the stored reportText if
 * the input text is null.
 */
        if ( text[0] ) {
            ier_fwrite = (int)fwrite( text, sizeof(char), strlen(text),
                        fpout );
        }
        else {
            pdata_getReportText( report, &ier1 );
	    if( ier1 <= G_NORMAL ) {
                ier_fwrite = (int)fwrite( report, sizeof(char), strlen(report), fpout );
            }
        }

        cfl_clos ( fpout, &ier1 );
	if( ier1 != G_NORMAL ) {
	    *iret = -2;
	}
	fpout = NULL;

/*
 * Print the temporary file to the default printer.
 *
 * This check is != 0 because fwrite returns the number of bytes written.
 */
	if ( ier_fwrite != 0 ) {
	    system ( "$LP /tmp/nwx.prnt" );

/*
 * Remove the temporary file.
 */
	    unlink( filnam );

	}
}

/*======================================================================*/

void txtw_dttmSet ( char *filnme )
/************************************************************************
 * txtw_dttmSet                                                         *
 *                                                                      *
 * This function stores the date from the filename.  Note that this     *
 * function does not display the new date.  Use txtw_dsplyDateTime to   *
 * do that.                                                             *
 *                                                                      *
 * void txtw_dttmSet (filnme)                                           *
 *                                                                      *
 * Input parameters:							*
 *	*filnme		char	file name string			*
 **                                                                     *
 * Log:                                                                 *
 * L. Williams/EAI      08/95                                           *
 * C. Lin/EAI		09/95  modify to use srchInfo			*
 *			       change function name			*
 * D.W.Plummer/NCEP	 9/96	added "O" data type processing		*
 * S. Jacobs/NCEP	 4/99	Changed all years to 4 digits		*
 * D. Kidwell/NCEP	 4/99	Changed iyr < 20 to iyr <= 20           *
 * T. Piper/SAIC	 5/02	Made filnme an input argument		*
 * D. Kidwell/NCEP	 9/02	Added check for SFC_HRLY data type      *
 * M. Mainelli/TPC	11/03	Added check for SYN_DATA data type	*
 * T. Piper/SAIC	04/05	Added check for SND_DATA data type      *
 * E. Safford/SAIC	12/07	moved func to nwxp_txtw.c		*
 * B. Hebbard/NCEP	01/21	Changed century breakpoint from		*
 *				2020/2021 to 2040/2041 (iyr)		*
 ***********************************************************************/
{
char *months[]={"JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG",
                  "SEP", "OCT", "NOV", "DEC"};
char date[30];
char *datatype;
int  iarr[2], num, ier, iyr, imn, idy, ihr;

/*---------------------------------------------------------------------*/

        datatype = nwxTable->dtyp_info[srchInfo.idtyp].datatyp;

	if  ( ( srchInfo.smethod == OBS )  &&
	    ( ( strcmp ( datatype, "SFC_HRLY" ) == 0 ) ||
	      ( strcmp ( datatype, "SND_DATA" ) == 0 ) ||
	      (	strcmp ( datatype, "SYN_DATA" ) == 0 ) ) ) {
	    cst_ilst ( filnme, '_', IMISSD, 2, iarr, &num, &ier );

	    idy = iarr[0] % 100;
	    imn = ( iarr[0] / 100 ) % 100;
	    iyr = iarr[0] / 10000;
	    if  ( iyr <= 40 )  iyr += 2000;
	    if  ( iyr < 100 )  iyr += 1900;

  	    sprintf ( date, "%02d %s %04d", idy, months[imn-1], iyr );

	}
	else {
	    cst_ilst ( filnme, '.', IMISSD, 2, iarr, &num, &ier );

	    ihr = iarr[0] % 100;
	    idy = ( iarr[0] / 100 ) % 100;
	    imn = ( iarr[0] / 10000 ) % 100;
	    iyr = iarr[0] / 1000000;
	    if  ( iyr <= 40 )  iyr += 2000;
	    if  ( iyr < 100 )  iyr += 1900;

  	    sprintf ( date, "%02d GMT %02d %s %04d",
		      ihr, idy, months[imn-1], iyr );

	}

        pdata_setDateTime( date, &ier );

}
