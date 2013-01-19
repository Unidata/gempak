#include "geminc.h"
#include "gemprm.h"
#include "hints.h"
#include "vgstruct.h"
#include "pgprm.h"
#include "pgcmn.h"
#include "Nxm.h"
#include "nmap_data.h"


#define NIDS_TBL      "nidsprod.tbl"

static char	_nidsCode[16];

typedef struct  {
        char    code[3];       /* NIDS code    */
        char    codeStr[64];     /* NIDS code description  */
} NIDSinfo;

typedef struct {
        int      nver;           /* Number of NIDS codes       */
        NIDSinfo *info;          /* NIDS code & description    */
} NIDScode_t;

NIDScode_t	_codeTbl;

/************************************************************************
 * nmap_nidstbl.c		      					*
 *									*
 * This module read in the nids product code table for data selection	*
 *									*
 * CONTENTS:								*
 *	nids_loadTable()	Load NIDS code  table			*
 *	nids_getLabel()		Return NIDS descriptor			*
 ***********************************************************************/

/*=====================================================================*/


void nids_loadTable ( int *iret )
/************************************************************************
 * nids_loadTable                                                      *
 *                                                                      *
 * This function loads the aodtvers.tbl table into its structure.       *
 *                                                                      *
 * nids_readTable ( iret )                                             *
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 * 	*iret           int     Return code                             *
 *                                -1 - Unable to open table      	*
 *                                -2 - No valid records in table 	*
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		06/05						*
 ***********************************************************************/
{
FILE    *fp;
int     ii, nn, nr, ier;
char    buffer[256];

/*---------------------------------------------------------------------*/
        *iret = G_NORMAL;

        /*
         *  Open the aodtvers table. If not found return an error.
         */

        fp = (FILE *)cfl_tbop( NIDS_TBL, "nmap", &ier);
        if ( fp == NULL  ||  ier != 0 )  {
                *iret = -1;
                return;
        }

        cfl_tbnr(fp, &nr, &ier);
        if ( ier != 0 || nr == 0 ) {
            *iret = -2;
            return;
        }

        _codeTbl.info = (NIDSinfo *)malloc( nr * sizeof(NIDSinfo) );
	for ( ii = 0; ii < nr; ii++ ) {
	    _codeTbl.info[ii].code[0]    = CHNULL;
	    _codeTbl.info[ii].codeStr[0]  = CHNULL;
	}

	rewind(fp);
        nn  = 0;
        while ( nn < nr && !feof(fp) ) {

            cfl_trln( fp, 256, buffer, &ier );
            if ( ier != 0 ) break;

            sscanf( buffer, "%s %[^\n]", _codeTbl.info[nn].code, _codeTbl.info[nn].codeStr );
	    nn++;
        }

        cfl_clos(fp, &ier);

        _codeTbl.nver = nn;

}

/*=====================================================================*/


char *nids_getLabel ( char *dirnam)
/************************************************************************
 * nids_getLabel                                                        *
 *                                                                      *
 * This function queries the number of AODT versions and the maximum    *
 * length of the version names in table aodtvers.tbl.                   *
 *                                                                      *
 * nids_getLabel ( dirname )                                            *
 *                                                                      *
 * Input parameters:                                                    *
 * 	*dirnam    char    Directory name                               *
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 * Log:                                                                 *
 * M. James/Unidata     10/11                                           *
 ***********************************************************************/
{
int     ii, nv;
/*---------------------------------------------------------------------*/
	nv = _codeTbl.nver;
        for ( ii = 0; ii < _codeTbl.nver; ii++ ) {
	    if ( strstr ( _codeTbl.info[ii].code, dirnam ) != 0 ) {
		return _codeTbl.info[ii].codeStr; 
	    }
        }
	return dirnam;
}

/*=====================================================================*/
