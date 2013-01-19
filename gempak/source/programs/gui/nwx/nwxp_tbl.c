#include "nwx_cmn.h"


#define MAP_TBL      "mapinfo.nwx"
#define MASTER_TBL   "master.tbl"
#define TABLE_TYPE   "nwx"

int     _cmp_bulls ( struct bulletin_list    *bull1, 
		     struct bulletin_list    *bull2 );

nwxtbl_t *nwxTable;

/************************************************************************
 * nwxtbl.c                                                             *
 *                                                                      *
 * This module deals with nwx tables.         				*
 *                                                                      *
 * CONTENTS:                                                            *
 *      nwxtbl_init()    read the nwx tables and initialize the         *
 *				structure.              		*
 *      nwxtbl_sdtyp()   search for the data type in the structure.     *
 *      nwxtbl_getstns() read related station table and save in the     *
 *				structure.    				*
 *      _cmp_bulls()     sort bulletin structure based on station ID	*
 ***********************************************************************/

/*=====================================================================*/

int nwxtbl_init ( void )
/************************************************************************
 * nwxtbl_init								*
 *									*
 * This routine will read the data type table and map table into        *
 * the global nwxTable structure.					*
 *									*
 * int nwxtbl_init ( )							*
 *									*
 * Output parameters:							*
 *	nwxtbl_init		int	 0 -- successful		*
 *		       			-1 -- table reading error	*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 7/94						*
 * S. Jacobs/NMC	10/94		Changed NWX_TABLES to		*
 *						NAWIPS_TABLES/nwx	*
 * C. Lin/EAI		 8/95						*
 * T. Piper/SAIC	01/04	removed tbldir and tblfile		*
 ***********************************************************************/
{
int	maxnum, iret;

/*---------------------------------------------------------------------*/
	iret = G_NORMAL;

/*
 * allocate space for nwxTable
 */
	nwxTable = (nwxtbl_t *)malloc(sizeof(nwxtbl_t));

/*
 * Set the data type table name, and maximum array size.
 */
	maxnum = MAXTYP; 

/*
 * Read the contents of the master table into the structure.
 */
	ctb_rdtyp( MASTER_TBL, TABLE_TYPE, &maxnum, 
		&(nwxTable->ndtyp), nwxTable->dtyp_info, &iret );
	if (iret != 0) return (-1);

/*
 * Set the map type table name, and maximum array size.
 */
	maxnum = MAXTYP;

/*
 * Read the contents of the map table into the structure.
 */
	ctb_rmtyp( MAP_TBL, TABLE_TYPE, &maxnum, 
		&(nwxTable->nmap), nwxTable->map_info, &iret );
	if ( iret != 0 ) return( -1 );

	return( 0 );
}

/*=====================================================================*/

int nwxtbl_sdtyp ( char *datatype )
/************************************************************************
 * nwxtbl_sdtyp                                                     	*
 *                                                                      *
 * This function searches the nwxtbl structure for matching data        *
 * type.								*
 *									*
 * int nwxtbl_sdtyp ( datatype )					*
 *									*
 * Input parameters:							*
 *	*datatype	char		data type name to be searched 	*
 *									*
 * Return parameters:							*
 *	nwxtbl_sdtyp	int	       -1 -- not found			*
 *	     >= 0 -- the index of the data type found in the nwxtbl str	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * L. Williams          6/95                                            *
 * C. Lin               8/95 	                   			*
 ***********************************************************************/
{
    int jj;

/*---------------------------------------------------------------------*/

    for (jj=0; jj<nwxTable->ndtyp; jj++) {
	if (strcmp(nwxTable->dtyp_info[jj].datatyp, datatype) == 0)
	    return(jj);
    }
    return(-1);
}

/*=====================================================================*/

void nwxtbl_getstns ( int inxdt, stnlist_t *stns, int *iret )
/************************************************************************
 * nwxtbl_getstns							*
 *									*
 * This routine will read the location information from either a	*
 * station table or a bulletin station.  The info retured as the  	*
 * station-list structure.						*
 *									*
 * nwxtbl_getstns ( inxdt, stns, iret ) 			        *
 *									*
 * Input parameters:							*
 *	inxdt		int		Index to selected data type	*
 *									*
 * Input/Output parameters:						*
 *	*stns	        stnlist_t       station list structure		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 7/94						*
 * L. Williams/EAI	12/94	Add check for new data type		*
 * C. Lin/EAI		 8/94						*
 * D.W.Plummer      	12/95	Add "W"-type data type			*
 * D.W.Plummer      	12/95	Add "S"-type data type			*
 * D.W.Plummer      	 2/96	Add "M"-type data type for MOS		*
 * D.W.Plummer      	 2/96	Add "O"-type data type (obs)  		*
 * D. Keiser/GSC	10/96	Added changes from S. Chiswell		*
 * D.W.Plummer/NCEP	11/96	Changed processing of "O" type		*
 * K. Tyle/GSC		 3/97	Add "F"-type data type (FFG)		*
 * D.W.Plummer/NCEP	 4/97	Add alphabetic sorting of bulletin table*
 * T. Piper/SAIC	01/04	Replaced NAWIPS_TABLES with GEMTBL	*
 * F. J. Yen/NCEP	 8/04	Fixed getting wrong ID for Reg. Tmp/Prec*  
 * T. Piper/SAIC	09/07	Modified for ctb_rstn CSC		*
 * T. Piper/SAIC	01/08	Changed bsflag from pointer to char arr	*
 ***********************************************************************/
{
int			ii, dummy, len, maxnum, ier;
char			table[SSTR_LEN], bsflag[2];
StnLst			*stations;
struct bulletin_list	bulletins[LLSTFL];

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 * Read the location table for either station data or
 * bulletin data.
 */
	maxnum  = LLSTFL;

	strcpy(bsflag, nwxTable->dtyp_info[inxdt].bsflag);

	switch ( bsflag[0] ) {

	    case 'R':
	    case 'M':
	    case 'O':
	    case 'F':

/*
 * Read station table
 */
	    	ctb_rstn( nwxTable->dtyp_info[inxdt].loctbl, TABLE_TYPE,
		       &(stns->nstn), &stations, iret );
	    	if  ( *iret != 0 )  return;

/*
 * Set the station list structure.
 */
	    	for ( ii = 0; ii < stns->nstn; ii++ ) {
		    if ( bsflag[0] == 'R' || bsflag[0] == 'O' ) {
		    	sprintf( stns->srchstr[ii], "%s", stations[ii].stid );
		        if ( strcmp ( nwxTable->dtyp_info[inxdt].loctbl,
				"rgtmpc.stn" ) == 0 ) {
			    cst_lstr ( stations[ii].stid, &len, &ier );	
			    if ( len == 5 ) {
/*
 *  Append a space to a State identifier since it could match the first
 *  5 characters of a 6 character WFO ID for the Regional Temp/Precip product.
 */
				stns->srchstr[ii][5] =  CHSPAC;
				stns->srchstr[ii][6] =  CHNULL;
			    }
			}
		    }
		    else if ( bsflag[0] == 'F' )
		    	sprintf( stns->srchstr[ii], "%-6s", stations[ii].stid );
		    else if ( bsflag[0] == 'M' )
			sprintf( stns->srchstr[ii], "%c%s ", 
				CHRS, stations[ii].stid );
		    strcpy( stns->stnName[ii], stations[ii].name );
		    strcpy( stns->stateId[ii], stations[ii].state );
		    strcpy( stns->counId[ii],  stations[ii].coun );
		    stns->lat[ii] = stations[ii].rlat;
		    stns->lon[ii] = stations[ii].rlon;
		    stns->elv[ii] = stations[ii].elev;
		}
		G_FREE(stations, StnLst);
		stns->bulstr[0] = '\0';
		
		break;


	    case 'B':
	    case 'S':
	    case 'W':

/*
 * Read bulletin table.
 */
	     	ctb_rbul( nwxTable->dtyp_info[inxdt].loctbl, TABLE_TYPE,
		       &maxnum, &(stns->nstn), bulletins, iret );
	    	if  ( *iret != 0 )  return;

/*
 * Sort bulletin table alphabetically.
 * Added (int(*)(const void*, const void*)) cast to satisfy qsort
 */
        	qsort(bulletins, (size_t)stns->nstn, sizeof(struct bulletin_list),
			(int(*)(const void*, const void*))_cmp_bulls );

/*
 * Set the station list structure.
 */
	    	for ( ii = 0; ii < stns->nstn; ii++ ) {
		    if ( bsflag[0] == 'W' || bsflag[0] == 'S' )
		        sprintf( stns->srchstr[ii], " %s ", 
				bulletins[ii].stid); 
		    else if ( bsflag[0] == 'B' )
		        sprintf( stns->srchstr[ii], "%s %s", 
				bulletins[ii].bullid, bulletins[ii].stid); 
		    strcpy( stns->stnName[ii], bulletins[ii].name );
		    strcpy( stns->stateId[ii], bulletins[ii].state );
		    strcpy( stns->counId[ii],  bulletins[ii].coun );
		    stns->lat[ii] = bulletins[ii].rlat;
		    stns->lon[ii] = bulletins[ii].rlon;
		    stns->elv[ii] = bulletins[ii].elev;
	    	}

	    	stns->bulstr[0] = '\0';

		break;


	    case 'Z':

/*
 * Read station table
 */
	     	strcpy( table, nwxTable->dtyp_info[inxdt].loctbl );
	     	strcat( table, ".stn" );
	     	ctb_rstn( table, TABLE_TYPE, &(stns->nstn), &stations, iret );
	     	if  ( *iret != 0 )  return;

/*
 * Set the station list structure. 
 */
		for ( ii = 0; ii < stns->nstn; ii++ ) {
		    strcpy( stns->srchstr[ii], stations[ii].stid );
		    strcpy( stns->stnName[ii], stations[ii].name );
		    strcpy( stns->stateId[ii], stations[ii].state );
		    strcpy( stns->counId[ii],  stations[ii].coun );
		    stns->lat[ii] = stations[ii].rlat;
		    stns->lon[ii] = stations[ii].rlon;
		    stns->elv[ii] = stations[ii].elev;
		}
		G_FREE(stations, StnLst);

/*
 * Read bulletin table to get bulletin search string.
 */
	        strcpy( table, nwxTable->dtyp_info[inxdt].loctbl );
	     	strcat( table, ".bull");
	     	ctb_rbul( table, TABLE_TYPE, &maxnum, &dummy, 
			bulletins, iret );
	     	if ( *iret != 0 ) return;

/*
 * Set the station list structure. 
 */
	        strcpy( stns->bulstr,  bulletins[0].bullid );

		break;


	    default: /* wrong type */

		*iret = -1;
		break;
	}
}

/*=====================================================================*/

int _cmp_bulls ( struct bulletin_list *bull1, struct bulletin_list *bull2 )
/************************************************************************
 * _cmp_bulls                                                           *
 *                                                                      *
 * This function compares station ID names in the bulletin_list struct. *
 *                                                                      *
 * int _cmp_bulls(bull1, bull2)                                         *
 *                                                                      *
 * Input parameters:                                                    *
 * *bull1	struct bulletin_list    struct bulletin_list element    *
 * *bull2	struct bulletin_list    struct bulletin_list element    *
 *                                                                      *
 * Return parameters:                                                   *
 * _cmp_bulls	int             Return code (ala strcmp)                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     04/97                                           *
 ***********************************************************************/
{
    return (strcmp(bull1->stid, bull2->stid));
}
