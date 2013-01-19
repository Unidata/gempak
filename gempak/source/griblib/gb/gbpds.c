#include "gbcmn.h"

void gb_pds ( unsigned char *ptarray )
/************************************************************************
 * gb_pds								*
 *									*
 * This function decodes section 1 (Product Definition Section) of a	*
 * GRIB message.							*
 *									*
 * gb_pds ( ptarray )							*
 *									*
 * Input parameters:							*
 *	*ptarray	unsigned char	Data buffer			*
 *									*
 **									*
 * Log:									*
 * J. Chou/EAI		02/93						*
 * J. Chou/EAI		07/93						*
 * S. Jacobs/EAI        11/93   Clean up; Added GBDIAG prints   	*
 * S. Jacobs/EAI	 1/94	Clean up; Rename variables		*
 * L. Williams/EAI	 7/94	Reformat header				*
 * D.W.Plummer/NCEP      1/96   Reformat;rearrange GBDIAG prints	*
 * D.W.Plummer/NCEP      1/96   Add PDS extension processing    	*
 * D.W.Plummer/NCEP      2/98   Add TDL ensemble processing		*
 * T. Piper/GSC		11/98	Updated prolog				*
 * D.W.Plummer/NCEP	 3/99	Remove returns before breaks		*
 * S. Jacobs/NCEP	12/00	Added prototypes			*
 * M. Li/SAIC		05/07	Added gb_wcmwfens			*
 ***********************************************************************/
{
int	indx;

/*---------------------------------------------------------------------*/

	/*
  	 * BYTES 1-3
  	 * Get the length of the section.
	 */
	indx = 0; 
	pds.length = gb_btoi(ptarray, indx, 3, FALSE);

	/*
  	 * BYTE 4
  	 * Get the parameter code table version number.
	 */
	indx = 3; 
	pds.version = gb_btoi(ptarray, indx, 1, FALSE);

	/*
  	 * BYTE 5
  	 * Get the center ID.
	 */
	indx = 4; 
	pds.center = gb_btoi(ptarray, indx, 1, FALSE); 

	/*
  	 * BYTE 6
  	 * Get the generating process ID.
	 */
	indx = 5; 
	pds.process = gb_btoi(ptarray, indx, 1, FALSE);

	/*
  	 * BYTE 7
  	 * Get the grid definition.
	 */
	indx = 6; 
	pds.grid_id = gb_btoi(ptarray, indx, 1, FALSE);

	/*
  	 * BYTE 8
  	 * Get the GDS/BMS flag.
	 */
	indx = 7; 
	pds.flag = gb_btoi(ptarray, indx, 1, FALSE);

	pds.isbms = ((pds.flag & 64) >> 6);
	pds.isgds = ((pds.flag & 128) >> 7);

	/*
  	 * BYTE 9
  	 * Get the GRIB parameter number.
	 */
	indx = 8; 
	pds.parameter = gb_btoi(ptarray, indx, 1, FALSE); 

	/*
  	 * BYTE 10
  	 * Get the GRIB vertical coordinate.
	 */
	indx = 9; 
	pds.vcoord = gb_btoi(ptarray, indx, 1, FALSE);

	/*
  	 * BYTES 11-12
  	 * Get the level(s).
	 */
	indx = 10; 
	pds.level = gb_btoi(ptarray, indx, 2, TRUE);

	/*
  	 * BYTES 11
  	 * Get level_1.
	 */
	pds.level_1 = gb_btoi(ptarray, indx, 1, FALSE);

	/*
  	 * BYTES 12
  	 * Get level_2.
	 */
	indx = 11; 
	pds.level_2 = gb_btoi(ptarray, indx, 1, FALSE);

	/*
  	 * BYTE 13
  	 * Get the year of century.
	 */
	indx = 12; 
	pds.year = gb_btoi(ptarray, indx, 1, FALSE);

	/*
  	 * BYTE 14
  	 * Get the month.
	 */
	indx = 13; 
	pds.month = gb_btoi(ptarray, indx, 1, FALSE);

	/*
  	 * BYTE 15
  	 * Get the day.
	 */
	indx = 14; 
	pds.day = gb_btoi(ptarray, indx, 1, FALSE);

	/*
  	 * BYTE 16
  	 * Get the hour.
	 */
	indx = 15; 
	pds.hour = gb_btoi(ptarray, indx, 1, FALSE);

	/*
  	 * BYTE 17
  	 * Get the minute.
	 */
	indx = 16; 
	pds.minute = gb_btoi(ptarray, indx, 1, FALSE);

	/*
  	 * BYTE 18
  	 * Get the indicator of unit of time range of forecast time. 
	 */
	indx = 17; 
	pds.time_unit = gb_btoi(ptarray, indx, 1, FALSE);

	/*
  	 * BYTE 19
  	 * Get P1 - part of forecast time.
	 */
	indx = 18; 
	pds.time_p1 =  gb_btoi(ptarray, indx, 1, FALSE);

	/*
  	 * BYTE 20
  	 * Get P2 - part of forecast time.
	 */
	indx = 19; 
	pds.time_p2 = gb_btoi(ptarray, indx, 1, FALSE);

	/*
  	 * BYTE 21
  	 * Get the time range indicator which flags how to combine P1 & P2.
	 */
	indx = 20; 
	pds.time_range = gb_btoi(ptarray, indx, 1, FALSE);

	/*
  	 * BYTES 22-23
  	 * Get the number included in average. (For use with P1 & P2.)
	 */
	indx = 21; 
	pds.avg_num =  gb_btoi(ptarray, indx, 2, FALSE);

	/*
  	 * BYTE 24
  	 * Get the Number missing from averages or accumulations.
  	 * (For use with P1 & P2.)
	 */
	indx = 23; 
	pds.avg_miss = gb_btoi(ptarray, indx, 1, FALSE);

	/*
  	 * BYTE 25
  	 * Get the century of the reference time.
	 */
	indx = 24; 
	pds.century = gb_btoi(ptarray, indx, 1, FALSE);

	/*
  	 * BYTE 26
  	 * Reserved - set to zero
	 */
	indx = 25; 
	pds.izero = gb_btoi(ptarray, indx, 1, FALSE);

	/*
  	 * BYTES 27-28
  	 * Get the units decimal scale factor (D).
	 */
	indx = 26; 
	pds.dec_scale = gb_btoi(ptarray, indx, 2, TRUE);

	/*
  	 * PDS EXTENSION
	 */
	pds.pdse = 0;
	pds.extension[0] = '\0';

	if ( GBDIAG_PDS == TRUE)  {
	    printf(" PDS bytes  1- 3 (pds.length)      = %d\n", pds.length );
	    printf(" PDS byte      4 (pds.version)     = %d\n", pds.version );
	    printf(" PDS byte      5 (pds.center)      = %d\n", pds.center );
	    printf(" PDS byte      6 (pds.process)     = %d\n", pds.process );
	    printf(" PDS byte      7 (pds.grid_id)     = %d\n", pds.grid_id );
	    printf(" PDS byte      8 (pds.flag)        = %d\n", pds.flag );
	    printf(" PDS byte      9 (pds.parameter)   = %d\n", pds.parameter );
	    printf(" PDS byte     10 (pds.vcoord)      = %d\n", pds.vcoord );
	    printf(" PDS bytes    11 (pds.level_1)     = %d\n", pds.level_1 );
	    printf(" PDS bytes    12 (pds.level_2)     = %d\n", pds.level_2 );
	    printf(" PDS bytes 11-12 (pds.level)       = %d\n", pds.level );
	    printf(" PDS byte     13 (pds.year)        = %d\n", pds.year );
	    printf(" PDS byte     14 (pds.month)       = %d\n", pds.month );
	    printf(" PDS byte     15 (pds.day)         = %d\n", pds.day );
	    printf(" PDS byte     16 (pds.hour)        = %d\n", pds.hour );
	    printf(" PDS byte     17 (pds.minute)      = %d\n", pds.minute );
	    printf(" PDS byte     18 (pds.time_unit)   = %d\n", pds.time_unit );
	    printf(" PDS byte     19 (pds.time_p1)     = %d\n", pds.time_p1 );
	    printf(" PDS byte     20 (pds.time_p2)     = %d\n", pds.time_p2 );
	    printf(" PDS byte     21 (pds.time_range)  = %d\n", pds.time_range );
	    printf(" PDS bytes 22-23 (pds.avg_num)     = %d\n", pds.avg_num );
	    printf(" PDS byte     24 (pds.avg_miss)    = %d\n", pds.avg_miss );
	    printf(" PDS byte     25 (pds.century)     = %d\n", pds.century );
	    printf(" PDS byte     26 (pds.izero)       = %d\n", pds.izero );
	    printf(" PDS bytes 27-28 (pds.dec_scale)   = %d\n", pds.dec_scale );
	}

	if ( pds.length > 40 ) {

	    /*
	     *	Process the entire PDS including extension.
	     */

	     if ( pds.center == 98 )  {
		gb_ecmwfens ( ptarray );  /* for ECMWF  */
	     }
	     else {
		gb_pdsext ( ptarray );    /* for US National Weather Service */
	     }

	}

	if ( GBDIAG_PDS == TRUE )  {
	    printf(" PDS EXT FLAG (1-app,0-nc,-1-rep)  = %d\n", pds.pdse);
	    printf(" PDS EXT STRING                    = %s\n", pds.extension );
	}
}

void gb_pdsext ( unsigned char *buff )
/************************************************************************
 * gb_pdsext								*
 *									*
 * This function forks processing to the appropriate PDS extension	*
 * processing function.							*
 *									*
 * gb_pdsext ( buff )							*
 *									*
 * Input parameters:							*
 *	*buff		unsigned char	PDS buffer			*
 *									*
 * Output parameters:							*
 *	none								*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP      1/96           New				*
 * S. Jacobs/NCEP	12/07	Changed Case 1 to Default		*
 ***********************************************************************/
{
int	indx;

	indx = 40;
	switch ( gb_btoi(buff, indx, 1, FALSE) ) {

		case 2 :
			gb_tdlens ( buff );
		break;

		default:
			gb_ensemble ( buff );
		break;

	}

	return;

}

void gb_ecmwfens ( unsigned char *buff )
/************************************************************************
 * gb_ecmwfens                                                          *
 *                                                                      *
 * This function processes the appropriate PDS extension for ECMWF      *
 * ensemble data.                                                 	*
 *                                                                      *
 * gb_ecmwfens ( buff )                                                 *
 *                                                                      *
 * Input parameters:                                                    *
 *      *buff           unsigned char   PDS buffer                      *
 *                                                                      *
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		05/07	created					*
 * M. Li/SAIC		06/07	Added case 2 for Cluster products	*
 * S. Jacobs/NCEP	12/07	Added Default case			*
 ***********************************************************************/
{
int     indx;

        indx = 40;
        switch ( gb_btoi(buff, indx, 1, FALSE) ) {

                case 30 :    /* Control, Perturbed & Calibration/Validation
			        forecast products       */
                         gb_ecmwfcpc ( buff );
                break;

		case  2	:    /* Cluster Means & Standard Deviations products */ 
			 gb_ecmwfclu ( buff );
		break;

		default:
			gb_ensemble ( buff );
		break;

        }

        return;

}
