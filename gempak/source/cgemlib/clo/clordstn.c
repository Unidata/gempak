#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

static void clo_rdtcabkpts( Stn_t *stn, FILE *fp, char *alias, 
                            int numstn, int *iret );

void clo_rdstn ( Stn_t *stn, char *fnm, char *alias, int *iret )
/************************************************************************
 * clo_rdstn								*
 *									*
 * This function loads station table information into the standard	*
 * station information structure.					*
 *									*
 * clo_rdstn ( stn, fnm, alias, iret )					*
 *									*
 * Input parameters:							*
 * *stn		Stn_t		station structure to fill		*
 * *fnm		char		station file name to use		*
 * *alias	char		alias of table name			*
 *									*
 * Output parameters:							*
 * *iret	int		Return code				*
 *				  -1 - Unable to open table		*
 *				  -2 - No records in table		*
 *				  -3 - Invalid type of point		*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 1/99	From (now replaced) cloancrd.c		*
 * D.W.Plummer/NCEP	 3/99	Add sorting index			*
 * T. Piper/GSC		 3/99	Corrected prolog			*
 * D.W.Plummer/NCEP	 4/99	Updated for MARINE & COASTAL types	*
 * D.W.Plummer/NCEP	 7/00	Updated for consolidated CLO structures	*
 * D.W.Plummer/NCEP	 8/00	Added conversion to upper case of desc	*
 * T. Piper/GSC		 6/01	Initialized c10 			*
 * R. Tian/SAIC		 8/03	Increased c10 size.			*
 * B. Yin/SAIC		 5/04	Modified code to handle blank id.	*
 * B. Yin/SAIC		 7/04	Modified for various break point types	*
 * m.gamazaychikov	01/05	Corrected for proper column reading	*
 ***********************************************************************/
{
int	counter, ier;
char 	buff[120];
FILE	*fp;
char	id[9], desc[64], state[8], cntry[8], c10[21]="\0", tmpstr[128];
int	stnm, lat, lon, numstn, elv, pri;

/*---------------------------------------------------------------------*/
    *iret = G_NORMAL;

    /*
     *  Open the table. If not found return an error.
     */
    fp = (FILE *)cfl_tbop(fnm, "stns", &ier);
    if ( fp == NULL  ||  ier != 0 )  {
	stn->nstn = 0;
        *iret = -1;
        return;
    }

    cfl_tbnr( fp, &numstn, &ier );

    if ( numstn != 0 )  {

        /* 
         *  Allocate the structure elements.
         */
        stn->nstn = numstn; 
        stn->station = (SInfo_t *) malloc( (size_t)numstn * sizeof(SInfo_t) );

    }
    else  {
	cfl_clos(fp, &ier);
        *iret = -2;
        return;
    }

    rewind(fp);

    if ( strncmp( alias, "TCA_BKPTS_", 10 ) == 0 ) {
       clo_rdtcabkpts( stn, fp, alias, numstn, iret );
       cfl_clos(fp, &ier);
       return;
    }

    /* 
     *  For every line in the station table list, read in the record,
     *  parse out the fields, and add to the structure.
     */

    counter = 0;
    while ( counter < numstn ) {
    
	cfl_trln(fp, sizeof(buff), buff, &ier);

	if ( ier == 0 )  {

	    id[ 0 ]	= '\0';
	    desc[ 0 ]	= '\0';
	    state[ 0 ]	= '\0';
	    cntry[ 0 ]	= '\0';
	    c10[ 0 ]	= '\0';
	    stnm 	= 0;
	    lat 	= 9999;
	    lon 	= 9999;
	    elv 	= 9999;
	    pri 	= 9999;

	    strncpy( tmpstr, buff, 8 );
	    tmpstr[8]='\0';
	    sscanf( tmpstr, "%s", id );
	    stn->station[counter].id = (char *)malloc(sizeof(char)*strlen(id)+1);
	    strcpy( stn->station[counter].id, id );

	    strncpy( tmpstr, &buff[9], 6 );
	    tmpstr[6]='\0';
	    sscanf( tmpstr, "%d", &stnm );
	    stn->station[counter].nm = stnm;

	    strncpy( tmpstr, &buff[16], 32 );
	    tmpstr[32]='\0';
	    sscanf( tmpstr, "%s", desc );
	    stn->station[counter].desc = (char *)malloc(sizeof(char)*strlen(desc)+1);
	    cst_lcuc ( desc, stn->station[counter].desc, &ier );

	    strncpy( tmpstr, &buff[49], 2 );
	    tmpstr[2]='\0';
	    sscanf( tmpstr, "%s", state );
	    stn->station[counter].state=(char *)malloc(sizeof(char)*strlen(state)+1);
	    strcpy( stn->station[counter].state, state );

	    strncpy( tmpstr, &buff[52], 2 );
	    tmpstr[2]='\0';
	    sscanf( tmpstr, "%s", cntry );
	    stn->station[counter].cntry=(char *)malloc(sizeof(char)*strlen(cntry)+1);
	    strcpy( stn->station[counter].cntry, cntry );

	    strncpy( tmpstr, &buff[55], 21 );
	    tmpstr[21]='\0';
	    sscanf( tmpstr, "%d %d %d %d", &lat, &lon, &elv, &pri );
	    stn->station[counter].lat = (float)lat / 100.0F;

	    stn->station[counter].lon = (float)lon / 100.0F;

	    stn->station[counter].elv = elv;

	    stn->station[counter].pri = pri;

	    strncpy( tmpstr, &buff[76], 20 );
	    tmpstr[20]='\0';
	    sscanf( tmpstr, "%s", c10 );
	    stn->station[counter].col10 = (char *)malloc(sizeof(char)*strlen(c10)+1);
	    strcpy( stn->station[counter].col10, c10 );

	    counter++;

    	}

    }

    cfl_clos(fp, &ier);

}
/*======================================================================*/

static void clo_rdtcabkpts( Stn_t *stn, FILE *fp, char *alias, 
                            int numstn, int *iret )
/************************************************************************
 * clo_rdtcabkpts							*
 *									*
 * This function loads break point table information into the standard	*
 * station information structure.					*
 *									*
 *									*
 * void clo_rdtcabkpts( stn, fp, alias, numstn, iret )			*
 *									*
 * Input parameters:							*
 * *stn		Stn_t		station structure to fill		*
 * *fp		FILE		FILE pointer to the table file		*
 * *alias	char		alias name of the table			*
 * numstn	int		total number of stations in the table	*
 *									*
 * Output parameters:							*
 * *iret	int		Return code				*
 **									*
 * Log:									*
 * B. Yin/SAIC		 7/04	Created					*
 * m.gamazaychikov	01/05	Corrected for proper column reading	*
 * D. Kidwell/NCEP	 4/05	Added pri = 99 check for supplemental   *
 ***********************************************************************/
{
    char 	buff[120];
    char	id[9], desc[64], state[8], cntry[8], c10[21]="\0", tmpstr[128];
    int		counter, ier, lat, lon, stnm, elv, pri;
/*------------------------------------------------------------------------*/

    counter = 0;
    ier = 0;

    while ( counter < numstn && ier != 4 ) {  			/* ier = 4 EOF */
    
	cfl_trln(fp, sizeof(buff), buff, &ier);

	if ( ier == 0 )  {

	    id[ 0 ]	= '\0';
	    desc[ 0 ]	= '\0';
	    state[ 0 ]	= '\0';
	    cntry[ 0 ]	= '\0';
	    c10[ 0 ]	= '\0';
	    stnm 	= 0;
	    lat 	= 9999;
	    lon 	= 9999;
	    elv 	= 9999;
	    pri 	= 9999;

	    strncpy( tmpstr, &buff[55], 21 );
	    tmpstr[21]='\0';
	    sscanf( tmpstr, "%d %d %d %d", &lat, &lon, &elv, &pri );

            if ( (  ( strcmp( alias, "TCA_BKPTS_OFF" ) == 0 &&
                       pri % 10 == 0 && pri != 80 && pri != 50 ) 	/* official break points */
                   ||( strcmp( alias, "TCA_BKPTS_SUP" ) == 0 && 
                       (( pri == 19 ) || ( pri == 99 )
                       ||( pri % 10 == 0 && pri != 80 && pri != 50)))   /* supplemental */
                   ||( strcmp( alias, "TCA_BKPTS_WAT" ) == 0 &&
                       pri == 80 )					/* water */
                   ||( strcmp( alias, "TCA_BKPTS_ISL" ) == 0 &&
                       pri == 50 ) ) ){					/* islands */
                 
	       stn->station[counter].lat = (float)lat / 100.0F;
	       stn->station[counter].lon = (float)lon / 100.0F;

	       stn->station[counter].elv = elv;
	       stn->station[counter].pri = pri;

	       strncpy( tmpstr, buff, 8 );
	       tmpstr[8]='\0';
	       sscanf( tmpstr, "%s", id );
	       stn->station[counter].id = (char *)malloc(sizeof(char)*strlen(id)+1);
	       strcpy( stn->station[counter].id, id );

	       strncpy( tmpstr, &buff[9], 6 );
	       tmpstr[6]='\0';
	       sscanf( tmpstr, "%d", &stnm );
	       stn->station[counter].nm = stnm;

	       strncpy( tmpstr, &buff[16], 32 );
	       tmpstr[32]='\0';
	       sscanf( tmpstr, "%s", desc );
	       stn->station[counter].desc = (char *)malloc(sizeof(char)*strlen(desc)+1);
	       cst_lcuc ( desc, stn->station[counter].desc, &ier );

	       strncpy( tmpstr, &buff[49], 2 );
	       tmpstr[2]='\0';
	       sscanf( tmpstr, "%s", state );
	       stn->station[counter].state=(char *)malloc(sizeof(char)*strlen(state)+1);
	       strcpy( stn->station[counter].state, state );

	       strncpy( tmpstr, &buff[52], 2 );
	       tmpstr[2]='\0';
	       sscanf( tmpstr, "%s", cntry );
	       stn->station[counter].cntry=(char *)malloc(sizeof(char)*strlen(cntry)+1);
	       strcpy( stn->station[counter].cntry, cntry );

	       strncpy( tmpstr, &buff[76], 20 );
	       tmpstr[20]='\0';
	       sscanf( tmpstr, "%s", c10 );
	       stn->station[counter].col10 = (char *)malloc(sizeof(char)*strlen(c10)+1);
	       strcpy( stn->station[counter].col10, c10 );

	       counter++;
            }

    	}
    }

    stn->nstn = counter; 

    /*
     * Free extra memory
     */
    stn->station = (SInfo_t *) realloc( stn->station, (size_t)counter * sizeof(SInfo_t) );

    *iret = 0;
}
