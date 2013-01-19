#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

Layer_t		layer[MAX_LAYERS];

void ctb_lyrd ( char *tabnam, int *ntoply, int *iret )
/************************************************************************
 * ctb_lyrd								*
 *									*
 * This function loads layer table into its structure.			*
 *									*
 * ctb_lyrd ( tabnam, ntoply, iret )					*
 *									*
 * Input parameters:							*
 * *tabnam		char	Table name				*
 *									*
 * Output parameters:							*
 * *ntoply		int	Top layer				*
 * *iret		int	Return code				*
 *				   2 - Default value used		*
 *				  -1 - Table unable to open or not exist*
 *				  -2 - No valid records in data table	*
 *				  -3 - Failure to allocate buffer	*
 *				  -4 - Failure to read LPF file 	*
 **									*
 * Log:									*
 * T. Lee/SAIC		 2/02	Created					*
 * T. Lee/SAIC		 2/02	Added group type			*
 * T. Lee/SAIC		 4/02	Added error code; added CHNULL to buff	*
 * T. Lee/SAIC		 5/02	Added display mode			*
 * A. Hardy/NCEP	10/03   Fixed to skipped comment lines   	*
 * E. Safford/SAIC	11/03	Changed MAX_LAYER to MAX_LAYERS		*
 * E. Safford/SAIC	11/03	add tag "layerN_output_file"		*
 ***********************************************************************/
{
    int		ii, id, len, nbs, ier, quit;
    long	lflen;
    char	fname[FILE_FULLSZ];
    char 	def[9], tag[81], data[128], udata[128];
    char	*buff, lineBuf[1024];
    FILE	*fp;
    Boolean 	exist;
    char	*defdir = NULL;

/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;   
    *ntoply = -1;

    cfl_inqr ( tabnam, defdir, &lflen, fname, iret );

    if ( *iret == 0 ) {

	fp = cfl_ropn ( fname, NULL, &ier );

	if ( fp == (FILE *)NULL ) {
	    *iret = -2;
	    return;
	}
    }
    else {
	*iret = -1;
	return;
    }
    /*
     *  Read the layer table. If not found return an error.
     */

    buff = (char *) malloc ( (lflen+1) * sizeof (char) );

    if ( buff == (char *)NULL ) {
	*iret = -3;
	return;
    }
    
    buff[0]='\0';
    quit = G_FALSE;
    while ( !quit ) {
	cfl_trln ( fp, sizeof(lineBuf), lineBuf, &ier );
	if ( ier == 4 ) {
	    quit = G_TRUE;
	}
	else if ( ier < 0 ) {
	   *iret = -4;
	   quit = G_TRUE;
	}
	else {
	    cst_ldsp ( lineBuf, lineBuf, &nbs, &ier );
	    if ( lineBuf[0] != '!' ) {
		if ( lineBuf[0] == '<' ) {
		    strcat ( buff, "\n");
		}
		strcat (buff, lineBuf);
	    }
	}
    }
    
    buff[lflen]='\0';

    /*
     * Retrieve layer information and put into layer structure. 
     */

    for ( ii = 0; ii < MAX_LAYERS; ii++ ) {

	exist = FALSE;

	/* 
	 * Get layer name: layer_i (default).
	 */
	sprintf ( tag, "layer%d_name", ii+1 );
        cst_gtag ( tag, buff, " ", data, &ier );
	sprintf ( def, "layer_%d", ii+1 );
	if ( ier == 0 )  {
	    exist = TRUE;
	    cst_rmbl ( data, data, &len, &ier );
	    if ( len == 0 ) {
		strcpy ( data, def );
	    	*iret = 2;
	    }
	}
	else {
	    strcpy ( data, def );
	}
	strcpy ( layer [ii].name, data );
	    

	/*
	 * Get VG file name.
	 */
	sprintf ( tag, "layer%d_file", ii+1 );
	cst_gtag ( tag, buff, " ", data, &ier );
	if ( ier == 0 )  {
	    exist = TRUE;
	}
	cst_rmbl ( data, data, &len, &ier );
	strcpy ( layer [ii].vgfile, data );

        /*
	 *  Get output file name.
	 */
	sprintf ( tag, "layer%d_output_file", ii+1 );
	cst_gtag ( tag, buff, " ", data, &ier );
	if ( ier == 0 )  {
	    exist = TRUE;
	}
	cst_rmbl ( data, data, &len, &ier );
	strcpy ( layer [ii].outfile, data );


	/*
	 * Get color mode: ALL (default)/MONO.
	 */
	sprintf ( tag, "layer%d_color_mode", ii+1 );
	cst_gtag ( tag, buff, " ", data, &ier );
	if ( ier == 0 )  {
	    exist = TRUE;
	    cst_rmbl ( data, data, &len, &ier );
	    cst_lcuc ( data, udata, &ier );
	    if ( ( strcmp ( udata, "ALL" ) != 0 && 
	           strcmp ( udata, "MONO" ) != 0 ) || len == 0 ) {
		strcpy ( data, "ALL" );
	        *iret = 2;
	    }
	}
	else {
	    strcpy ( data, "ALL" );
	}
	strcpy ( layer [ii].cmode,  data );


	/*
	 * Get color id: 19 (default).
	 */
	sprintf ( tag, "layer%d_color_id", ii+1 );
	cst_gtag ( tag, buff, " ", data, &ier );
	if ( ier == 0 )  {
	    exist = TRUE;
	    cst_rmbl ( data, data, &len, &ier );
	    if ( len == 0 ) {
		strcpy ( data, "19" );
		*iret = 2;
	    }
	}
	else { 
	    strcpy ( data, "19" );	    
	}
	cst_numb ( data, &id, &ier );

	if ( id < 0 ) {
	    id = 19;
	}
	id = id%32; 
	if ( id == 0 ) { 
	    id = 32; 
	} 
	layer [ii].cid = id; 

	/* 
	 * Get fill mode: ON (default)/OFF.  
	 */ 
	sprintf ( tag, "layer%d_fill_mode", ii+1 ); 
	cst_gtag ( tag, buff, " ", data, &ier ); 
	if ( ier == 0 )  {
	    exist = TRUE;
	    cst_rmbl ( data, data, &len, &ier );
	    cst_lcuc ( data, udata, &ier );
	    if ( ( strcmp ( udata, "ON" ) != 0 &&
               	   strcmp ( udata, "OFF" ) != 0 ) || len == 0 ) {
		strcpy ( data, "ON" );
	    	*iret = 2;
	    }
	}
	else {
	    strcpy ( data, "ON" );
	}
	strcpy ( layer [ii].fmode, data );

	/*
	 * Get group type.
	 */
	sprintf ( tag, "layer%d_group_type", ii+1 );
	cst_gtag ( tag, buff, " ", data, &ier );
	if ( ier == 0 )  {
	    exist = TRUE;
	    cst_rmbl ( data, data, &len, &ier );
	    if ( len == 0 ) {
	        *iret = 2;
	    }
	}
	strcpy ( layer [ii].gtype, data );

	/*
	 * Get display mode: ON (default)/OFF.
	 */
	sprintf ( tag, "layer%d_display_mode", ii+1 );
	cst_gtag ( tag, buff, " ", data, &ier );
	if ( ier == 0 )  {
	    exist = TRUE;
	    cst_rmbl ( data, data, &len, &ier );
	    cst_lcuc ( data, udata, &ier );
	    if ( ( strcmp ( udata, "ON" ) != 0 &&
                   strcmp ( udata, "OFF" ) != 0 ) || len == 0 ) {
		strcpy ( data, "ON" );
		*iret = 2;
	    }
	}
	else {
	    strcpy ( data, "ON" );
	}
	strcpy ( layer [ii].dsply, data );

	if ( exist )  {
	    *ntoply = ii+1;
	}
    }

    if ( *ntoply < 0 ) {
	*iret = -2;
    }

    free ( buff );
    cfl_clos ( fp, &ier );
}
