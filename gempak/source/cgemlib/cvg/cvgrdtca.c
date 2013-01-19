#include "cvgcmn.h"

/*
 *  Private function to load a TcaInfo struct from a string.
 */
static void cvg_loadTcaInfo ( TcaInfo *info, char *  ptca, int *iret );

void cvg_rdtca ( FILE *fptr, long fil_pos, VG_DBStruct *el, int *iret )
/************************************************************************
 * cvg_rdtca								*
 *									*
 * This function reads a TCA element from the given file. TCA elements	*
 * are stored in the files using TcaFileInfo, but loaded into TcaInfo	*
 * for internal reference.						*
 *									*
 * cvg_rdtca ( fptr, fil_pos, el, iret )				*
 *									*
 * Input parameters:							*
 *	*fptr		FILE		File pointer			*
 *	fil_pos		long		Position to read from		*
 *									*
 * Output parameters:							*
 *	*el		VG_DBStruct	Element structure		*
 *	*iret		int		Return code			*
 *					 -3 = Seek to a bad location	*
 *					 -8 = No file has been opened	*
 *					 -14 = Fail to read elem	*
 **									*
 * Log:                                                                 *
 * B. Yin/SAIC		02/04	Created					*
 * B. Yin/SAIC		05/04	Modified code to read out record size 	*
 * B. Yin/SAIC		12/04	Changed parameter type for cfl_seek	*
 ***********************************************************************/
{
    int		ier, nbytes, nbin;
    char	*ptca;
/*---------------------------------------------------------------------*/

    *iret = 0;

    if ( fptr == NULL ) {
        *iret = -8;
        return;
    }


    /*
     *	Seek to the given position.
     */
    cfl_seek( fptr, (long)(fil_pos - sizeof(VG_HdrStruct)), 0, &ier );
    if ( ier != 0 ) {
        *iret = -3;
        return;    
    }

    /*
     * Read the header to get the record size
     */

    cfl_read(fptr, sizeof(VG_HdrStruct), (void *)el, &nbin, &ier);
    if ( MTMACH == MTULTX ||
         MTMACH == MTALPH ||
         MTMACH == MTLNUX ) {
        cvg_swap( SWPHDR, G_TRUE, *el, el, &ier );
    }

    /*
     *	Read the Tca string.
     */
    nbytes = el->hdr.recsz - sizeof(VG_HdrStruct);
    if ( !(ptca = (char *) malloc ( nbytes + 1 ))) return;
    ptca[ nbytes ] = '\0';
    cfl_read ( fptr, nbytes, (void *) ptca, &nbin, &ier); 
    
    if ( nbin != nbytes ) {
        *iret = -14;
        return;
    }

    /*
     *	Load the Tca string into el->TcaInfo.
     */
    cvg_loadTcaInfo ( &el->elem.tca.info, ptca, iret );
    free ( ptca );
}

/*=====================================================================*/

static void cvg_loadTcaInfo ( TcaInfo *info, char *tca_str, int *iret )
/************************************************************************
 * cvg_loadTcaInfo							*
 *									*
 * This function loads a TCA string into a TcaInfo structure.		*
 * 									*
 * cvg_loadTcaInfo ( info, tca_str, iret )				*
 *									*
 * Input parameters:							*
 *	*info		TcaInfo		Pointer to TcaInfo struct	*
 *	*tca_str	char		TCA string			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:								 	*
 * B. Yin/SAIC		02/04	Created					*
 * B. Yin/SAIC		03/04	Added storm type in tca structure	*
 * B. Yin/SAIC		05/04	Changed calling sequence		*
 * B. Yin/SAIC		07/04	Modified for more than 2 bkpts		*
 * B. Yin/SAIC		12/04	Added time zone				*
 * B. Yin/SAIC		04/05	Added issue status, removed year	*
 * S. Gilbert/NCEP	11/05	Added text_lat, text_lon                *
 * S. Gilbert/NCEP	01/06	Changed advisoryNum from int to char    *
 * M. Li/SAIC		05/06	Added check for bkpt_buf length		*
 ***********************************************************************/
{
    double	dbltmp;
    int		ii, ier, inttmp, counter, len;
    char	buf[ STD_STRLEN ], cmiss[20];
    char	tag_str[ STD_STRLEN ], *dft = "", *bkpt_buf;
    float       lat[9]={30., 30., 30., 25., 25., 25., 20., 20., 20. };
    float       lon[9]={-85., -80., -75., -85., -80., -75., -85., -80., -75. };
/*---------------------------------------------------------------------*/
    
    /*
     *  Retrieve the tagged field and loads into specified field.
     */    
    cst_gtag ( TCA_STORMNUM_TAG, tca_str, dft, buf, &ier );
    info->stormNum = atoi ( buf );       
        
    cst_gtag ( TCA_ISSUESTATUS_TAG, tca_str, dft, buf, &ier );
    info->issueStatus = buf[ 0 ];      

    cst_gtag ( TCA_BASIN_TAG, tca_str, dft, buf, &ier );
    inttmp = atoi ( buf );
    info->basin = (enum basin_t)inttmp;       
        
    cst_gtag ( TCA_ADVISORYNUM_TAG, tca_str, dft, buf, &ier );
    strcpy ( info->advisoryNum, buf );       
        
    cst_gtag ( TCA_STORMNAME_TAG, tca_str, dft, buf, &ier );
    strcpy( info->stormName, buf ); 
        
    cst_gtag ( TCA_STORMTYPE_TAG, tca_str, dft, buf, &ier );
    inttmp = atoi ( buf );
    info->stormType = (enum storm_type_t)inttmp;       
        
    cst_gtag ( TCA_VALIDTIME_TAG, tca_str, dft, buf, &ier );
    strcpy( info->validTime, buf ); 
        
    cst_gtag ( TCA_TIMEZONE_TAG, tca_str, dft, buf, &ier );
    strcpy( info->timezone, buf ); 

    sprintf(cmiss,"%f",RMISSD);
    cst_gtag ( TCA_TEXTLAT_TAG, tca_str, cmiss, buf, &ier );
    dbltmp = atof( buf );
    info->text_lat = (float)dbltmp;
    if ( ERMISS ( info->text_lat ) ) info->text_lat = lat [info->stormNum % 9];
        
    cst_gtag ( TCA_TEXTLON_TAG, tca_str, cmiss, buf, &ier );
    dbltmp = atof( buf );
    info->text_lon = (float)dbltmp;
    if ( ERMISS ( info->text_lon ) ) info->text_lon = lon [info->stormNum % 9];
        
    cst_gtag ( TCA_TEXTFONT_TAG, tca_str, "1", buf, &ier );
    info->text_font = atoi( buf );
        
    cst_gtag ( TCA_TEXTSIZE_TAG, tca_str, "1.0", buf, &ier );
    info->text_size = atof( buf );
        
    cst_gtag ( TCA_TEXTWIDTH_TAG, tca_str, "3", buf, &ier );
    info->text_width = atoi( buf );
        
    cst_gtag ( TCA_WWNUM_TAG, tca_str, dft, buf, &ier );
    info->wwNum = atoi ( buf );      

    if ( !( bkpt_buf = malloc ( strlen( tca_str ) ) ) ) return;

    for ( counter = 0; counter < info->wwNum; counter++ ) {
        sprintf ( tag_str, "%s_%d", TCA_TCAWWSTR_TAG, counter );
        cst_gtag ( tag_str, tca_str, dft, buf, &ier );

	inttmp = atoi( strtok( buf, "|" ) );
	info->tcaww[ counter ].severity = (enum tca_sev_t)inttmp;
	inttmp = atoi( strtok( (char*)0, "|" ) );
	info->tcaww[ counter ].advisoryType = (enum tca_adv_t)inttmp;
        inttmp = atoi( strtok( (char*)0, "|" ) );
	info->tcaww[ counter ].specialGeog = (enum tca_sp_geog_t)inttmp;

        sprintf ( tag_str, "%s_%d", TCA_NUMBKPTS_TAG, counter );
        cst_gtag ( tag_str, tca_str, dft, buf, &ier );
	info->tcaww[ counter ].numBreakPts = atoi( buf );

        sprintf ( tag_str, "%s_%d", TCA_BREAKPTS_TAG, counter );
        cst_gtag ( tag_str, tca_str, dft, bkpt_buf, &ier );
	
        if ( !(info->tcaww[ counter ].breakPnt = malloc ( 
						  info->tcaww[ counter ].numBreakPts *
						  sizeof( Breakpt_T ) ))) {
	   return;
        }

        cst_rxbl ( bkpt_buf, bkpt_buf, &len, &ier );
	if ( len > 0 ) { 
            info->tcaww[ counter ].breakPnt[ 0 ].lat = atof( strtok( bkpt_buf, "|" ) );
            info->tcaww[ counter ].breakPnt[ 0 ].lon = atof( strtok( (char*)0, "|" ) );
            strcpy( info->tcaww[ counter ].breakPnt[ 0 ].breakPtName, strtok( (char*)0, "|" ) );

	    for ( ii = 1; ii < info->tcaww[ counter ].numBreakPts; ii++ ) {
            	info->tcaww[ counter ].breakPnt[ ii ].lat = atof( strtok( (char*)0, "|" ) );
            	info->tcaww[ counter ].breakPnt[ ii ].lon = atof( strtok( (char*)0, "|" ) );
            	strcpy( info->tcaww[ counter ].breakPnt[ ii ].breakPtName, strtok( (char*)0, "|" ) );
            }
        }
    }

    free ( bkpt_buf );

    *iret = 0;
}
