#include "geminc.h"
#include "gemprm.h"
#include "interface.h"
#include "panel.h"
#include "Nxm.h"


static	int _psMode;

extern char	WaitFlag;
extern int	meta_id;
extern int	doflip;
extern short	*FrameBuffer;
extern  _NXMpixmapData  NXMpixmapData;


int	loadOnePage ( int  indx );
void	realloc_FrameBuffer ( void );


/***********************************************************************/

void ntrans_print ( void )
 /***********************************************************************
 * ntrans_print								*
 *                                                                      *
 * this function print ntrans products to a ps file			*
 *                                                                      *
 * void ntrans_print()							*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC	   1/97							*
 * S. Wang/GSC	   5/97		modified for busy/stop button		*
 * I. Durham/GSC   5/98		changed call for underscore		*
 ***********************************************************************/
{
int	mode_plus, break_flag;
int     pg_mode;
int	meta_id_save, FrameNo_save, SelectNo_save;
Meta_str	meta_st_save;
/*---------------------------------------------------------------------*/

	pg_mode = NxmPrt_isPgFlgSet();
	mode_plus = (pg_mode == 0 && ViewFrame) ? 0:1;
	break_flag = 0;

	if ( mode_plus == 1 ) {
	    meta_id_save  = meta_id;
	    meta_st_save  = meta_st;
	    FrameNo_save  = FrameNo;
	    SelectNo_save = SelectGroupNo;
	}

	loadBeginNotify();

        if (pg_mode == 0) {
            _psMode = 1;	
	    if ( ViewFrame ) {
		defaultView();
                Trans_Frame(ViewFrame-1);
	    }
	    else 
		break_flag = loadOnePage(NXMpixmapData.current);
	}
        else {
            if (GroupLoadFlag) {
                _psMode = 2;
	   	if ( loadAllPage() != 0 )
		    break_flag = -1;
            }
        }

	if ( mode_plus == 1 ) {
	    meta_id = meta_id_save;
	    meta_st = meta_st_save;
	    FrameNo = FrameNo_save;
	    SelectGroupNo = SelectNo_save;
	}

	if ( break_flag != -1 ) 
	    NxmBusy_animateFinish();

	loadEndNotify();

        _psMode = 0;
	return;

}

/***********************************************************************/

int nFrame ( void )
 /***********************************************************************
 * nFrame								*
 *                                                                      *
 * this function compute the maximum number of frames in multipanel	*
 * print.								*
 *                                                                      *
 * void nFrame()							*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC	   1/97							*
 ***********************************************************************/
{
int	ii, jj;
int	maxN, old_maxN;

/*---------------------------------------------------------------------*/

	old_maxN = 0;

	for ( ii=0; ii<Mpanel.rows; ii++ ) {
               for ( jj=0;jj<Mpanel.columns;jj++ ) {
                        if ( prt_multiPanel_info[ii][jj].flag == 1 ) {
			    maxN = prt_multiPanel_info[ii][jj].frame_num;
			    if (maxN > old_maxN)
				old_maxN = maxN;
			}    
               }
        }

	return (old_maxN);
}

/***********************************************************************/

int loadAllPage ( void )
 /***********************************************************************
 * loadAllPage								*
 *                                                                      *
 * this function load all pictures 					*
 *                                                                      *
 * int loadAllPage()							*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC	   2/97							*
 * S. Wang/GSC	   5/97		modified for busy/stop button		*
 ***********************************************************************/
{
int	i, ier;
int	nframe;

/*---------------------------------------------------------------------*/

	nframe = nFrame();

        for (i=0; i<nframe; i++) {
		if ( loadOnePage(i) == -1 ) {
                    return((i+1));
                }

	        if ( i < nframe-1 ) 
		    if ( _psMode == 2) {
		        gclear(&ier);
		    }

		if ( _psMode == 0 ) {
		    (PixmapData.pixmap_no)++;
                    gsplot(&ier);
		}
	}			/* end of group loop 	*/
	return(0);
}


/***********************************************************************/

int loadOnePage ( int indx )
 /***********************************************************************
 * loadOnePage								*
 *                                                                      *
 * this function loads one page of picture				*
 *                                                                      *
 * int loadOnePage()							*
 *                                                                      *
 * Input parameters:                                                    *
 * 	indx	int		page index                              *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC	   2/97							*
 * S. Wang/GSC	   5/97		modified for busy/stop button		*
 * S. Jacobs/NCEP  4/01		Added check for byte flipping		*
 ***********************************************************************/
{
int     frame, ii, jj;
short	char1;
/*---------------------------------------------------------------------*/

	for ( ii=0; ii<Mpanel.rows; ii++ ) {
	    for ( jj=0;jj<Mpanel.columns;jj++ ) {
		if ( prt_multiPanel_info[ii][jj].flag == 1 ) {

		    meta_id = prt_multiPanel_info[ii][jj].meta_id;
		    meta_st = prt_multiPanel_info[ii][jj].meta_st;
		    FrameNo = meta_st.num_frames;
		    SelectGroupNo = prt_multiPanel_info[ii][jj].group_no;

		    realloc_FrameBuffer();
		    if ( prt_multiPanel_info[ii][jj].frame_num > indx ) {
			frame = prt_multiPanel_info[ii][jj].frame[indx];
			if ( frame > -1 ) {
		            setView(ii, jj);

			    lseek( meta_id, 36, SEEK_SET);
			    read(meta_id, &char1, 2);
			    if(char1 > 255) {
			       doflip = 1;
			    }
			    else {
			       doflip = 0;
			    }

                            Trans_Frame(frame);
			}
		    }    
		}	 
	    }		 
	}		 

	NxmBusy_checkStopBtn();

	if ( WaitFlag ) {
	    NxmPrt_stopPrt();
	    return(-1);
	}
	else
	    return(0);
}

/***********************************************************************/

void realloc_FrameBuffer ( void )
 /***********************************************************************
 *                                                                      *
 * realloc_FrameBuffer()						*
 *                                                                      *
 * this function reallocate space for FrameBuffer			*
 *                                                                      *
 * void realloc_FrameBuffer()						*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC	   1/97							*
 ***********************************************************************/
{
int     i, buffer_size, start, end;
static	int	old_size;
int	allocate;

/*---------------------------------------------------------------------*/

        buffer_size = 0;
	allocate    = 0;

        for (i=0; i<FrameNo; i++) {
                start = (meta_st.frame[i]).off_byte;
                end   = (meta_st.frame[i]).end_byte;

                if ( (end - start) > buffer_size )
                        buffer_size = end - start;
        }

        if ( FrameBuffer == (short *)NULL )
                allocate = 1;
        else {
                if ( buffer_size/2 > old_size ) {
                        free(FrameBuffer);
                        FrameBuffer = (short *)NULL;
                        allocate = 1;
                }
        }

        if (allocate) {
                FrameBuffer = (short *)malloc((size_t)(buffer_size/2)*sizeof(short));
                old_size = buffer_size/2;
        }

	if ( FrameBuffer == NULL) {
		printf("error: Cannot get enough space for FrameBuffer\n");
        	exit(1);
        }
}
