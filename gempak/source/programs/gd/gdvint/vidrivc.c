#include "geminc.h"
#include "gemprm.h"

/* 
 * This is dynamically allocated memory which is referenced by fortran code.
 * It is large enough for all of the MXGD sized arrays previously defined 
 * in Fortran common blocks and as local variables.
 */
static float *cmn_data_p=NULL;
static unsigned long cmn_data_size=0;

/*
 * These are indexes into the cmn_data_p buffer.
 */
static int    ovcsfc_array_indx=0;
static int    sfcval_array_indx=0;
static int    ovcbuf1_array_indx=0;
static int    ovcbuf2_array_indx=0;
static int    rlnpi_array_indx=0;
static int    rlnpo_array_indx=0;
static int    valu_array_indx=0;
static int    plast_array_indx=0;
static int    tlast_array_indx=0;
static int    zlast_array_indx=0;
    /* for previously local Fortran arrays */
static int    sphbuf_array_indx=0;     /* for VI_GETQ */
static int    tmpbuf_array_indx=0;     /* for VI_GETQ */
static int    hgtbuf_array_indx=0;     /* for VI_GETZ */
static int    grid_array_indx=0;       /* for VI_LNPO */
static int    pln_array_indx=0;        /* for VI_POOL */
static int    aprs_array_indx=0;       /* for VI_RDTK */
static int    rdwrbuf_array_indx=0;    /* for VI_WSFC */
static int    vgread_buf_array_indx=0;    /* for VI_WSFC */

void vi_drivf ( float *cmn_data, int *iret );

void vi_gindx ( int *ovcsfc_indx_p,  int *sfcval_indx_p, 
                int *ovcbuf1_indx_p, int *ovcbuf2_indx_p,
                int *rlnpi_indx_p,   int *rlnpo_indx_p,  int *valu_indx_p,
                int *plast_indx_p,   int *tlast_indx_p, int *zlast_indx_p,
                int *sphbuf_indx_p,  int *tmpbuf_indx_p, int *hgtbuf_indx_p,
                int *grid_indx_p,    int *pln_indx_p,    
                int *aprs_indx_p,    int *rdwr_indx_p,
                int *vgread_buf_indx_p );

void vi_drivc ( int *input_grid_size, 
                int *output_grid_size,
                int *nli, int *nlo, 
                int *nprms, int *iret );

void vi_drivc ( int *input_grid_size, 
                int *output_grid_size,
                int *nli, int *nlo, 
                int *nprms, int *iret )
/************************************************************************
 * vi_drivc                                                            *
 *                                                                      *
 * This function is a wrapper for the FORTRAN subroutine VI_DRIV. It's  *
 * purpose is to dynamically allocate memory which was previously       *
 * defined in FORTRAN common areas. 
 * The work array is freed from memory as soon as vi_drivc returns.     *
 *                                                                      *
 * Input parameters:                                                    *
 *     *input_grid_size  int            number of points in input grid  *
 *     *output_grid_size int            number of points in output grid *
 *     *nli              int            number of input levels          *
 *     *nlo              int            number of output levels         *
 *     *nprms            int            number of grid parmeters        *
 *                                                                      *
 * Output parameters:                                                   *
 *      iret            int             Return code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * G. Hull/SAIC		03/08    : Created.                             *
 *
 ***********************************************************************/
{
/*
 *    compute the size of the buffers needed by gdvint.
 *    This space is used for the following FORTRAN arrays:
 *
 *  ovcsfc
 *  sfcval
 *  ovcbuf1
 *  ovcbuf2
 *  rlnpi
 *  rlnpo
 *  valu
 *  plast
 *  tlast
 *  zlast
 *  sphbuf
 *  tmpbuf
 *  hgtbuf
 *  grid
 *  pln
 *  aprs
 */
    int grid_size;
    grid_size = *output_grid_size;

    cmn_data_size=0;  /* number of array entries, not bytes */
    *iret = 0;
    
    if( *input_grid_size  <= 0 ||
        *output_grid_size <= 0 )
    {
       printf("input or output grid size is invalid. (%d/%d)\n", 
              *input_grid_size, *output_grid_size );
       *iret = -15;
       return ;
    }

    ovcsfc_array_indx = cmn_data_size;
    cmn_data_size += grid_size;        /* ovcsfc(grid_size) */

    sfcval_array_indx = cmn_data_size; /* sfcval(grid_size,(*nprms)) */
    cmn_data_size += (grid_size * (*nprms));

    ovcbuf1_array_indx = cmn_data_size;
    cmn_data_size += grid_size;  /* ovcbuf1(grid_size) */

    ovcbuf2_array_indx = cmn_data_size;
    cmn_data_size += grid_size;  /* ovcbuf2(grid_size) */

    rlnpi_array_indx = cmn_data_size;
    cmn_data_size += (grid_size * (*nli));  /* rlnpi(grid_size, nli) */

    rlnpo_array_indx = cmn_data_size;
    cmn_data_size += (grid_size * (*nlo));  /* rlnpo(grid_size, nlo) */

    valu_array_indx = cmn_data_size;
    cmn_data_size += (grid_size * (*nli)); /* */

    plast_array_indx = cmn_data_size;
    cmn_data_size += grid_size;        /* plast(grid_size) */

    tlast_array_indx = cmn_data_size;
    cmn_data_size += grid_size;        /* tlast(grid_size) */

    zlast_array_indx = cmn_data_size;
    cmn_data_size += grid_size;        /* zlast(grid_size) */

    sphbuf_array_indx = cmn_data_size;
    cmn_data_size += (grid_size);     /* for local fortran in GETQ */

    tmpbuf_array_indx = cmn_data_size;
    cmn_data_size += (grid_size);     /* for local fortran in GETQ */

    hgtbuf_array_indx = cmn_data_size;
    cmn_data_size += (grid_size);     /* for local fortran in GETZ */

    grid_array_indx = cmn_data_size;
    cmn_data_size += (grid_size);     /* for local fortran in LNPO */

    pln_array_indx = cmn_data_size;
    cmn_data_size += (grid_size);     /* for local fortran in POOL */

    aprs_array_indx = cmn_data_size;
    cmn_data_size += (grid_size);     /* for local fortran in RDTK */

    vgread_buf_array_indx = cmn_data_size;
    cmn_data_size += (*input_grid_size);  /* for VG_READ before subsetting*/
/*
 *    Allocate work array
 */
     G_MALLOC( cmn_data_p, float, cmn_data_size,
                           "Common Data for Grid Arrays" );

     vi_drivf ( cmn_data_p, iret ); 

     G_FREE( cmn_data_p, float );
}

void vi_gindx ( int *ovcsfc_indx_p,  int *sfcval_indx_p, 
                int *ovcbuf1_indx_p, int *ovcbuf2_indx_p,
                int *rlnpi_indx_p,   int *rlnpo_indx_p,  int *valu_indx_p,
                int *plast_indx_p,   int *tlast_indx_p,  int *zlast_indx_p,
                int *sphbuf_indx_p,  int *tmpbuf_indx_p, int *hgtbuf_indx_p,
                int *grid_indx_p,    int *pln_indx_p,    
                int *aprs_indx_p,    int *rdwrbuf_indx_p, 
                int *vgread_buf_indx_p )
/************************************************************************
 * vi_gindx                                                             *
 *                                                                      *
 * This function returns the indexes into the cmn_data_p
 * NOTE: THESE ARE RETURNED AS INDEXES INTO FORTRAN ARRAYS and so each  *
 * is incremented by 1.
 *                                                                      *
 * Output parameters:                                                   *
 *     *ovcsfc_indx_p   int         indx into cmn_data_p for ovcsfc     *
 *     *sfcval_indx_p   int         indx into cmn_data_p for sfcval     *
 *     *ovcbuf1_indx_p  int         indx into cmn_data_p for ovcbuf1    *
 *     *ovcbuf2_indx_p  int         indx into cmn_data_p for ovcbuf2    *
 *     *rlnpi_indx_p    int         indx into cmn_data_p for rlnpi      *
 *     *rlnpo_indx_p    int         indx into cmn_data_p for rlnpo      *
 *     *valu_indx_p     int         indx into cmn_data_p for valu       *
 *     *plast_indx_p    int         indx into cmn_data_p for plast      *
 *     *tlast_indx_p    int         indx into cmn_data_p for tlast      *
 *     *zlast_indx_p    int         indx into cmn_data_p for zlast      *
 *     *sphbuf_indx_p   int         indx into cmn_data_p for  VI_GETQ   *
 *     *tmpbuf_indx_p   int         indx into cmn_data_p for  VI_GETQ   *
 *     *hgtbuf_indx_p   int         indx into cmn_data_p for  VI_GETZ   *
 *     *grid_indx_p     int         indx into cmn_data_p for  VI_LNPO   *
 *     *pln_indx_p      int         indx into cmn_data_p for  VI_POOL   *
 *     *aprs_indx_p     int         indx into cmn_data_p for  VI_RDTK   *
 *     *rdwrbuf_indx_p  int         indx into cmn_data_p for  VI_WSFC   *
 *     *vgread_buf_indx_p int       indx into cmn_data_p for  VG_READ   *
 *                                                                      *
 * Log:                                                                 *
 * G. Hull/SAIC		03/08    : Created.                             *
 *
 ***********************************************************************/
{
    *ovcsfc_indx_p  = ovcsfc_array_indx + 1;
    *sfcval_indx_p  = sfcval_array_indx + 1;
    *ovcbuf1_indx_p = ovcbuf1_array_indx + 1;
    *ovcbuf2_indx_p = ovcbuf2_array_indx + 1;
    *rlnpi_indx_p   = rlnpi_array_indx + 1;
    *rlnpo_indx_p   = rlnpo_array_indx + 1;
    *valu_indx_p    = valu_array_indx + 1;
    *plast_indx_p   = plast_array_indx + 1;
    *tlast_indx_p   = tlast_array_indx + 1;
    *zlast_indx_p   = zlast_array_indx + 1;
    *sphbuf_indx_p  = sphbuf_array_indx + 1;
    *tmpbuf_indx_p  = tmpbuf_array_indx + 1;
    *hgtbuf_indx_p  = hgtbuf_array_indx + 1;
    *grid_indx_p    = grid_array_indx + 1;
    *pln_indx_p     = pln_array_indx + 1;
    *aprs_indx_p    = aprs_array_indx + 1;
    *rdwrbuf_indx_p = rdwrbuf_array_indx + 1;
    *vgread_buf_indx_p = vgread_buf_array_indx + 1;
}

