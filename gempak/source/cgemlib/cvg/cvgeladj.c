#include "cvgcmn.h"

static void cvg_ashadj(Handle id, VG_DBStruct *el, int *iret);
static void cvg_ccfpadj(Handle id, VG_DBStruct *el, int *iret);
static void cvg_circleadj(Handle id, VG_DBStruct *el, int *iret);
void cvg_filladj(Handle id, VG_DBStruct *el, int *iret);  /*  NOT used?  */
static void cvg_frontadj(Handle id, VG_DBStruct *el, int *iret);
static void cvg_gfaadj(Handle id, VG_DBStruct *el, int *iret);
static void cvg_sgwxadj(Handle id, VG_DBStruct *el, int *iret);
void cvg_groupadj(Handle id, VG_DBStruct *el, int *iret);  /*  NOT used?  */
static void cvg_jetadj(Handle id, VG_DBStruct *el, int *iret);
static void cvg_lineadj(Handle id, VG_DBStruct *el, int *iret);
static void cvg_listadj(Handle id, VG_DBStruct *el, int *iret);
void cvg_regeladj(Handle id, VG_DBStruct *el, int *iret);  /*  NOT used?  */
static void cvg_sigmetadj(Handle id, VG_DBStruct *el, int *iret);
static void cvg_splnadj(Handle id, VG_DBStruct *el, int *iret);
static void cvg_symbadj(Handle id, VG_DBStruct *el, int *iret);
static void cvg_tcaadj(Handle id, VG_DBStruct *el, int *iret);
static void cvg_textadj(Handle id, VG_DBStruct *el, int *iret);
static void cvg_trkstmadj(Handle id, VG_DBStruct *el, int *iret);
static void cvg_vectadj(Handle id, VG_DBStruct *el, int *iret);
static void cvg_volcadj(Handle id, VG_DBStruct *el, int *iret);
static void cvg_wboxadj(Handle id, VG_DBStruct *el, int *iret);

void cvg_eladj(int elpos, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_eladj
 * 
 * Verifies if the given VG object at the given file offset contains 
 * information in the CAP data structures.  If so, the location information 
 * for the VG element is adjusted based on the CAP information and returned.
 *
 * Input parameters:
 *  elpos       int             File offset for the VG element
 *  *el         VG_DBStruct     VG element to check for CAP information
 *
 * Output parameters:
 *  *el         VG_DBStruct     Adjusted VG element 
 *  *iret       int             Return code
 *                                 0 = Function successful
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC            3/06   Created
 * L. Hinson/AWC         1/12   Add SGWX_ELM and cvg_sgwxadj
 ****************************************************************************/
{
    Handle      id;
/*---------------------------------------------------------------------*/

    *iret = 0;
    if (!el) {
        *iret = -1;
        return;
    }

    if (cvg_placements && !cvg_meta_loading) {
        id = cvg_el2hndl(elpos, el, iret);

        switch ( el->hdr.vg_type) {
            case FRONT_ELM:
                cvg_frontadj(id, el, iret);
            break;

            case SPLN_ELM:
                cvg_splnadj(id, el, iret);
            break;

            case LINE_ELM:
                cvg_lineadj(id, el, iret);
            break;

            case TEXT_ELM:
            case TEXTC_ELM:
            case SPTX_ELM:
                cvg_textadj(id, el, iret);
            break;

            case BARB_ELM:
            case ARROW_ELM:
            case DARR_ELM:
            case HASH_ELM:
                cvg_vectadj(id, el, iret);
            break;

            case MARK_ELM:
            case WXSYM_ELM:
            case CTSYM_ELM:
            case ICSYM_ELM:
            case PTSYM_ELM:
            case PWSYM_ELM:
            case SKSYM_ELM:
            case SPSYM_ELM:
            case TBSYM_ELM:
            case CMBSY_ELM:
                cvg_symbadj(id, el, iret);
            break;

            case WBOX_ELM:
                cvg_wboxadj(id, el, iret);
            break;

            case CIRCLE_ELM:
                cvg_circleadj(id, el, iret);
            break;

            case TRKSTORM_ELM:
                cvg_trkstmadj(id, el, iret);
            break;

            case SIGAIRM_ELM:
            case SIGCONV_ELM:
            case SIGINTL_ELM:
            case SIGNCON_ELM:
            case SIGOUTL_ELM:
                cvg_sigmetadj(id, el, iret);
            break;

            case SIGCCF_ELM:
                cvg_ccfpadj(id, el, iret);
            break;

            case VOLC_ELM:
                cvg_volcadj(id, el, iret);
            break;

            case ASHCLD_ELM:
                cvg_ashadj(id, el, iret);
            break;

            case LIST_ELM:
                cvg_listadj(id, el, iret);
            break;

            case JET_ELM:
                cvg_jetadj(id, el, iret);
            break;

            case GFA_ELM:
                cvg_gfaadj(id, el, iret);
            break;
            
	    case SGWX_ELM:
                cvg_sgwxadj(id, el, iret);
            break;
	    
            case TCA_ELM:
                cvg_tcaadj(id, el, iret);
            break;

            default:
                /* Error? */
            break;
        }
    }
}


static void cvg_ashadj(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_ashadj
 * 
 * Adjusts the given VG element by the information given in the CAP data
 * provided.
 *
 * Input parameters:
 *  id          Handle          Handle for the 'base' part of the object
 *  *el         VG_DBStruct     VG element to check for CAP information
 *
 * Output parameters:
 *  *el         VG_DBStruct     Adjusted VG element 
 *  *iret       int             Return code
 *                                 0 = Function successful
 *                                -1 = Invalid pointer in arguments
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC            3/06   Created
 ****************************************************************************/
{
    Placement   place;
    int         placed;
/*---------------------------------------------------------------------*/

    cap_psgetpl(cvg_placements, id, &place, iret);
    if (place) {
        cap_plgetplaced(place, &placed, iret);
        if (placed) {
        }
    }
}


static void cvg_ccfpadj(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_ccfpadj
 * 
 * Adjusts the given VG element by the information given in the CAP data
 * provided.
 *
 * Input parameters:
 *  id          Handle          Handle for the 'base' part of the object
 *  *el         VG_DBStruct     VG element to check for CAP information
 *
 * Output parameters:
 *  *el         VG_DBStruct     Adjusted VG element 
 *  *iret       int             Return code
 *                                 0 = Function successful
 *                                -1 = Invalid pointer in arguments
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC            3/06   Created
 * L. Hinson/AWC         07/09  Fixed to make placement functional
 ****************************************************************************/
{
    Placement   place;
    int         placed;
    float     off_x, off_y, dx[MAXPTS], dy[MAXPTS], x_arrow[2], y_arrow[2];
    int       np;  

/*---------------------------------------------------------------------*/
    id += 1;
    cap_psgetpl(cvg_placements, id, &place, iret);
    if (place) {
        cap_plgetplaced(place, &placed, iret);
        if (placed) {
           cap_plgetoffset(place, &off_x, &off_y, iret);
           
           np = 1;
           
           dx[0] = el->elem.ccf.info.textlat;
           dy[0] = el->elem.ccf.info.textlon;
           
           gtrans(sys_M, sys_D, &np, dx, dy,
               &(dx[1]), &(dy[1]), iret, strlen(sys_M), strlen(sys_D));
           dx[1] += off_x;
           dy[1] += off_y;
           gtrans(sys_D, sys_M, &np, &(dx[1]), &(dy[1]), 
                   &(dx[0]), &(dy[0]), iret, strlen(sys_D), strlen(sys_M));
           
           el->elem.ccf.info.textlat = dx[0];
           el->elem.ccf.info.textlon = dy[0];
           
           cap_plgetline(place, x_arrow, y_arrow, iret);
           np = 1;
           gtrans(sys_D, sys_M, &np, &(x_arrow[1]), &(y_arrow[1]), 
                   &(dx[0]), &(dy[0]), iret, strlen(sys_D), strlen(sys_M));
           
           el->elem.ccf.info.arrowlat = dx[0];
           el->elem.ccf.info.arrowlon = dy[0];
                   
        }
    }
}


static void cvg_circleadj(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_circleadj
 * 
 * Adjusts the given VG element by the information given in the CAP data
 * provided.
 *
 * Input parameters:
 *  id          Handle          Handle for the 'base' part of the object
 *  *el         VG_DBStruct     VG element to check for CAP information
 *
 * Output parameters:
 *  *el         VG_DBStruct     Adjusted VG element 
 *  *iret       int             Return code
 *                                 0 = Function successful
 *                                -1 = Invalid pointer in arguments
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC            3/06   Created
 ****************************************************************************/
{
    Placement   place;
    int         placed;
/*---------------------------------------------------------------------*/

    cap_psgetpl(cvg_placements, id, &place, iret);
    if (place) {
        cap_plgetplaced(place, &placed, iret);
        if (placed) {
        }
    }
}

/*======================================================================*/

void cvg_filladj(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_filladj
 * 
 * Adjusts the given VG element by the information given in the CAP data
 * provided.
 *
 * Input parameters:
 *  id          Handle          Handle for the 'base' part of the object
 *  *el         VG_DBStruct     VG element to check for CAP information
 *
 * Output parameters:
 *  *el         VG_DBStruct     Adjusted VG element 
 *  *iret       int             Return code
 *                                 0 = Function successful
 *                                -1 = Invalid pointer in arguments
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC            3/06   Created
 ****************************************************************************/
{
    Placement   place;
    int         placed;
/*---------------------------------------------------------------------*/

    cap_psgetpl(cvg_placements, id, &place, iret);
    if (place) {
        cap_plgetplaced(place, &placed, iret);
        if (placed) {
        }
    }
}


static void cvg_frontadj(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_frontadj
 * 
 * Adjusts the given VG element by the information given in the CAP data
 * provided.
 *
 * Input parameters:
 *  id          Handle          Handle for the 'base' part of the object
 *  *el         VG_DBStruct     VG element to check for CAP information
 *
 * Output parameters:
 *  *el         VG_DBStruct     Adjusted VG element 
 *  *iret       int             Return code
 *                                 0 = Function successful
 *                                -1 = Invalid pointer in arguments
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC            3/06   Created
 ****************************************************************************/
{
    Placement   place;
    int         placed;
/*---------------------------------------------------------------------*/

    cap_psgetpl(cvg_placements, id, &place, iret);
    if (place) {
        cap_plgetplaced(place, &placed, iret);
        if (placed) {
        }
    }
}


static void cvg_gfaadj(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_gfaadj
 * 
 * Adjusts the given VG element by the information given in the CAP data
 * provided.
 *
 * Input parameters:
 *  id          Handle          Handle for the 'base' part of the object
 *  *el         VG_DBStruct     VG element to check for CAP information
 *
 * Output parameters:
 *  *el         VG_DBStruct     Adjusted VG element 
 *  *iret       int             Return code
 *                                 0 = Function successful
 *                                -1 = Invalid pointer in arguments
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC            3/06   Created
 ****************************************************************************/
{
    Placement   place;
    float       off_x, off_y, dx[MAXPTS], dy[MAXPTS], x_arrow[2], y_arrow[2];
    char	value[32];
    int         np, placed;
/*---------------------------------------------------------------------*/

    /*
     * The part of the GFA that is placed is the 'on the fly' text box,
     * who's id is 1 more than the 'base' object
     */
    id += 1;

    cap_psgetpl(cvg_placements, id, &place, iret);
    if (place) {
        cap_plgetplaced(place, &placed, iret);
        if (placed) {
            cap_plgetoffset(place, &off_x, &off_y, iret);
            np = 1;

            cvg_getFld ( el, TAG_GFA_LAT, value, iret );
            dx[0] = atof ( value );

            cvg_getFld ( el, TAG_GFA_LON, value, iret );
            dy[0] = atof ( value );

            gtrans(sys_M, sys_D, &np, dx, dy, 
                   &(dx[1]), &(dy[1]), iret, strlen(sys_M), strlen(sys_D));

            dx[1] += off_x;
            dy[1] += off_y;

            gtrans(sys_D, sys_M, &np, &(dx[1]), &(dy[1]), 
                   &(dx[0]), &(dy[0]), iret, strlen(sys_D), strlen(sys_M));

            sprintf(value, "%6.2f", dx[0]);
            cvg_setFld(el, TAG_GFA_LAT, value, iret);

            sprintf(value, "%7.2f", dy[0]);
            cvg_setFld(el, TAG_GFA_LON, value, iret);

            cap_plgetline(place, x_arrow, y_arrow, iret);
            np = 1;

            gtrans(sys_D, sys_M, &np, &(x_arrow[1]), &(y_arrow[1]), 
                   &(dx[0]), &(dy[0]), iret, strlen(sys_D), strlen(sys_M));

            sprintf(value, "%6.2f", dx[0]);
            cvg_setFld(el, TAG_GFA_ARROW_LAT, value, iret);

            sprintf(value, "%7.2f", dy[0]);
            cvg_setFld(el, TAG_GFA_ARROW_LON, value, iret);
        }
    }
}

/*======================================================================*/

static void cvg_sgwxadj(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_sgwxadj
 * 
 * Adjusts the given VG element by the information given in the CAP data
 * provided.
 *
 * Input parameters:
 *  id          Handle          Handle for the 'base' part of the object
 *  *el         VG_DBStruct     VG element to check for CAP information
 *
 * Output parameters:
 *  *el         VG_DBStruct     Adjusted VG element 
 *  *iret       int             Return code
 *                                 0 = Function successful
 *                                -1 = Invalid pointer in arguments
 *
 * Return value:
 *  None
 **
 * Log:
 * L. Hinson/AWC            1/12   Created
 ****************************************************************************/
{
    Placement   place;
    int         placed;
    float     off_x, off_y, dx[MAXPTS], dy[MAXPTS], x_arrow[2], y_arrow[2];
    int       np;  

/*---------------------------------------------------------------------*/
    id += 1;
    cap_psgetpl(cvg_placements, id, &place, iret);
    if (place) {
        cap_plgetplaced(place, &placed, iret);
        if (placed) {
           cap_plgetoffset(place, &off_x, &off_y, iret);
           
           np = 1;
           
           dx[0] = el->elem.sgwx.info.textlat;
           dy[0] = el->elem.sgwx.info.textlon;
           
           gtrans(sys_M, sys_D, &np, dx, dy,
               &(dx[1]), &(dy[1]), iret, strlen(sys_M), strlen(sys_D));
           dx[1] += off_x;
           dy[1] += off_y;
           gtrans(sys_D, sys_M, &np, &(dx[1]), &(dy[1]), 
                   &(dx[0]), &(dy[0]), iret, strlen(sys_D), strlen(sys_M));
           
           el->elem.sgwx.info.textlat = dx[0];
           el->elem.sgwx.info.textlon = dy[0];
           
           cap_plgetline(place, x_arrow, y_arrow, iret);
           np = 1;
           gtrans(sys_D, sys_M, &np, &(x_arrow[1]), &(y_arrow[1]), 
                   &(dx[0]), &(dy[0]), iret, strlen(sys_D), strlen(sys_M));
           
           el->elem.sgwx.info.arrowlat = dx[0];
           el->elem.sgwx.info.arrowlon = dy[0];
                   
        }
    }
}

/*======================================================================*/

void cvg_groupadj(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_groupadj
 * 
 * Adjusts the given VG element by the information given in the CAP data
 * provided.
 *
 * Input parameters:
 *  id          Handle          Handle for the 'base' part of the object
 *  *el         VG_DBStruct     VG element to check for CAP information
 *
 * Output parameters:
 *  *el         VG_DBStruct     Adjusted VG element 
 *  *iret       int             Return code
 *                                 0 = Function successful
 *                                -1 = Invalid pointer in arguments
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC            3/06   Created
 ****************************************************************************/
{
    Placement   place;
    int         placed;
/*---------------------------------------------------------------------*/

    cap_psgetpl(cvg_placements, id, &place, iret);
    if (place) {
        cap_plgetplaced(place, &placed, iret);
        if (placed) {
        }
    }
}


static void cvg_jetadj(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_jetadj
 * 
 * Adjusts the given VG element by the information given in the CAP data
 * provided.
 *
 * Input parameters:
 *  id          Handle          Handle for the 'base' part of the object
 *  *el         VG_DBStruct     VG element to check for CAP information
 *
 * Output parameters:
 *  *el         VG_DBStruct     Adjusted VG element 
 *  *iret       int             Return code
 *                                 0 = Function successful
 *                                -1 = Invalid pointer in arguments
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC            3/06   Created
 ****************************************************************************/
{
    Placement   place;
    int         placed;
/*---------------------------------------------------------------------*/

    cap_psgetpl(cvg_placements, id, &place, iret);
    if (place) {
        cap_plgetplaced(place, &placed, iret);
        if (placed) {
        }
    }
}


static void cvg_lineadj(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_lineadj
 * 
 * Adjusts the given VG element by the information given in the CAP data
 * provided.
 *
 * Input parameters:
 *  id          Handle          Handle for the 'base' part of the object
 *  *el         VG_DBStruct     VG element to check for CAP information
 *
 * Output parameters:
 *  *el         VG_DBStruct     Adjusted VG element 
 *  *iret       int             Return code
 *                                 0 = Function successful
 *                                -1 = Invalid pointer in arguments
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC            3/06   Created
 ****************************************************************************/
{
    Placement   place;
    int         placed;
/*---------------------------------------------------------------------*/

    cap_psgetpl(cvg_placements, id, &place, iret);
    if (place) {
        cap_plgetplaced(place, &placed, iret);
        if (place) {
        }
    }
}


static void cvg_listadj(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_listadj
 * 
 * Adjusts the given VG element by the information given in the CAP data
 * provided.
 *
 * Input parameters:
 *  id          Handle          Handle for the 'base' part of the object
 *  *el         VG_DBStruct     VG element to check for CAP information
 *
 * Output parameters:
 *  *el         VG_DBStruct     Adjusted VG element 
 *  *iret       int             Return code
 *                                 0 = Function successful
 *                                -1 = Invalid pointer in arguments
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC            3/06   Created
 ****************************************************************************/
{
    Placement   place;
    int         placed;
/*---------------------------------------------------------------------*/

    cap_psgetpl(cvg_placements, id, &place, iret);
    if (place) {
        cap_plgetplaced(place, &placed, iret);
        if (placed) {
        }
    }
}

/*======================================================================*/

void cvg_regeladj(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_regeladj
 * 
 * Adjusts the given VG element by the information given in the CAP data
 * provided.
 *
 * Input parameters:
 *  id          Handle          Handle for the 'base' part of the object
 *  *el         VG_DBStruct     VG element to check for CAP information
 *
 * Output parameters:
 *  *el         VG_DBStruct     Adjusted VG element 
 *  *iret       int             Return code
 *                                 0 = Function successful
 *                                -1 = Invalid pointer in arguments
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC            3/06   Created
 ****************************************************************************/
{
    Placement   place;
    int         placed;
/*---------------------------------------------------------------------*/

    cap_psgetpl(cvg_placements, id, &place, iret);
    if (place) {
        cap_plgetplaced(place, &placed, iret);
        if (placed) {
        }
    }
}


static void cvg_sigmetadj(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_sigmetadj
 * 
 * Adjusts the given VG element by the information given in the CAP data
 * provided.
 *
 * Input parameters:
 *  id          Handle          Handle for the 'base' part of the object
 *  *el         VG_DBStruct     VG element to check for CAP information
 *
 * Output parameters:
 *  *el         VG_DBStruct     Adjusted VG element 
 *  *iret       int             Return code
 *                                 0 = Function successful
 *                                -1 = Invalid pointer in arguments
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC            3/06   Created
 ****************************************************************************/
{
    Placement   place;
    int         placed;
/*---------------------------------------------------------------------*/

    cap_psgetpl(cvg_placements, id, &place, iret);
    if (place) {
        cap_plgetplaced(place, &placed, iret);
        if (placed) {
        }
    }
}


static void cvg_splnadj(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_splnadj
 * 
 * Adjusts the given VG element by the information given in the CAP data
 * provided.
 *
 * Input parameters:
 *  id          Handle          Handle for the 'base' part of the object
 *  *el         VG_DBStruct     VG element to check for CAP information
 *
 * Output parameters:
 *  *el         VG_DBStruct     Adjusted VG element 
 *  *iret       int             Return code
 *                                 0 = Function successful
 *                                -1 = Invalid pointer in arguments
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC            3/06   Created
 ****************************************************************************/
{
    Placement   place;
    int         placed;
/*---------------------------------------------------------------------*/

    cap_psgetpl(cvg_placements, id, &place, iret);
    if (place) {
        cap_plgetplaced(place, &placed, iret);
        if (placed) {
        }
    }
}


static void cvg_symbadj(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_symbadj
 * 
 * Adjusts the given VG element by the information given in the CAP data
 * provided.
 *
 * Input parameters:
 *  id          Handle          Handle for the 'base' part of the object
 *  *el         VG_DBStruct     VG element to check for CAP information
 *
 * Output parameters:
 *  *el         VG_DBStruct     Adjusted VG element 
 *  *iret       int             Return code
 *                                 0 = Function successful
 *                                -1 = Invalid pointer in arguments
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC            3/06   Created
 ****************************************************************************/
{
    Placement   place;
    int         placed;
/*---------------------------------------------------------------------*/

    cap_psgetpl(cvg_placements, id, &place, iret);
    if (place) {
        cap_plgetplaced(place, &placed, iret);
        if (placed) {
        }
    }
}


static void cvg_tcaadj(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_tcaadj
 * 
 * Adjusts the given VG element by the information given in the CAP data
 * provided.
 *
 * Input parameters:
 *  id          Handle          Handle for the 'base' part of the object
 *  *el         VG_DBStruct     VG element to check for CAP information
 *
 * Output parameters:
 *  *el         VG_DBStruct     Adjusted VG element 
 *  *iret       int             Return code
 *                                 0 = Function successful
 *                                -1 = Invalid pointer in arguments
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC            3/06   Created
 ****************************************************************************/
{
    Placement   place;
    int         placed;
/*---------------------------------------------------------------------*/

    cap_psgetpl(cvg_placements, id, &place, iret);
    if (place) {
        cap_plgetplaced(place, &placed, iret);
        if (placed) {
        }
    }
}


static void cvg_textadj(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_textadj
 * 
 * Adjusts the given VG element by the information given in the CAP data
 * provided.
 *
 * Input parameters:
 *  id          Handle          Handle for the 'base' part of the object
 *  *el         VG_DBStruct     VG element to check for CAP information
 *
 * Output parameters:
 *  *el         VG_DBStruct     Adjusted VG element 
 *  *iret       int             Return code
 *                                 0 = Function successful
 *                                -1 = Invalid pointer in arguments
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC            3/06   Created
 ****************************************************************************/
{
    Placement   place;
    int         placed, np;
    float       dx[MAXPTS], dy[MAXPTS], off_x, off_y;
/*---------------------------------------------------------------------*/

    cap_psgetpl(cvg_placements, id, &place, iret);
    if (place) {
        cap_plgetplaced(place, &placed, iret);
        if (placed) {
            cap_plgetoffset(place, &off_x, &off_y, iret);
            if (el->hdr.vg_type == SPTX_ELM) {
                    cvg_todev(el, &np, dx, dy, iret);
                    dx[0] += off_x;
                    dy[0] += off_y;
                    gtrans(sys_D, sys_M, &np, dx, dy, 
                           &el->elem.spt.info.lat, &el->elem.spt.info.lon,
                           iret, strlen(sys_D), strlen(sys_M));
            } else {
                    cvg_todev(el, &np, dx, dy, iret);
                    dx[0] += off_x;
                    dy[0] += off_y;
                    gtrans(sys_D, sys_M, &np, dx, dy, 
                           &el->elem.txt.info.lat, &el->elem.txt.info.lon,
                           iret, strlen(sys_D), strlen(sys_M));
            }
        }
    }
}


static void cvg_trkstmadj(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_trkstmadj
 * 
 * Adjusts the given VG element by the information given in the CAP data
 * provided.
 *
 * Input parameters:
 *  id          Handle          Handle for the 'base' part of the object
 *  *el         VG_DBStruct     VG element to check for CAP information
 *
 * Output parameters:
 *  *el         VG_DBStruct     Adjusted VG element 
 *  *iret       int             Return code
 *                                 0 = Function successful
 *                                -1 = Invalid pointer in arguments
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC            3/06   Created
 ****************************************************************************/
{
    Placement   place;
    int         placed;
/*---------------------------------------------------------------------*/

    cap_psgetpl(cvg_placements, id, &place, iret);
    if (place) {
        cap_plgetplaced(place, &placed, iret);
        if (placed) {
        }
    }
}


static void cvg_vectadj(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_vectadj
 * 
 * Adjusts the given VG element by the information given in the CAP data
 * provided.
 *
 * Input parameters:
 *  id          Handle          Handle for the 'base' part of the object
 *  *el         VG_DBStruct     VG element to check for CAP information
 *
 * Output parameters:
 *  *el         VG_DBStruct     Adjusted VG element 
 *  *iret       int             Return code
 *                                 0 = Function successful
 *                                -1 = Invalid pointer in arguments
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC            3/06   Created
 ****************************************************************************/
{
    Placement   place;
    int         placed;
/*---------------------------------------------------------------------*/

    cap_psgetpl(cvg_placements, id, &place, iret);
    if (place) {
        cap_plgetplaced(place, &placed, iret);
        if (placed) {
        }
    }
}


static void cvg_volcadj(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_volcadj
 * 
 * Adjusts the given VG element by the information given in the CAP data
 * provided.
 *
 * Input parameters:
 *  id          Handle          Handle for the 'base' part of the object
 *  *el         VG_DBStruct     VG element to check for CAP information
 *
 * Output parameters:
 *  *el         VG_DBStruct     Adjusted VG element 
 *  *iret       int             Return code
 *                                 0 = Function successful
 *                                -1 = Invalid pointer in arguments
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC            3/06   Created
 ****************************************************************************/
{
    Placement   place;
    int         placed;
/*---------------------------------------------------------------------*/

    cap_psgetpl(cvg_placements, id, &place, iret);
    if (place) {
        cap_plgetplaced(place, &placed, iret);
        if (placed) {
        }
    }
}


static void cvg_wboxadj(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_wboxadj
 * 
 * Adjusts the given VG element by the information given in the CAP data
 * provided.
 *
 * Input parameters:
 *  id          Handle          Handle for the 'base' part of the object
 *  *el         VG_DBStruct     VG element to check for CAP information
 *
 * Output parameters:
 *  *el         VG_DBStruct     Adjusted VG element 
 *  *iret       int             Return code
 *                                 0 = Function successful
 *                                -1 = Invalid pointer in arguments
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC            3/06   Created
 ****************************************************************************/
{
    Placement   place;
    int         placed;
/*---------------------------------------------------------------------*/

    cap_psgetpl(cvg_placements, id, &place, iret);
    if (place) {
        cap_plgetplaced(place, &placed, iret);
        if (placed) {
        }
    }
}
