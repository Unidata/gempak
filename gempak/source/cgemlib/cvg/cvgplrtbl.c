#define CVG_AUTOPLACE_CONF      /* to setup space for the placement config */
#define PLACEMENT_TBL "placement.tbl"
#include "cvgcmn.h"

static int typestr2int(char *str);
static int groupstr2int(char *str);

void cvg_plrtbl(int *iret)
/*****************************************************************************
 * cvg_plrtbl
 * 
 * Loads the placement configuration file into the local structures for later
 * use by the cvg_el2place functions.  If the function is called multiple 
 * times the file will only be re-read if the date/time stamp of the file
 * changes.  An empty table, or no table is ok as this will just disable the
 * auto-placement functionallity.
 *
 * Input parameters:
 *  None
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC            3/06   Created
 ****************************************************************************/
{
    FILE    *fp;
    Boolean enabled;
    long    filesize;
    int     quit, numrec, loglev, ier, ier1, fields;
    int     inside, both_sides, attempts, point_center;
    float   offset, increment;
    char    grp[4];
    struct  stat buf;
    char    top_typestr[64], groupstr[64], pl_typestr[64], ref_typestr[64];
    int     top_type, group, pl_type, ref_type, pl_subtype, ref_subtype;
    char    fullpath[LLPATH], tstr[LLMXLN];

    *iret = 0;

    /*
     * See first if placement is even enabled
     */
    ctb_pfbool ( "ENABLE_AUTOPLACE", &enabled, &ier );

    if (!enabled) {
        cvg_cap_conf_info_valid = 0;
        return;
    }

    /*
     * Get the full path to the file based on the name
     */
    cfl_tinq ( PLACEMENT_TBL, "pgen", &filesize, fullpath, &ier );

    /* 
     * No file, that's fine just leave the settings alone
     */
    if (ier != 0) {
        return;
    }

    /*
     * It should be there, make sure it is and check when it was last 
     * updated
     */
    ier = stat(fullpath, &buf);

    /* 
     * No file, or it hasn't bee updated.
     * That's fine just leave the settings alone
     */
    if (ier != 0 || buf.st_mtime <= place_conf_file_time) {
        return;
    }

    /*
     * Remember the update time so we can tell later if we need to
     * load it again
     */
    place_conf_file_time = buf.st_mtime;

    /*
     * Open the configuration table.  If we can't, just leave it alone
     */
    if ( (fp = (FILE * )cfl_tbop(PLACEMENT_TBL, "pgen", &ier)) == NULL) {
        loglev = 2;
        strcpy(grp, "CVG");
        er_lmsg (&loglev, grp, iret, SETTING_TBL, &ier1, strlen(grp),
                 strlen(SETTING_TBL));
        return;
    }

    /*
     * See if there is anything in the file worth looking at
     */
    cfl_tbnr( fp, &numrec, &ier);
    if (numrec < 1) {
        cfl_clos ( fp, &ier );
        return;
    }

    /*
     * Ok, we know there is a file that has something in it, 
     * reset all the local data so we can store at it.
     */
    memset(cvg_cap_by_type, 0, sizeof(cvg_cap_by_type));
    memset(cvg_cap_by_group, 0, sizeof(cvg_cap_by_group));
    cvg_cap_conf_info_valid = 0;
    quit = G_FALSE;
    while ( !quit ) {

        cfl_trln ( fp, sizeof(tstr), tstr, &ier );

        if ( ier == 4 ) {
            /*
             * Here for end of file.
             */
            quit = G_TRUE;
        } else if ( ier != 0 ) {
            /*
             * Here for a bad read; record is ignored.
             */
            loglev = 2;
            strcpy(grp, "CVG");
            ier = 2;
            er_lmsg ( &loglev, grp, &ier, PLACEMENT_TBL, &ier1,
                      strlen(grp), strlen(PLACEMENT_TBL) );
        } else {
            fields = sscanf(tstr, "%s %s %s %d %s %d %d %d %d %f %f %d", 
                    top_typestr, groupstr, pl_typestr, &pl_subtype,
                    ref_typestr, &ref_subtype, &inside, &both_sides, 
                    &attempts, &offset, &increment, &point_center
                );

            if (fields == 12) {
                top_type = typestr2int(top_typestr);
                group    = groupstr2int(groupstr);
                /*
                 * Only one of the top_type or group is allowed per line
                 * For non-group types, so far we only allow GFA_ELM 
                 * objects
                 */
                if ((group != 0 && top_type == 0) ||
                    (group == 0 && top_type == GFA_ELM)) {
                    pl_type  = typestr2int(pl_typestr);
                    ref_type = typestr2int(ref_typestr);
                    if (pl_type != 0 && ref_type != 0) {
                        /*
                         * Make sure the rest is 'sane', then save it
                         */
                        if ((inside == 0 || inside == 1) &&
                            (both_sides == 0 || both_sides == 1) &&
                            (attempts > 0) &&
                            (offset >= 0) &&
                            (increment > 0) &&
                            (point_center == 0 || point_center == 1)) {
                            cvg_cap_conf_info_valid = 1;
                            if (group != 0) {
                                cvg_cap_by_group[group].enabled = 1;
                                cvg_cap_by_group[group].obj_type = pl_type;
                                cvg_cap_by_group[group].obj_subtype = pl_subtype;
                                cvg_cap_by_group[group].ref_type = ref_type; 
                                cvg_cap_by_group[group].ref_subtype = ref_subtype; 
                                cvg_cap_by_group[group].inside = inside;
                                cvg_cap_by_group[group].both_sides = both_sides;
                                cvg_cap_by_group[group].attempts = attempts;
                                cvg_cap_by_group[group].offset = offset;
                                cvg_cap_by_group[group].increment = increment;
                                cvg_cap_by_group[group].point_center = point_center;
                            } else {
                                cvg_cap_by_type[top_type].enabled = 1;
                                cvg_cap_by_type[top_type].obj_type = pl_type;
                                cvg_cap_by_type[top_type].obj_subtype = pl_subtype;
                                cvg_cap_by_type[top_type].ref_type = ref_type; 
                                cvg_cap_by_type[top_type].ref_subtype = ref_subtype; 
                                cvg_cap_by_type[top_type].inside = inside;
                                cvg_cap_by_type[top_type].both_sides = both_sides;
                                cvg_cap_by_type[top_type].attempts = attempts;
                                cvg_cap_by_type[top_type].offset = offset;
                                cvg_cap_by_type[top_type].increment = increment;
                                cvg_cap_by_type[top_type].point_center = point_center;
                            }
                        } else {
                            /*
                             * Warning?
                             */
                        }
                    }
                } else {
                    /*
                     * Warning?
                     */
                }
            } else {
                /*
                 * Warning?
                 */
            }
        }
    }
    cfl_clos ( fp, &ier );
}


static int typestr2int(char *typestr)
/*****************************************************************************
 * typestr2int
 * 
 * Returns the integer corresponding to the given VG type string
 *
 * Input parameters:
 *  *typestr    char            String to convert
 *
 * Output parameters:
 *  None
 *
 * Return value:
 *              int             Integer coresponding to the string
 **
 * Log:
 * S.Danz/AWC            3/06   Created
 * L. Hinson/AWC         1/12   Add SGWX_ELM
 ****************************************************************************/
 {
    int result  = 0;

    if (strcmp(typestr, "ARROW_ELM") == 0) result = ARROW_ELM;
    else if (strcmp(typestr, "ASHCLD_ELM" ) == 0) result = ASHCLD_ELM;
    else if (strcmp(typestr, "BARB_ELM") == 0) result = BARB_ELM;
    else if (strcmp(typestr, "CIRCLE_ELM") == 0) result = CIRCLE_ELM;
    else if (strcmp(typestr, "CMBSY_ELM") == 0) result = CMBSY_ELM;
    else if (strcmp(typestr, "CTSYM_ELM") == 0) result = CTSYM_ELM;
    else if (strcmp(typestr, "DARR_ELM") == 0) result = DARR_ELM;
    else if (strcmp(typestr, "FRONT_ELM" ) == 0) result = FRONT_ELM;
    else if (strcmp(typestr, "GFA_ELM") == 0) result = GFA_ELM;
    else if (strcmp(typestr, "HASH_ELM") == 0) result = HASH_ELM;
    else if (strcmp(typestr, "ICSYM_ELM") == 0) result = ICSYM_ELM;
    else if (strcmp(typestr, "JET_ELM") == 0) result = JET_ELM;
    else if (strcmp(typestr, "LINE_ELM") == 0) result = LINE_ELM;
    else if (strcmp(typestr, "LIST_ELM") == 0) result = LIST_ELM;
    else if (strcmp(typestr, "MARK_ELM" ) == 0) result = MARK_ELM;
    else if (strcmp(typestr, "PTSYM_ELM") == 0) result = PTSYM_ELM;
    else if (strcmp(typestr, "PWSYM_ELM") == 0) result = PWSYM_ELM;
    else if (strcmp(typestr, "SIGAIRM_ELM" ) == 0) result = SIGAIRM_ELM;
    else if (strcmp(typestr, "SIGCCF_ELM" ) == 0) result = SIGCCF_ELM;
    else if (strcmp(typestr, "SIGCONV_ELM" ) == 0) result = SIGCONV_ELM;
    else if (strcmp(typestr, "SIGINTL_ELM" ) == 0) result = SIGINTL_ELM;
    else if (strcmp(typestr, "SIGNCON_ELM" ) == 0) result = SIGNCON_ELM;
    else if (strcmp(typestr, "SIGOUTL_ELM" ) == 0) result = SIGOUTL_ELM;
    else if (strcmp(typestr, "SGWX_ELM" ) == 0) result = SGWX_ELM;
    else if (strcmp(typestr, "SKSYM_ELM") == 0) result = SKSYM_ELM;
    else if (strcmp(typestr, "SPLN_ELM") == 0) result = SPLN_ELM;
    else if (strcmp(typestr, "SPSYM_ELM") == 0) result = SPSYM_ELM;
    else if (strcmp(typestr, "SPTX_ELM") == 0)  result = SPTX_ELM;
    else if (strcmp(typestr, "TBSYM_ELM") == 0) result = TBSYM_ELM;
    else if (strcmp(typestr, "TCA_ELM") == 0) result = TCA_ELM;
    else if (strcmp(typestr, "TEXTC_ELM") == 0) result = TEXTC_ELM;
    else if (strcmp(typestr, "TEXT_ELM") == 0) result = TEXT_ELM;
    else if (strcmp(typestr, "TRKSTORM_ELM" ) == 0) result = TRKSTORM_ELM;
    else if (strcmp(typestr, "VOLC_ELM" ) == 0) result = VOLC_ELM;
    else if (strcmp(typestr, "WBOX_ELM") == 0)  result = WBOX_ELM;
    else if (strcmp(typestr, "WCNTY_ELM") == 0)  result = WBOX_ELM;
    else if (strcmp(typestr, "WSM_ELM") == 0)  result = WSM_ELM;
    else if (strcmp(typestr, "WXSYM_ELM") == 0) result = WXSYM_ELM;

    return result;
}


static int groupstr2int(char *groupstr)
/*****************************************************************************
 * groupstr2int
 * 
 * Returns the integer corresponding to the given group name string
 *
 * Input parameters:
 *  *groupstr   char            String to convert
 *
 * Output parameters:
 *  None
 *
 * Return value:
 *              int             Integer coresponding to the string
 **
 * Log:
 * S.Danz/AWC            3/06   Created
 ****************************************************************************/
 {
    int result, ier;

    result = 0;

    /* 
     * First try with the CES tables
     */
    ces_gtgid (groupstr, &result, &ier);

    /*
     * The CES routine does not have entries for things like CCFP
     * FIXME: Maybe it should be added?  Need to find out
     */
    if (ier == -1) {
        if (strcmp(groupstr, "CCFP") == 0) result = GRPTYP_CCF;
        else if (strcmp(groupstr, "SGWX") == 0) result = GRPTYP_SGWX;
        else if (strcmp(groupstr, "WATCH") == 0) result = GRPTYP_WATCH;
        else if (strcmp(groupstr, "COMSYM") == 0) result = GRPTYP_COMSYM;
    }

    return result;
 }
