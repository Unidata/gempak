
/************************************************************************
 * nmap_stnm.h                                                          *
 *                                                                      *
 * Header file for station plot model.       				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      6/96                                                 *
 * C. Lin/EAI      4/97		take out marker field                   *
 * A. Hardy/GSC	   4/00		added font size multiplier		*
 ***********************************************************************/

typedef struct {
        char   skpmis;              /* skip */
        float  filter;              /* filter factor */
        char   parm[128];           /* parameter string  */
        char   colors[128];         /* color string  */
        int    ifont;               /* font index */
        int    ihwsw;               /* flag for hardware/software text */
        int    itxtw;               /* width of text */
        float  txtsiz;              /* text size */
        int    mult;                /* multiplier size */
        char   vcoord[73];          /* vertical coordinates */
        char   level[73];           /* vertical level */
} stnm_t;

/*=====================================================================*/

