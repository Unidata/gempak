
/************************************************************************
 * nmap_stbl.h                                                          *
 *                                                                      *
 * Header file for the station model table structures			*
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI	03/97							*
 ***********************************************************************/

typedef struct {
        char    *title;  /* station model name shown in data selection */
        char    *alias;  /* station model alias name */
	float   filter;
}sfc_ent_t; /* sfc resource data struct */

typedef struct {
    int        nitems; /* total # of station models */
    sfc_ent_t *items;  /* pointer to the array of station models */
}sfcstbl_t;

typedef struct {
        char    *title;  /* station model name shown in data selection */
        char    *alias;  /* station model alias name */
        char    *vcoord; /* GEMPAK vcoord for this station model */
        int     level;   /* default vertical level */
}snm_ent_t; /* sfc resource data struct */

typedef struct {
    int        nitems; /* total # of station models */
    snm_ent_t *items;  /* pointer to the array of station models */
}snmstbl_t;
