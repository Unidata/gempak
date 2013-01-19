#define	NMP_GLOBAL
#include "nmpcmn.h"

#define MAPAREA_FILE    "mapinfo.nmap" /* map area info file */
#define MAPOVL_FILE     "mapovl.nmap"  /* map overlay info file */
#define	TBL_DIR		"nmap"

void nmp_init ( int *iret )
/************************************************************************
 * nmp_init								*
 *                                                                      *
 * This function reads the mapinfo.tbl and mapovl.tbl tables into 	*
 * the structure.							*
 *                                                                      *
 * void nmp_init( iret )                                              	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *					 = 0  - OK			*
 *					 = -1 - map table not read	*
 *					 = -2 - overlay table not read	*
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC		11/00	Created					*
 * M. Li/GSC		12/00	Allocated saved_overlay			*
 * M. Li/GSC		12/00	Added nmp_setmap			*
 * M. Li/GSC		02/01	expanded map, lat/lon and STN		*
 * E. Safford/GSC	04/01	init imgfile, imgtyp			*
 * E. Safford/GSC	05/01	use SAT_STR && RAD_STR 			*
 * M. Li/GSC		05/01	replaced nmp_savovl with nmp_save	*
 * J. Wu/SAIC		08/01	allocate space for default_overlay[]	*
 * J. Wu/SAIC		08/01	save off default_map & default_overlay	*
 * J. Wu/SAIC		08/03	save marker size as float number	*
 * T. Piper/SAIC	08/04	Added check on nn (number of overlays)	*
 * T. Piper/SAIC	08/04	Added itype 5 (scale legend) support	*
 ***********************************************************************/
{
int   	active, color, ier, ii, itype, lat_opt, lp, nn, val_opt, which;
float	lat, msize;
char  	app[5][3], buffer[256], val_txt[64];
FILE  	*fp;
nmpovlstr_t title, gname, ovlattr;

/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     * Read table mapinfo.tbl
     */ 

    fp = cfl_tbop(MAPAREA_FILE, TBL_DIR, &ier);
    if (ier!= 0) {
	*iret = -1;
    }
    else {

        /*
         * figure out the total # of records
         */
        nn = 0;
        while ( !feof(fp) ) {

            /*
             * read a record
             */
            cfl_trln(fp, 256, buffer, &ier);

            if ( ier == 0 )  nn++;

        }
    }

    map_tbl = malloc((nn+EXTRA_PDFBTN)* sizeof(maptbl_ent_t));

    if ( nn > 0 ) {

	num_maps = nn;	

        rewind(fp);

        ii = 0;
        while ( ii < nn ) {

            cfl_trln(fp, 256, buffer, &ier);

            if ( ier == 0 ) {
                sscanf(buffer, "%s %s", map_tbl[ii].name,
                        map_tbl[ii].geog);
            }
	    ii++;
        } 
    } 

    if ( fp ) {
        fclose(fp);
    }

    /* 
     * Add SAT and Custom
     */
    num_maps += EXTRA_PDFBTN;

    /*
     * set satellite projection button
     */
    which = num_maps-SAT_BTN;
    strcpy(map_tbl[which].name, SAT_STR);
    strcpy(map_tbl[which].geog, "DSET");

    /*
     * set customize button
     */
    which = num_maps - CUSTOM_BTN;
    strcpy(map_tbl[which].name, RAD_STR);
    strcpy(map_tbl[which].geog, "\0");
   
    /*
     * Set the map object
     */
    nmp_setmap(map_tbl[0].name, 0, TRUE, &ier);
    for (lp = 0; lp < MAX_LOOP; lp++) {
	maps[lp].mode   = 1;
	maps[lp].imgtyp = NO_IMG;
	strcpy (maps[lp].imgfile, "");
    }

    /*
     * Read table mapovl.tbl
     */

    fp = cfl_tbop(MAPOVL_FILE, TBL_DIR, &ier);
    if (ier != 0) {
	*iret = -2;

	for (lp =0; lp < MAX_LOOP; lp ++) {
	    overlay[lp].novl = 0;
	} 
	return;
    }
    else {

        /*
         * figure out the total # of records
         */
        nn = 0;
        while ( !feof(fp) ) {

            /*
             * read a record
             */
            cfl_trln(fp, 256, buffer, &ier);

            if ( ier == 0 ) { 
                nn++;
            }
        }
    }
    if ( nn > MAX_OVL ) nn = MAX_OVL;	

    /* 
     * Load the table into the structure
     */
    if ( nn > 0 ) {

        for (lp=0; lp<MAX_LOOP; lp++) {
            overlay[lp].novl   = nn;
	    if (overlay[lp].mapovl == NULL) {
	        overlay[lp].mapovl = (overlay_t *)malloc(overlay[lp].novl*
                                        sizeof(overlay_t));
            }
        } 

	rewind(fp);

	ii = 0;
	while ( ii < nn ) {

            cfl_trln(fp, 256, buffer, &ier);

            if ( ier == 0 ) {
		sscanf ( buffer, "%s %d", title, &itype);
		if ( itype == 1 ) {
                    sscanf ( buffer, "%s %d %s %d %d %s %s",
                   	     title, &itype, gname, &active, &color,
	 	    	     app[0], app[1]);
			        
		    sprintf ( ovlattr, "%d %s %s", color, app[0], app[1] );
		}                
	        else if ( itype == 0 ) {
		    sscanf ( buffer, "%s %d %s %d %d %s %s %s %s",
			     title, &itype, gname, &active, &color,
			     app[0], app[1], app[2], app[3]); 
		
		    sprintf ( ovlattr, "%d %s %s %s %s", 
			      color, app[0], app[1], app[2], app[3] );
	        } 
	        else if ( itype == 2 ) {
                    sscanf ( buffer, "%s %d %s %d %d %s %s %s %s %s",
                            title, &itype, gname, &active, &color,
			    app[0], app[1], app[2], app[3], app[4] );

                    msize = (float) atof ( app[1] );
		    if ( msize < 0.5 || msize > 3.0 ) msize = 1.0;
			
		    sprintf ( ovlattr, "%d %s %2.1f %s %s %s",
                              color, app[0], msize, app[2],
			      app[3], app[4] );
		}	
		else if ( itype == 5 ) {
		    sscanf ( buffer, "%s %d %s %d %d %s %s %s %s %s %f %s",
			     title, &itype, gname, &active, &color,
			     app[0], app[1], app[2], app[3], app[4], &lat, val_txt );
		    if ( !ERMISS(lat) ) {
			lat_opt = 1;  /*  Manual mode  */
		    }
		    else {
			lat_opt = 0;  /*  Auto mode  */
		    }
		    cst_lcuc(val_txt, val_txt, &ier);
		    if ( strcmp(val_txt, "AUTO") == 0 ) {
			val_opt = 0;  /*  Auto mode  */
			val_txt[0] = '-';
			val_txt[1] = '\0';
		    }
		    else {
			val_opt = 1;  /*  Manual mode  */
		    } 

		    sprintf ( ovlattr, "%d %s %d %f %d %s %s %s %s %s", 
			color, app[0], lat_opt, lat, val_opt, val_txt, app[1], 
			app[2], app[3], app[4] );
		}

		for ( lp = 0; lp < MAX_LOOP; lp++ ) {                    
                    strcpy ( overlay[lp].mapovl[ii].title, title );
		    strcpy ( overlay[lp].mapovl[ii].gname, gname );
		    overlay[lp].mapovl[ii].ityp = itype;
		    overlay[lp].mapovl[ii].active = active;
		    strcpy ( overlay[lp].mapovl[ii].attr, ovlattr );
                }

                ii++;
            }  

        }

 	/* 
	 * Save a copy of map & overlay feature 
	 */
	for (lp=0; lp<MAX_LOOP; lp++) {
	
            strcpy(default_map[lp].imgfile, maps[lp].imgfile);
            default_map[lp].imgtyp = maps[lp].imgtyp;
            strcpy(default_map[lp].mapfile, maps[lp].mapfile);
            strcpy(default_map[lp].mapattr, maps[lp].mapattr);
            strcpy(default_map[lp].map, maps[lp].map);
            strcpy(default_map[lp].proj, maps[lp].proj);
            strcpy(default_map[lp].garea[0], maps[lp].garea[0]);
            strcpy(default_map[lp].garea[1], maps[lp].garea[1]);
            default_map[lp].mode = maps[lp].mode;	

	    if (default_overlay[lp].mapovl == NULL) {
                default_overlay[lp].mapovl = 
			(overlay_t *)malloc(nn * sizeof(overlay_t));
            }
	    
	    if (saved_overlay[lp].mapovl == NULL) {
                saved_overlay[lp].mapovl = 
			(overlay_t *)malloc(nn * sizeof(overlay_t));
            }
           
	    default_overlay[lp].novl = overlay[lp].novl;
            for (ii =0; ii < default_overlay[lp].novl; ii++ ) {
                default_overlay[lp].mapovl[ii].ityp = overlay[lp].mapovl[ii].ityp;
                strcpy (default_overlay[lp].mapovl[ii].attr,
                        overlay[lp].mapovl[ii].attr);
                strcpy (default_overlay[lp].mapovl[ii].gname,
                        overlay[lp].mapovl[ii].gname);
                strcpy (default_overlay[lp].mapovl[ii].title,
                        overlay[lp].mapovl[ii].title);
                default_overlay[lp].mapovl[ii].active = overlay[lp].mapovl[ii].active;
            }
	
	}
	
	nmp_save(&ier);
    }

    cfl_clos(fp, &ier);

}

/*=====================================================================*/
