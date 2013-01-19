#define CVG_META_DATA       /* To define the global variables for metadata */

#include "cvgcmn.h"
#include "drwids.h"
#include "pgprm.h"

static void print_object(FILE *outfp, PlacementSet placements, CMDObject object);
static void dump_objects(char *output, CMDObjectSet objects, PlacementSet placements);

void cvg_load ( char *fname, Boolean selflag, Boolean wrtflg, 
			Boolean plotflg, int layer, int icol, int *iret )
/************************************************************************
 * cvg_load								*
 *									*
 * This function loads data from a VGF formatted file and converts 	*
 * encountered records to a set of actions that display the feature	*
 * to an appropriate driver.  The parameter icol controls whether the	*
 * graphics will be plotted with the file-specified colors (icol=0)	*
 * or whether the VGF file will be plotted monochrome (icol=other).	*
 *									*
 * void cvg_load ( fname, selflag, wrtflg, plotflg, layer, icol, iret )	*
 *									*
 * Input parameters:							*
 *	*fname		char		Name of file to load		*
 *	selflag		Boolean		Are elements "selectable"	*
 *	wrtflg		Boolean		Write flag for file		*
 *	plotflg		Boolean		Display the elements or not	*
 *	layer		int		Layer the file contents will be	*
 *					   assigned to			*
 *	icol		int		Plot color			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  1 = elms in file exceeded 	*
 *				              (MAX_EDITABLE_ELEMS)	*
 *					 -1 = error opening VG file	*
 *					 -2 = error closing VG file	*
 *					 -4 = error reading el hdr      *
 *					 -5 = VG file is empty		*
 **									*
 * Log:									*
 * E. Wehner/EAi	10/96	Created					*
 * D. Keiser/GSC	 1/97	Clean up				*
 * E. Wehner/EAi	 2/97	Added selection flag.			*
 * E. Wehner/EAi	 5/97	Add double tier loading 		*
 * E. Wehner/EAi	 5/97	Stop loading if > MAX_EDITABLE_ELEMS	*
 * E. Wehner/EAi	 5/97	Added refresh after displaying 		*
 * D. Keiser/GSC	 5/97	Change cvg_dsply to cds_dspvg		*
 * S. Jacobs/NCEP	 6/97	Added write flag			*
 * C. Lin/NCEP	 	 6/97	Bug fix in calling cfl_ropn 		*
 * E. Wehner/EAi	 6/97	Added check to not load header		*
 * E. Wehner/EAi	 7/97	Filled areas on all elements		*
 * E. Wehner/EAi	 8/97	Removed the graphics info record.	*
 * E. Wehner/EAi	 9/97	Allow NULL file name			*
 * F. Yen/NCEP		 1/98	Updated calls for crg library cleanup	*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * F. J. Yen/NCEP	 5/98	Updated cds function names		*
 * E. Safford/GSC	10/98	added display levels revised file reads *
 * T. Piper/GSC		10/98	Prolog update				*
 * E. Safford/GSC	12/98	move display levels to pgprm.h		*
 * S. Jacobs/NCEP	 3/99	Added symbols to the last display level	*
 * A. Hardy/GSC          1/01   changed fptr from int to FILE           *
 * J. WU/SAIC           11/01   change prototype & add plotflg param	*
 * J. WU/SAIC           12/01   add layer param to feed crg_set()	*
 * J. WU/SAIC           12/01   locate displaying level	with cvg_level  *
 * T. Lee/SAIC		11/03	changed .DEFAULT.vgf to work_file	*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 * M. Li/SAIC		08/04	Added time filter			*
 * B. Yin/SAIC          08/04   Added code to free TCA memory           *
 * B. Yin/SAIC          08/04   changed pgtca_freeBkpts to cvg_freeBkpts*
 * J. Wu/SAIC		10/04	free GFA block pointers			*
 * S. Danz/AWC          03/06   Call autoplacement if enabled           *
 * J. Wu/SAIC		06/06	call cvg_matchfilter 			*
 * M. Li/SAIC           03/07   Updated cvg_matchfilter                 *
 * m.gamazaychikov/SAIC	06/07	Add code to free TCB and TCT memory	*
 ***********************************************************************/
{
    int		    curpos, ier, flag, el_level;
    long	    maxbytes;
    int		    ier1, loglev, ic, icx, ii, jj, kk, one=1;
    int		    elms[3][MAX_EDITABLE_ELEMS], lvl_cnt[LEVELS], total_elms;
    char	    newfil[LLPATH], reqfil[LLPATH], grp[4], dbgfile[LLPATH];
    Boolean         autopl_enabled;
    VG_DBStruct	    el;
    filter_t	    el_filter, timeMatched;
    Boolean	    filter_match, matchAny = False;
    FILE	    *fptr;
    CMDObjectSet    nmap_metadata = NULL;
    PlacementSet    nmap_placements = NULL;
    cvg_group_info  *nmap_group_check = NULL;
/*---------------------------------------------------------------------*/

    ic = icol;
    icx = 256;
    if ( ic > icx || ic < 0 )  ic = 0;
    cds_scol ( ic, iret );

    strcpy(grp, "CVG");
    loglev = 0;
    *iret = 0;
    curpos  = 0;
    maxbytes = 0;    

    if (!fname) {
	strcpy(reqfil, work_file);
    }
    else {
	strcpy(reqfil, fname);
    }


    cvg_open(reqfil, (int)wrtflg, &fptr, &ier);

    if (( ier != 0 ) || ( fptr == NULL )) {
	*iret = -1;
	er_lmsg( &loglev, grp, &ier, reqfil, &ier1, strlen(grp),strlen(reqfil));
	return;
    }

    cfl_inqr(reqfil, NULL, &maxbytes, newfil, &ier);
    if (ier < 0) {
	*iret = -1;
	er_lmsg( &loglev, grp, &ier, reqfil, &ier1, strlen(grp),strlen(reqfil));
	return;
    }

    /*
     * Get each el.hdr and assign each elem offset to one of the levels.
     */
    for (ii = 0; ii < 3; ii++) {
        lvl_cnt[ii] = 0;
    }

    /*
     *  Scan the vg file and load the offsets of all non-deleted elements.
     *  Stop loading at end of file or MAX_EDITABLE_ELEMS.
     */
    total_elms = 0;
    while (((long)curpos < maxbytes) && (total_elms < MAX_EDITABLE_ELEMS))  {

        cvg_rdhdr(newfil, fptr, curpos, (int)maxbytes, &el, &flag, &ier);
	if (ier < 0 || el.hdr.recsz < 0 || el.hdr.recsz > 65536) {
	    *iret = -4;
	    return;
	}

        cvg_level( &el.hdr, &el_level, &ier );
	if ( ier == 0 ) {	
	    elms[ el_level ] [ lvl_cnt[el_level]++ ] = curpos;
	    total_elms++;
	}
		
 	curpos += el.hdr.recsz;	
    }

    if (total_elms >= MAX_EDITABLE_ELEMS) {
	*iret = 1;
	er_lmsg( &loglev, grp, &ier, newfil, &ier1, 
						strlen(grp),strlen(reqfil));
    }

    /*
     *  Create the structures to manage the metadata and object placement
     *  information.  If there were structures already in place, hold them
     *  aside as they were put there by product generation.
     */
    ctb_pfbool( "ENABLE_AUTOPLACE", &autopl_enabled, &ier );
    if (autopl_enabled) {
        if (cvg_metadata) {
            nmap_metadata = cvg_metadata;
            nmap_placements = cvg_placements;
            nmap_group_check = cvg_group_check;
        }
        cmd_osnew(&cvg_metadata, &ier);
        cap_psnew(&cvg_placements, &ier);
        G_CALLOC(cvg_group_check, cvg_group_info, one, "cvg_load: cvg_group_check");

        /*
         * Fill in the structures with the information needed
         */
        cvg_ld4place(fname, iret);

        /*
         * Ok, its all loaded, so place all the objects where they belong
         */
        cap_psplace(cvg_placements, cvg_metadata, iret);

        ctb_pfstr("AUTOPLACE_DBG", dbgfile, &ier );
        if (dbgfile[0] != '\0') {
            dump_objects(dbgfile, cvg_metadata, cvg_placements);
        }
    }

    /*
     *  Now loop thru the levels and display the elements
     */
    kk = 0;

    for (ii = 0; ii < LEVELS; ii++) {

	for (jj=0; jj<lvl_cnt[kk]; jj++) {
	    curpos = elms[ii][jj];
            cvg_rdhdr(newfil, fptr, curpos, (int)maxbytes, &el, &flag, &ier);
	    cvg_rdele(&el, curpos, el.hdr.recsz, fptr, &ier);

	    cvg_getElFilter ( &el, el_filter, &ier ); 

            cvg_matchfilter ( el_filter, matchAny, &filter_match, timeMatched, &ier );

	    if (selflag) {
	        crg_set(&el, curpos, layer, &ier);
	    }

	    if ( filter_match ) {
	        cds_dspelm( &el, &ier); 
	    }

            /*
             * Free TCA/GFA/TCT/TCB memory
             */
            if ( el.hdr.vg_type == TCA_ELM ) {
                cvg_freeBkpts ( &el );
            }
            else if ( el.hdr.vg_type == GFA_ELM ) {
                cvg_freeElPtr ( &el );
            }
            else if ( el.hdr.vg_type == TCTRK_ELM ) {
                cvg_freeTct ( &el );
            }
            else if ( el.hdr.vg_type == TCBKL_ELM ) {
                cvg_freeTcb ( &el );
	    }
	}

	kk++;
    }

    cvg_clos(fptr, &ier);
    if ( ier != 0 )
	*iret = -2;

    if ( plotflg ) geplot(&ier);
    ic = 0;
    cds_scol ( ic, iret );

    /*
     * Free up the metadata and placement information now that we 
     * are done plotting everything
     */
    if (autopl_enabled) {
        cmd_osdel(cvg_metadata, iret);
        cap_psdel(cvg_placements, iret);
        G_FREE(cvg_group_check, cvg_group_info);

        /*
         * Put back the product generation metadata
         */
        cvg_metadata = nmap_metadata;
        cvg_placements = nmap_placements;
        cvg_group_check = nmap_group_check;
    }

    return;

}


/* Some utility 'dump' functions for testing */

static void
print_object(
    FILE            *outfp,
    PlacementSet    placements,
    CMDObject       object
) {
    Handle          id;
    Handle          ref;
    const float     *xpts;
    const float     *ypts;
    int             points;
    int             iret;
    Placement       place;
    int             placed;
    float           cntr_x;
    float           cntr_y;
    float           offset_x;
    float           offset_y;
    float           arrow_x[2];
    float           arrow_y[2];
    int             in_center;
    int             ispoly;
    int             isvisible;
    int             vertex;
    int             opt_placecenter;
    int             opt_pointcenter;
    int             opt_sides;
    int             opt_max_tries ;
    float           opt_dist;
    float           opt_offset;

    if (object) {
        cmd_obgetid(object, &id, &iret);
        cmd_obgetpoints(object, &xpts, &ypts, &points, &iret);
        cmd_obgetcntr(object, &cntr_x, &cntr_y, &iret);
        cmd_obispoly(object, &ispoly, &iret);
        cmd_obisvisible(object, &isvisible, &iret);
        if (ispoly) {
            fprintf(outfp,"{\n        {type polygon}\n");
        } else {
            fprintf(outfp,"{\n        {type line}\n");
        }

        if (isvisible) {
            fprintf(outfp,"        {visible}\n");
        } else {
            fprintf(outfp,"        {notvisible}\n");
        }
        fprintf(outfp,"        {id %d}\n", id);

        cap_psgetpl(placements, id, &place, &iret);
        placed = 0;
        in_center = 0;
        offset_x = 0;
        offset_y = 0;
        if (place) {
            cap_plgetcenter(place, &opt_placecenter, &iret);
            fprintf(outfp,"        {placecenter %d}\n", opt_placecenter);
            cap_plgetpcntr(place, &opt_pointcenter, &iret);
            fprintf(outfp,"        {pointcenter %d}\n", opt_pointcenter);
            cap_plgetdistincr(place, &opt_dist, &iret);
            fprintf(outfp,"        {dist %f}\n", opt_dist);
            cap_plgetdistoffset(place, &opt_offset, &iret);
            fprintf(outfp,"        {offset %f}\n", opt_offset);
            cap_plgetsides(place, &opt_sides, &iret);
            fprintf(outfp,"        {sides %d}\n", opt_sides);
            cap_plgettries(place, &opt_max_tries, &iret);
            fprintf(outfp,"        {tries %d}\n", opt_max_tries);

            cap_plgetref(place, &ref, &iret);
            fprintf(outfp,"        {ref %d}\n", ref);
            cap_plgetplaced(place, &placed, &iret);
            if (placed) {
                cap_plgetincntr(place, &in_center, &iret);
                cap_plgetoffset(place, &offset_x, &offset_y, &iret);
                cap_plgetline(place, arrow_x, arrow_y, &iret);
            } else {
                fprintf(outfp,"        {notplaced}\n");
            }
        }
        for (vertex = 0; vertex < points; vertex++) {
            fprintf(outfp,"        {vertex %.4f %.4f}\n", xpts[vertex] + offset_x, 
                    ypts[vertex] + offset_y
                );
        }

        if (placed && !in_center) {
            fprintf(outfp,"        {arrow %.4f %.4f %.4f %.4f}\n", arrow_x[0], arrow_y[0], 
                    arrow_x[1], arrow_y[1]
                );
        }
        fprintf(outfp,"}\n");
    }
}


static void
dump_objects(
    char            *output,
    CMDObjectSet    objects,
    PlacementSet    placements
) {
    FILE        *outfp;
    CMDObject   object;
    float       minx;
    float       miny;
    float       maxx;
    float       maxy;
    int         usestdout, iret;
    char        testname[LLPATH];

    if (output == NULL || output[0] == '\0') {
        return;
    }

    /* 
     * First see if we should go to the screen or use the file name
     */

    cst_lcuc(output, testname, &iret);
    if (strcmp("STDOUT", testname) == 0) {
        outfp = stdout;
        usestdout = 1;
    } else {
        outfp = cfl_wopn(output, &iret);
        usestdout = 0;
    }

    if (!outfp) {
        return;
    }

    if (!objects || !placements) {
        fprintf(outfp, "{nothing}\n");
        return;
    }

    cap_psgetarea(placements, &minx, &maxx, &miny, &maxy, &iret);
    fprintf(outfp,"{\n");
    fprintf(outfp,"        {area %.4f %.4f %.4f %.4f}\n",minx, maxx, miny, maxy);
    fprintf(outfp,"}\n");

    cmd_ositerinit(objects, &iret);
    cmd_ositernext(objects, &object, &iret);
    while (object) {
        print_object(outfp, placements, object);
        cmd_ositernext(objects, &object, &iret);
    }

    fflush(outfp);

    if (!usestdout) {
        cfl_clos(outfp, &iret);
    }
}

