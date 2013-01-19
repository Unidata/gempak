#include "crgcmn.h"
#include "vgstruct.h"

void crg_gettxtbox ( VG_DBStruct *el, int boxflag, float *box_x, float *box_y )
/************************************************************************
 * crg_gettxtbox							*
 *									*
 * This function returns the coordinates for a box which would contain	*
 * the text after taking into consideration the text font, size,	*
 * alignment, and rotation.  Based on the boxflag, the output is either	*
 * a box for ghost text (rotated if necessary, no extra padding, and	*
 * the coordinates are relative to the original device location) or a	*
 * box used for refresh (a box with no rotation circumscribed around	*
 * the ghost text box, extra padding added, absolute device		*
 * coordinates).							*
 *									*
 * crg_gettxtbox ( el, boxflag, box_x, box_y ) 				*
 *									*
 * Input parameters:                                                    *
 *	*el		VG_DBStruct	Element containing text		*
 *	boxflag		int		Flag:  0 - ghost, 1 - refresh	*
 *	                                       2 - rotated ghost        *
 *									*
 * Output parameters:                                                   *
 *	*box_x		float		x coordinates [4]		*
 *	*box_y		float		y coordinates [4]		*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * S. Law/GSC		07/98	Moved from crg_settxt			*
 * W. Li/EAI		10/98	Added text box for north rotation	*
 * E. Safford/GSC	10/98	mod to handle offset text		*
 * S. Jacobs/NCEP	10/98	Changed gazdrm to gp_azdr		*
 * T. Piper/GSC		10/98	Prolog update				*
 * E. Safford/GSC	10/98	mod to include lat/lon pt - offset text	*
 * W. LI/EAI		01/99	Added "Filled under line" for text	*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations	*
 * M. Li/SAIC		11/01	Added icing				*
 * H. Zeng/EAI          05/02   removed "extra" variable                *
 * T. Lee/SAIC		02/02	Added overline text			*
 * J. Wu/SAIC		03/03	add midlevel cloud text			*
 * S. Jacobs/NCEP	11/03	Fixed coord sys for computing offset box*
 * S. Danz/AWC          03/06   Updated computation used to compute box *
 * L. Hinson/AWC        12/06   Added sptxt typ 16 code for GFA text lbl*
 * L. Hinson/AWC        12/07   For sptxt typ 16, bypass col/row calc   *
 *                              for mnemonic strings IBDR/BGFN          * 
 * L. Hinson/AWC        05/08   Fixed sptxt typ 16, problem with spaces *
 * L. Hinson/AWC        06/08   Fixed sptxt typ 16, problem with        *
 *                              underscores in Freezing Level text      *
 * L. Hinson/AWC        07/09   Add support for sptxt typ 17 for CCF txt*
 ***********************************************************************/
{
    char	*txtptr, *ptr;
    char	**txtarry, *token;

    int		ii, jj, ier, np, sptx, ithw, font, style;
    int		jalign, ioffx, ioffy, irrotn, turbsyml, turbsymr;
    int		rows, cols, longest, quit, npts, in_box, wrapslash;
    int		numcld, mclditem = 8, strsiz = 256, numgfa=0, numelem=0;
    int		num_mtyp, num_mcod, num_mtstrm, row1, row2;
    int         symcols, symrows;

    float       rx, ry, srx, sry, nrotn, srotn;
    float	tpad, bpad, sizmult, hboxsz, sztxx;
    float	szmk, sztx, szwb, szws, szab, szah;
    float	bcos, bsin, xoff, yoff, rotn, hfnt = 1.0F, vfnt = 1.0F;
    float	elwidth, elheight, theight, twidth, xmin, xmax, ymin, ymax;
    float	aleft, arght, abot, atop, apad, hpad;
    float	rot_x[4], rot_y[4], swhfnt, swvfnt;

    double	radrotn;
/*---------------------------------------------------------------------*/

    /*
     * Define variables based on whether it is a special text or not
     */
    if (el->hdr.vg_type == SPTX_ELM) {

	rx = el->elem.spt.info.lat;
	ry = el->elem.spt.info.lon;
	sizmult = el->elem.spt.info.sztext;
	ithw = el->elem.spt.info.ithw;
	font = el->elem.spt.info.itxfn % 10;
	style = el->elem.spt.info.itxfn / 10;
	jalign = el->elem.spt.info.ialign + 2;
	txtptr = el->elem.spt.text;
	ioffx  = el->elem.spt.info.offset_x;
	ioffy  = el->elem.spt.info.offset_y;

	irrotn = ( (int)el->elem.spt.info.rotn / 1000 ) + 1;
	rotn = (float) fmod ( (double)el->elem.spt.info.rotn, 1000. );
	sptx = el->elem.spt.info.sptxtyp;
    }
    else {

	rx = el->elem.txt.info.lat;
	ry = el->elem.txt.info.lon;
	sizmult = el->elem.txt.info.sztext;
	ithw = el->elem.spt.info.ithw;
	font = el->elem.spt.info.itxfn % 10;
	style = el->elem.spt.info.itxfn / 10;
	jalign = el->elem.txt.info.ialign;
	txtptr = el->elem.txt.text;
	ioffx  = el->elem.txt.info.offset_x;
	ioffy  = el->elem.txt.info.offset_y;

	irrotn = ( (int)el->elem.txt.info.rotn / 1000 ) + 1;
	rotn = (float) fmod ( (double)el->elem.txt.info.rotn, 1000. );

	sptx = 0;
    }
    /*
     * Get text size...
     */
    gqsizd ( &szmk, &sztx, &szwb, &szws, &szab, &szah, &ier );

    /*
     * To get actual pixel size, multiply base (device pixel size)
     * by size multiplier.
     */
    sztxx = sztx * sizmult;

    swhfnt = 0.78F;
    swvfnt = 0.98F;
    if (ithw == 2) {	/* Hardware fonts */
	switch (font) {
	case 1:		/* Courier */
	    hfnt = 0.75F;	/* horizontal font type multiplier */
	    vfnt = 1.10F;	/*   vertical font type multiplier */
	    break;
	case 2:		/* Helvetica */
	    hfnt = 0.68F;
	    vfnt = 1.15F;
	    break;
	case 3:		/* Times */
	    hfnt = 0.63F;
	    vfnt = 1.00F;
	    break;
	}
    }
    else {		/* Software fonts */
	switch (font) {
	case 1:		/* Software */
	    hfnt = swhfnt;
	    vfnt = swvfnt;
	    break;
	case 2:		/* SOFTWARE */
	    hfnt = swhfnt;
	    vfnt = swvfnt;
	    break;
	}
    }

    wrapslash = 0;
    hpad = 0;
    switch (sptx) {
      case  0:	/* unadorned text */
	tpad  = 0.00F;	/* vertical additive padding */
	bpad  = 0.00F;	/* vertical additive padding */
	break;
      case  1:	/* low  pressure */
	tpad  = 0.30F;
	bpad  = 0.78F;
	hpad  = 0.30F;
        /* the size of the box is always computed based on the SW fonts */
        hfnt  = swhfnt;
	break;
      case  2:	/* high pressure */
	tpad  = 0.78F;
	bpad  = 0.30F;
	hpad  = 0.30F;
        /* the size of the box is always computed based on the SW fonts */
        hfnt  = swhfnt;
	break;
      case  3:	/* unfilled box */
      case  4:	/*   filled box */
	tpad  = 0.30F;
	bpad  = 0.30F;
	hpad  = 0.30F;
        break;
      case  5:	/* just filled, ok? */
	tpad  = 0.22F;
	bpad  = 0.22F;
	hpad  = 0.30F;
        /* the width of the box is always computed based on the SW fonts */
        hfnt  = swhfnt;
	break;
      case  6:	/* freeze level */
	tpad  = 0.30F;
	bpad  = 0.30F;
	hpad  = 0.30F;
        /* the width of the box is always computed based on the SW fonts */
        hfnt  = swhfnt;
	break;
      case  7:	/* low turb */
	tpad  = 0.85F;
	bpad  = 0.00F;
	hpad  = 0.30F;
	break;
      case  8:	/* cloud top */
	tpad  = 0.22F;
	bpad  = 0.22F;
	hpad  = 0.30F;
        wrapslash=1;
        /* the length of the line is always computed based on the SW fonts */
        hfnt  = swhfnt;
	break;
      case  9:	/* high turb */
	tpad  = 0.85F;
	bpad  = 0.00F;
	hpad  = 0.30F;
        wrapslash=1;
        /* the length of the line is always computed based on the SW fonts */
        hfnt  = swhfnt;
	break;
      case 10:	/* underlined */
      case 11:	/* filled underlined */
	tpad  = 0.00F;
	bpad  = 0.30F;
	hpad  = 0.30F;
        /* the width of the line is always computed based on the SW fonts */
        hfnt  = swhfnt;
	break;
      case 13:	/* overlined */
      case 14:	/* filled overlined */
	tpad  = 0.30F;
	bpad  = 0.00F;
	hpad  = 0.30F;
        /* the width of the line is always computed based on the SW fonts */
        hfnt  = swhfnt;
	break;
      case 12:  /* icing */
        tpad  = 0.85F;
        bpad  = 0.00F;
        hpad  = 0.30F;
        if (el->elem.spt.info.turbsym > 1) {
            tpad += 0.17F;
        }
        wrapslash=1;
        /* the length of the line is always computed based on the SW fonts */
        hfnt  = swhfnt;
        break;
      case 15:  /* midlevel cloud */
        tpad  = 0.30F;
        bpad  = 0.30F;
        hpad  = 0.30F;
        /* the size of the box is always computed based on the SW fonts */
        hfnt  = swhfnt;
	vfnt = swvfnt;
        break;
      case 16:  /* GFA Element */
      case 17:  /* CCF Element */
        tpad  = 0.30F;
        bpad  = 0.30F;
        hpad  = 0.30F;
        /* the size of the box is always computed based on the SW fonts */
        hfnt  = swhfnt;
	vfnt = swvfnt;
        break;
    }

    /* add/delete top spacing for turb based on the symbol being used */
    if (sptx == 7 || sptx == 9) {
        turbsymr = el->elem.spt.info.turbsym % 10;
        turbsyml = el->elem.spt.info.turbsym / 10 - 1;
        if (turbsyml == 6 || turbsymr == 6) {
            tpad += 0.50F;
        } else if (el->elem.spt.info.turbsym == 5) {
            tpad += 0.17F;
        } else if (el->elem.spt.info.turbsym == 0) {
            tpad -= 0.17F;
        } else if (el->elem.spt.info.turbsym > 6) {
            tpad += 0.25F;
        }
    }

    /* Increase top/bottom padding for bold software fonts */
    if (style > 1 && ithw != 2) {
        tpad += 0.08;
        bpad += 0.08;
    }

    /* scale the padding based on the font multiplier */
    tpad *= sztxx;
    bpad *= sztxx;
    hpad *= sztxx;

    /*
     * Convert point in element to device...
    */
    np = 1;

    gtrans ( sys_M, sys_D, &np, &rx, &ry, &srx, &sry, &ier,
	     strlen(sys_M), strlen(sys_D) );

    /*
     * Add up the rows and columns of text in the element so 
     * we get some idea of the size of the text block...
     * 
     * Note: For midlevel cloud text, we need to count the number
     * of cloud types, codes and thunderstorm types to calculate the
     * the rows of text; for all other text types, we simply count
     * the number of '\n' in the text as the number of rows.
     */         
    if ( sptx == 15 ) { /* midlevel cloud text */
        
	cols = 9;   /* fixed number of columns */
	
	txtarry= (char **) malloc ( mclditem * sizeof(char *) );

	for( ii = 0; ii < mclditem; ii++ )
	    txtarry[ii] = (char *) malloc ( strsiz * sizeof(char) );
	
	cst_clst( txtptr, '|', "", mclditem, strsiz, txtarry,
			   &numcld, &ier );	
        num_mtyp = 0;
        token = strtok ( txtarry[0], ";" );
        while ( token != NULL ) {
	    num_mtyp++;	    
	    token = strtok ( NULL, ";" );
        }

        num_mcod = 0;
        token = strtok ( txtarry[1], ";" );
        while ( token != NULL ) {
	    num_mcod++;	    
	    token = strtok ( NULL, ";" );
        }
    
        num_mtstrm = 0;
        token = strtok ( txtarry[6], ";" );
        while ( token != NULL ) {
	    num_mtstrm++;	    
	    token = strtok ( NULL, ";" );
        }        
    
        /*
	 *  number of rows for cloud types/codes
	 */
	row1 = ( num_mtyp + num_mcod ) / 2;
	rows = (( num_mtyp + num_mcod ) % 2 ) ? ( row1 + 1 ): row1;
	
        /*
	 *  number of rows for turbulance/icing
	 */
	rows += 4;
	
        /*
	 *  number of rows for thunderstorm types
	 */
	row1 = num_mtstrm / 2;
	row2 = ( num_mtstrm % 2 ) ? ( row1 + 1 ) : row1;
	rows += row2;
	
        /*
	 *  number of rows for thunderstorm level
	 */
	if ( strlen( txtarry[7] ) > (size_t)0 )  rows += 2;	
	
	for( ii = 0; ii < mclditem; ii++ )
	    free( txtarry[ii] );
	
        if ( txtarry )  free( (char **) txtarry );
    } else if (sptx == 16 || sptx == 17) {
      cols = 0;
      rows = 0;
      numelem = 0;
      ptr = txtptr;
      while ((ptr = strchr(ptr,'|')) != NULL) {
        ptr++;
        numelem++;
      }
      numelem++;
      txtarry = (char **) malloc (numelem * sizeof(char *) );
      for (ii = 0; ii < numelem; ii++ )
        txtarry[ii] = (char *) malloc ( strsiz * sizeof(char) );
      cst_rspc (txtptr, &ier);
      cst_clst (txtptr, '|', "", numelem, strsiz, txtarry,
                                 &numgfa, &ier );
      cst_rnan (txtptr, txtptr, &ier);
      for (ii = 0; ii < numgfa; ii++) {
	/*
	 * The FOR loop calculates columns and rows of GFA text.
	 * IBDR (border type/fill ) and BGFN (background font) do not
	 * add rows or columns, so they are ignored.
	 */
        if (strstr(txtarry[ii],"SSYM")!=NULL ||
            strstr(txtarry[ii],"WSYM")!=NULL ||
            strstr(txtarry[ii],"TSYM")!=NULL ||
            strstr(txtarry[ii],"ISYM")!=NULL) {
            symcols = 1;
            symrows = 1;
            if (strchr(txtarry[ii],'/') != NULL) {
              token = strtok( txtarry[ii], "/"); /* Symbol Number */
              if (token != NULL) 
                token = strtok (NULL,"/"); /*Text Size */
              if  (token != NULL) {
                token = strtok (NULL,"/"); /*columns */
                symcols = atoi(token);
              }
              if (token != NULL) {
                token = strtok (NULL,"/"); /*rows*/
                symrows = atoi(token);
              }
            }  
            cols = ( symcols > cols) ? symcols: cols;
            rows+=symrows;
        } else if (strstr(txtarry[ii],"IBDR") == NULL &&
                   strstr(txtarry[ii],"BGFN") == NULL) {
            cols = ((int)strlen(txtarry[ii]) > cols) ? (int)strlen(txtarry[ii]):cols;
            rows++;
        } 
      }
      for ( ii = 0; ii < numgfa; ii++)
        free(txtarry[ii]);
      if ( txtarry ) free ( (char **) txtarry );
    } else {  /* all other text types */
        rows = 1;
        cols = 0;
        longest = cols;
        quit = 0;
        ii  = 0;
        /* an extra row for the letter L or H */
        if (sptx == 1 || sptx == 2) {
                rows++;
        }
        /* characters for the leading info */
        if (sptx == 6) {
            cols = 4;
        }
        while ((quit == 0) && (ii < MAX_TEXT)) {
            if (txtptr[ii] == '\n' || (wrapslash && txtptr[ii] == '/')) {
	        if (cols > longest) longest = cols;
	        rows++;
	        cols = 0;
	    }
	    else {
	        if (txtptr[ii] == '\0') {
                    /* the display of the str trims any trailing spaces/tabs */
                    jj = ii - 1;
                    while ((txtptr[jj] == ' ' || txtptr[jj] == '\t')
                        && jj != 0) {
                        jj--;
                        cols--;
                    }
		    quit = 1;
		    if (cols < longest) cols = longest;
	        }
	        else {
	            cols++;
	        }
	    }
            ii++;    
        }
        if (!cols) cols = 1;
    }

    /*
     * Compute the width, height and offsets for the text element.
     */
    twidth  = ((float)cols * sztxx * hfnt);
    elwidth  = twidth + hpad * 2.0;
    theight = ((float)rows * sztxx * vfnt);
    elheight = theight + tpad + bpad;

    /*
     *  calculate offset                           
     */
    xoff =  (((float)ioffx)/2.0F) * sztxx;
    yoff =  (((float)ioffy)/2.0F) * sztxx;

    /*
     * Compute the edges of the text area using the alignment value.
     * if there is a box around the text, include its padding for
     * the alignment offset
     */
    if (sptx == 0 || (sptx > 6 && sptx < 10) || sptx == 12) {
        apad = 0.50 * sztxx * hfnt;
    } else {
        apad = hpad + (0.50 * sztxx * hfnt);
    }
    switch (jalign) {
      case 1:		/* left justified */
	aleft = xoff - apad;
	arght = xoff + elwidth - apad;
	break;

      case 2:		/* centered */
	aleft = xoff - (elwidth/2.0F);
	arght = xoff + (elwidth/2.0F);
	break;

      case 3:		/* right justified */
	aleft = xoff - elwidth + apad;
	arght = xoff + apad;
	break;
    }

    abot = -(theight/2.0F) + yoff - bpad;
    atop =   theight/2.0F  + yoff + tpad;


    if (irrotn == 2) {       			/* north relative */
        np = 1;
	srotn = 0.0F;
	gp_azdr (&srotn, &rx, &ry, &nrotn, &ier );
	radrotn = (double)(rotn - nrotn) * DTR;
    } else {    
        radrotn = (double)rotn * DTR;
    }      

    bcos = (float) cos (radrotn);
    bsin = (float) sin (radrotn);
    
    if (boxflag == 0){
	box_x[0] = aleft;
	box_x[1] = aleft;
	box_x[2] = arght;
	box_x[3] = arght;

	box_y[0] = abot;
	box_y[1] = atop;
	box_y[2] = atop;
	box_y[3] = abot;

    }
    else if (boxflag == 2){
	if (irrotn){   /* rotated */
	    box_x[0] = ((aleft * bcos) - (abot * bsin));
	    box_x[1] = ((aleft * bcos) - (atop * bsin));
	    box_x[2] = ((arght * bcos) - (atop * bsin));
	    box_x[3] = ((arght * bcos) - (abot * bsin));

	    box_y[0] = ((aleft * bsin) + (abot * bcos));
	    box_y[1] = ((aleft * bsin) + (atop * bcos));
	    box_y[2] = ((arght * bsin) + (atop * bcos));
	    box_y[3] = ((arght * bsin) + (abot * bcos));
        } else  {  /* non-rotated */
	    box_x[0] = aleft;
	    box_x[1] = aleft;
	    box_x[2] = arght;
	    box_x[3] = arght;

	    box_y[0] = abot;
	    box_y[1] = atop;
	    box_y[2] = atop;
	    box_y[3] = abot;
        }

    }
    else if (boxflag == 1) {  /* use bigger area for refresh */

        if (irrotn) {
	    rot_x[0] = srx  + ((aleft * bcos) - (abot * bsin));
	    rot_x[1] = srx  + ((aleft * bcos) - (atop * bsin));
	    rot_x[2] = srx  + ((arght * bcos) - (atop * bsin));
	    rot_x[3] = srx  + ((arght * bcos) - (abot * bsin));

	    rot_y[0] = sry  - ((aleft * bsin) + (abot * bcos));
	    rot_y[1] = sry  - ((aleft * bsin) + (atop * bcos));
	    rot_y[2] = sry  - ((arght * bsin) + (atop * bcos));
	    rot_y[3] = sry  - ((arght * bsin) + (abot * bcos));

	    /*
	     *  determine max/min values and load box_ arrays
	     */ 
	    xmin = xmax = rot_x[0];
 	    ymin = ymax = rot_y[0];

	    for (ii=1; ii < 4; ii++) {
	        if (rot_x[ii] < xmin)
	            xmin = rot_x[ii];
	        if (rot_x[ii] > xmax)
	            xmax = rot_x[ii];
	        if (rot_y[ii] < ymin)
	            ymin = rot_y[ii];
	        if (rot_y[ii] > ymax)
	            ymax = rot_y[ii];
	    }

	}
        else {			/* non-rotated text */
            xmin = srx + aleft;
            ymin = sry + abot;
            xmax = srx + arght;
            ymax = sry + atop; 
        }

        hboxsz = (G_MAX ( elwidth, elheight ))/2.0F;

        box_x[0] = xmin - hboxsz;
        box_y[0] = ymin - hboxsz;

        box_x[1] = xmax + hboxsz;
        box_y[1] = ymin - hboxsz;

        box_x[2] = xmax + hboxsz;
        box_y[2] = ymax + hboxsz;

        box_x[3] = xmin - hboxsz;
        box_y[3] = ymax + hboxsz;

        /*
	 *  verify the box for any offset text includes the x,y point
	 */
	if (ioffx || ioffy) {
	    npts = 4;
	    np   = 1;
	    cgr_inpoly ( sys_D, &np, &srx, &sry, sys_D, &npts, box_x, box_y, 
			 &in_box, &ier );

	    /*
	     *  If rx, ry is not in the box, increase the box size to
	     *  include it
	     */
	    if (!in_box) {
	        if ( fabs((double)(box_x[0] - srx)) < fabs((double)(box_x[1] - srx))) {
		    /* 
		     *  decrease xmin value
		     */
		    box_x[0] -= (float)(fabs((double)(box_x[0] - srx)) + 10.0);
		    box_x[3] -= (float)(fabs((double)(box_x[3] - srx)) + 10.0);
		}
		else {
		    /* 
		     *  increase xmax value
		     */
		    box_x[1] += (float)(fabs((double)(box_x[1] - srx)) + 10.0);
		    box_x[2] += (float)(fabs((double)(box_x[1] - srx)) + 10.0);
		}

		if (fabs((double)(box_y[0] - sry)) < fabs((double)(box_y[2] - sry))) {
		    /* 
		     *  decrease ymin value
		     */
		    box_y[0] -= (float)(fabs((double)(box_y[0] - sry)) + 10.0);
		    box_y[1] -= (float)(fabs((double)(box_y[1] - sry)) + 10.0);
		}
		else {
		    /* 
		     *  increase ymax value
		     */
		    box_y[2] += (float)(fabs((double)(box_y[2] - sry)) + 10.0);
		    box_y[3] += (float)(fabs((double)(box_y[3] - sry)) + 10.0);
		}

	    }
	}
    }
  
}
