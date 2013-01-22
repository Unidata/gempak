#include "xwcmn.h"
#include "app.h"	/* Resrcs */
#include "color.h"

void colr_rtbl ( int ncolors, Pixel color_pixels[] );

/************************************************************************
 * colr.c                                                               *
 *                                                                      *
 * This module initialize the color map.                        	*
 *                                                                      *
 * CONTENTS:                                                            *
 *      colr_init()      initialize colors   				*
 *      colr_rtbl()      read graphic color table   			*
 ***********************************************************************/

/*=====================================================================*/

int colr_init ( int nbank, int banks[], Boolean verbose )
/************************************************************************
 * colr_init                                                        	*
 *                                                                      *
 * This function initialize the color map.    				*
 *                                                                      *
 * int colr_init( nbank, banks, verbose)                            	*
 *                                                                      *
 * Input parameters:                                                    *
 *      nbank   int      number of color banks    			*
 *      banks[] int      color indecies into color map for each bank    *
 *      verbose Boolean  verbose mode    				*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *       int           0 = success, -1 = error                          *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * Chien Lin/EAI    10/92                                       	*
 * Chien Lin/EAI     8/93                                  		*
 * C. Lin/EAI       12/95    clean up the header                        *
 * C. Lin/EAI        5/96    set the graphic back ground color black    *
 * C. Lin/EAI       12/96   set the graphic background based on resource*
 * C. Lin/EAI       11/97   rename from colrMapInit, 			*
 *			    replace NxmLoadColorTable with colr_rtbl	*
 * I. Durham/GSC     5/98   changed call for underscore			*
 * T. Piper/SAIC	2/02	freed ret_banks				*
 * A. Person/Penn State 06/02						*
 *			    Updated to support 16- and 24-bit graphics	*
 * S. Chiswell/Unidata	9/02	Fixed bgcolr specification for 16/24	*
 * T. Piper/SAIC	01/04	Modified for changes to colr_rtbl	*
 * T. Piper/SAIC	07/04	Added <type>Cid indexing		*
 * S. Jacobs/NCEP	11/12	Return if depth is not 8 bit		*
 ***********************************************************************/

{
int             ii, jj, ncolors, ret, nalloced, *ret_banks;
Pixel		pxls[MAXCOLORS];
float           pix_scale;
XColor          colors[MAX_TOTAL_COLOR], bgcolr, ignclr, xcolr;
char            rgb_flag;
int		xdpth;
unsigned char   sendcolr[MAXCOLORDATA];
unsigned int	icval;
int		nbyte, numb;

/*---------------------------------------------------------------------*/

	if ( verbose ) {
	    printf("nbank -- %d\n", nbank);
	    for ( ii = 0; ii < nbank; ii++)
		printf("banks[%d] = %d\n", ii, banks[ii]);
	}
	xdpth = DefaultDepth((XtPointer)gemdisplay,DefaultScreen((XtPointer)gemdisplay));

	/* Fix a problem with 16,24-bit color sharing */
	if ( xdpth != 8 ) return;

	ret_banks = (int *)malloc((size_t)nbank*sizeof(int));
        xcamgr(gemdisplay, gemmap, nbank, banks, ret_banks, &ret);

        if ( ret != G_NORMAL ) {
	    ncolors = nalloced = 0;
	    for ( ii = 0; ii < nbank; ii++) {
        	ncolors  += banks[ii];
            	nalloced += ret_banks[ii];
	    }
            printf("\t Request total # of colors = %d\n", ncolors);
	    if ( xdpth == 8 ) {
               while ( 1 )  {
                   if ( XAllocColorCells(gemdisplay, gemmap, False, 
						NULL, 0, pxls, ncolors) )
                        break;
                   ncolors --;
               }
               printf("\t Current available system colors = %d\n", 
							ncolors+nalloced);
	    }
            exit(1);
	}
	free(ret_banks);

        /*
         * initialize graphic colors
         */
        ncolors = banks[GraphCid];

	colr_rtbl( ncolors, (Pixel*)ColorBanks.colrs[0]);

	/*
	 * set the application graphic background color, since colr_rtbl
	 * fills in 1 to ncolor.
	 */
	rgb_flag = 0;
	bgcolr.pixel  = ColorBanks.colrs[0][0];
        bgcolr.flags  = DoRed|DoGreen|DoBlue;
	if ( Resrcs.appbgName != NULL ) {
	    if (XLookupColor(gemdisplay, gemmap, Resrcs.appbgName, 
				&ignclr, &xcolr)) {
		bgcolr.red   = xcolr.red;
		bgcolr.green = xcolr.green;
		bgcolr.blue  = xcolr.blue;
	    }
	    else
		rgb_flag = 1;
	}
	else 
		rgb_flag = 1;

	if ( rgb_flag ) {
		bgcolr.red   = (unsigned short)Resrcs.appbgRed;
		bgcolr.green = (unsigned short)Resrcs.appbgGreen;
		bgcolr.blue  = (unsigned short)Resrcs.appbgBlue;
	}
	if( xdpth == 8 ) 
		XStoreColor(gemdisplay, gemmap, &bgcolr);
	else {
		if( !XAllocColor( gemdisplay, gemmap, &bgcolr ) ) 
                      printf("colr:  Error calling XAllocColor!\n");
                else /* Save the color in ColorBanks.colrs */
                      ColorBanks.colrs[0][0] = bgcolr.pixel;
	}

	/* Save the color definitions for 16- and 24-bit cases to the X server */
	if( xdpth != 8 ) {
	    nbyte = 0; numb = 4;
	    icval = nbank;
	    mv_itob ( (int *)(&icval), &nbyte, &numb, sendcolr, &ret);
            nbyte += numb;

	    for ( ii = 0; ii < nbank; ii++) {
		icval = banks[ii];
	        mv_itob ( (int *)(&banks[ii]), &nbyte, &numb,
			  sendcolr, &ret);
                nbyte += numb;
	    }
	    for ( ii = 0; ii < nbank; ii++) {
	        if( banks[ii] > 0 ) {
	            for (jj = 0; jj < banks[ii]; jj++) {
			icval = (unsigned int)ColorBanks.colrs[ii][jj];
			if ( verbose ) {
			    printf("look val %d %d    %d %d %d %d [ test %d]\n",ii,jj,
			       icval % 256, (icval >> 8 ) & 255,
			       (icval >> 16) & 255, (icval >> 24) & 255,
			       ( icval >> 24 ) );
			}
	                mv_itob ( (int *)(&icval), &nbyte, &numb,
				  sendcolr, &ret);
                        nbyte += numb;
	            }
	        }
	    }

            xcsdat(gemdisplay, ShareColorData, (char *)sendcolr, nbyte);
        }


	if ( verbose ) {
            for ( ii = 0; ii < ncolors; ii++ )
                printf("pixels[%d] -- %ld\n", ii, 
				ColorBanks.colrs[0][ii]);
	}


        /*
         * initialize satellite colors
         * need to check color #
         */
        ncolors = banks[SatCid];

	if ( verbose ) 
            for ( ii = 0; ii < ncolors; ii++ )
                printf("IMGpixels[%d] -- %ld\n", ii, 
			 ColorBanks.colrs[1][ii]);

	if ( ncolors > 1 )
            pix_scale = 65535.0F/(float)(ncolors - 1);
	else
            pix_scale = 65535.0F;

        for(ii = 0; ii < ncolors; ii++) {
                colors[ii].pixel = ColorBanks.colrs[1][ii];
                colors[ii].red   = (unsigned short)((float)ii*pix_scale);
                colors[ii].green = (unsigned short)((float)ii*pix_scale);
                colors[ii].blue  = (unsigned short)((float)ii*pix_scale);
                colors[ii].flags = DoRed|DoGreen|DoBlue;
        }

        if( xdpth == 8 ) XStoreColors(gemdisplay,gemmap,colors,ncolors);

        /*
         * initialize radar colors
         * need to check color #
         */
        ncolors = banks[RadCid];
	if ( verbose ) 
            for ( ii = 0; ii < ncolors; ii++ )
                printf("RADpixels[%d] -- %ld\n", ii, 
			ColorBanks.colrs[2][ii]);

	if ( ncolors > 1 )
            pix_scale = 65535.0F/(float)(ncolors - 1);
	else
            pix_scale = 65535.0F;
        for(ii = 0; ii < ncolors; ii++) {
                colors[ii].pixel = ColorBanks.colrs[2][ii];
                colors[ii].red   = (unsigned short)((float)ii*pix_scale);
                colors[ii].green = (unsigned short)((float)ii*pix_scale);
                colors[ii].blue  = (unsigned short)((float)ii*pix_scale);
                colors[ii].flags = DoRed|DoGreen|DoBlue;
        }
        if( xdpth == 8 ) XStoreColors(gemdisplay,gemmap,colors,ncolors);
	
	return(0);

}

/*=====================================================================*/

void colr_rtbl ( int ncolors, Pixel color_pixels[] )
/************************************************************************
 * colr_rtbl                                                    	*
 *                                                                      *
 * This function reads a graphic color table file consisting of the RGB *
 * definitions of each color pixel and changes the corresponding colors.*
 * The value of each RGB component in the file is between 0 - 255.  	*
 *                                                                      *
 * void colr_rtbl( ncolors, color_pixels )          			*
 *                                                                      *
 * Input parameters:                                                    *
 *  ncolors      int     number of colors                               *
 *  color_pixels Pixel*  index array of color pixels                    *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       11/97	based on NxmLoadColorTable              *
 * T. Piper/SAIC	01/04	replaced fopen with cfl_tbop		*
 * T. Piper/SAIC	01/04	removed filename parameter		*
 * T. Piper/SAIC	07/04	Fixed calculation of colors. w/COLR_SCAL*
 ***********************************************************************/
{
FILE    *fp;
char    buff[128], dummy[20];
int     ired[GRAPH_COLORS], igreen[GRAPH_COLORS], iblue[GRAPH_COLORS];
float   ratio;
XColor  colors[MAXCOLORS];
int	ii, ier, xdpth;
/*---------------------------------------------------------------------*/

	xdpth = DefaultDepth((XtPointer)gemdisplay,
		DefaultScreen((XtPointer)gemdisplay));

        fp = cfl_tbop(CLR_TBL, CLR_DIR, &ier);
        if ( fp == NULL  ||  ier != 0 ) {
	    printf("Warning: Cannot find graphic color table %s.\n",
		    CLR_TBL );

	    if (ncolors > 1 ) {
              for (ii = 1; ii < ncolors; ii++){
		ratio = (float)ii/(float)(ncolors-1);
                colors[ii-1].red   = (unsigned short)(ratio * 65535.0F);
                colors[ii-1].green = (unsigned short)(ratio * 65535.0F);
                colors[ii-1].blue  = (unsigned short)(ratio * 65535.0F);
                colors[ii-1].flags = DoRed | DoGreen | DoBlue;
		if( xdpth == 8 ) {
                    colors[ii-1].pixel = color_pixels[ii];
		}
		else {
                    if( !XAllocColor( gemdisplay, gemmap, &colors[ii-1] ) ) {
                        printf("colr:  Error calling XAllocColor!\n");
                        fflush(stdout);
                    }   
                    else {
                        /* Save the color in ColorBanks.colrs */
                        color_pixels[ii]=colors[ii-1].pixel;
                    }   
		}
              }
	    }

	} 
	else {

            ii = 0;
            while ( !feof(fp) ) {
                cfl_trln(fp, sizeof(buff), buff, &ier);
                if ( ier == 0 ) {
                    sscanf(buff, "%s %s %d %d %d %s",
                           dummy, dummy, &ired[ii],
                           &igreen[ii], &iblue[ii], dummy);
                    ii++;
		}
	    }
	    ncolors = ii--;
            if ( ncolors > GRAPH_COLORS ) {
                ncolors = GRAPH_COLORS;
            }

            for (ii = 1; ii < ncolors; ii++) {

                colors[ii-1].red   = (unsigned short)(((ired[ii]+1)*COLR_SCAL)-1);
                colors[ii-1].green = (unsigned short)(((igreen[ii]+1)*COLR_SCAL)-1);
                colors[ii-1].blue  = (unsigned short)(((iblue[ii]+1)*COLR_SCAL)-1);
                colors[ii-1].flags = DoRed | DoGreen | DoBlue;
		if( xdpth == 8 ) {
                    colors[ii-1].pixel = color_pixels[ii];
		}
		else {
		    if( !XAllocColor( gemdisplay, gemmap, &colors[ii-1] ) ) {
		        printf("colr:  Error calling XAllocColor!\n");
		        fflush(stdout);
		    }
		    else {
		        /* Save the color in ColorBanks.colrs */
		        color_pixels[ii]=colors[ii-1].pixel;
		    }
		}
            }

            fclose(fp);
	}

	if( xdpth == 8 ) XStoreColors(gemdisplay, gemmap, colors, ncolors - 1);

}

/*=====================================================================*/

