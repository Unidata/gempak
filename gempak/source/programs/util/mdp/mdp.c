#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"

/************************************************************************
 * mdp.c								*
 *									*
 * CONTENTS:								*
 * mdp									*
 ***********************************************************************/
 

int main ( int argc, char **argv )
/************************************************************************
 * mdp									*
 *                                                                      *
 * This program generates the mesoscale discussion latlon pairings.	*
 * Pairings will be written to a file whose filename is based on the	*
 * input filename (filename extension, if exists, is replaced w/ "mdp").*
 *                                                                      *
 * Example:								*
 * mdp input_vgf_file.vgf						*
 * Produces latlon pairings in the ASCII file				*
 * output_vgf_file.mdp							*
 *                                                                      *
 * main(argc, argv)                                                     *
 *                                                                      *
 * Input parameters:                                                    *
 *  argc   int      number of parameters of command line                *
 *  argv   char**   parameter array of command line                     *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 6/02						*
 * D.W.Plummer/NCEP	 8/02	Bug fix for lons > 99.995 and < 100.0	*
 * m.gamazaychikov/SAIC  3/03	Made mdp work only on scallop lines	*
 *				added WFOs and STATES to output text	*
 * m.gamazaychikov/SAIC  3/03	Made mdp work on lines grouped or not 	*
 *				grouped with WFOs and/or STATES		*
 *				Made MDP exit upon encountering first	*
 *				scallop line in VGF file		*
 * 				Made changes to format of output text	*
 * T. Piper/SAIC	12/05	Updated cst_wrap for CSC		*
 * J. Wu/SAIC		04/06	Added parameter in cst_wrap 		*
 ***********************************************************************/
{
int    	ii, ix, iy, pagflg, ier;
int	ilen, npts;
char	vg_class, vg_type;
char    blank[2]={' '}, errgrp[8], infile[128], ifname[128], outfile[128];
float	x, y;
long	ifilesize;
int	ne1, more1, curpos1;
char	buffer[1024], str[20], *cptr;

int     wrtoutfg, wrtwfofg, wrtsttfg;
int     grpnumbr1, grpnumbr2;
int     lintyp, jj, narea, areatype;
int	ne2, more2, curpos2;
char    strwfo[480], strstt[480], bufferlat[1024];

VG_DBStruct     el;

FILE    *ifptr, *ofptr;
/*---------------------------------------------------------------------*/

    /*
     *  Check if number of input arguments is correct.
     */
    if ( argc < 2 )  {
	pagflg = G_FALSE;
	strcpy ( errgrp, "MDP" );
        ip_help ( errgrp, &pagflg, &ier,
                  strlen(errgrp) );
	exit (0);
    }

    /*
     *  First input on command line is input vgf file name.
     */
    strcpy ( infile, argv[1] );
    cfl_inqr ( infile, NULL, &ifilesize, ifname, &ier );
    ifptr = (FILE *) cfl_ropn(ifname, "", &ier);
    if ( ier != 0 )  {
	printf("Error opening VGF file %s\n", infile );
	exit (0);
    }

    /*
     *  Output filename is input filename w/ "mdp" filename extension.
     */
    strcpy ( outfile, infile );
    cptr = strrchr( outfile, '.' );
    if ( cptr != (char *)NULL )  {
	cptr[0] = '\0';
    }
    strcat( outfile, ".mdp" );

    /*
     *  Loop through all the elements until a line is found.
     */
    ne1 = 0;
    more1 = G_TRUE;
    curpos1 = 0;
    buffer[0] = '\0';
    bufferlat[0] = '\0';
    while ( ne1 < MAX_EDITABLE_ELEMS && more1 == G_TRUE )  {

        wrtoutfg = G_FALSE;
        wrtsttfg = G_FALSE;
        wrtwfofg = G_FALSE;

	cvg_rdrecnoc( ifname, ifptr, curpos1, &el, &ier );

	if ( ier < 0 )  {
	    more1 = G_FALSE;
	}
	else  {


	    curpos1 += el.hdr.recsz;
	    vg_class = el.hdr.vg_class;
	    vg_type  = el.hdr.vg_type;
	    grpnumbr1 = el.hdr.grpnum;

            if ( (int)vg_class == CLASS_LINES )  {
               /*
                *  Open output file.
                */
                ofptr = (FILE *)cfl_wopn ( outfile, &ier );
                if ( ier != 0 )  {
                    printf("Error opening/creating output file %s\n", outfile );
                    exit (0);
                }
                if ((int)vg_type == SPLN_ELM ) {
                   /*
                    * type of special line
                    */
                    lintyp   = el.elem.spl.info.spltyp;
                    if (lintyp == 3 ) {
                       /*
                        * number of points in a line
                        */
		        npts    = el.elem.spl.info.numpts;
		       /*
		        *  Format lats and lons into buffer.
		        */
		        for ( ii = 0; ii < npts; ii++ )  {
		            x = el.elem.spl.latlon[ii];
		            y = -el.elem.spl.latlon[ii+npts];
		           /*
		            *  Make sure lats and lons are rounded to 100ths of deg.
		            */
		            x = ((int)(x*100)) / 100.0F;
		            y = ((int)(y*100)) / 100.0F;
		            if ( y >= 100.0F )  y -= 100.0F;
		            ix = G_NINT(x*100.0F);
		            iy = G_NINT(y*100.0F);

	         	    sprintf( str, "%04d%04d ", ix, iy );
		            strcat ( bufferlat, str );
                            /*
                             * this flag is here to ensure exit as soon
                             * as a line is found
                             */
	                     more1 = G_FALSE;
                            /*
                             * this flag is here to ensure that lat-lon
                             * pairs are written out even without wfo and states 
                             * grouped with it
                             */
                             wrtoutfg = G_TRUE;
                             
		        }

                        ne2 = 0;
                        curpos2 = 0;
	                more2 = G_TRUE;
                        while ( ne2 < MAX_EDITABLE_ELEMS && more2 == G_TRUE )  {
                           cvg_rdrecnoc( ifname, ifptr, curpos2, &el, &ier );
                         if ( ier < 0 )  {
	                  more2 = G_FALSE;
                         }
	                 else  {
                           curpos2 += el.hdr.recsz;
                           vg_class = el.hdr.vg_class;
                           vg_type  = el.hdr.vg_type;
	                   grpnumbr2 = el.hdr.grpnum;
                           if (grpnumbr2 == grpnumbr1 && grpnumbr2 != 0) {
                            if ((int)vg_type == LIST_ELM ) {
                              areatype = 0;
                              areatype = el.elem.lst.info.subtyp;
                              if (areatype == 3) {
                                 wrtwfofg = G_TRUE;
                                 narea = el.elem.lst.data.nitems;
                                 strwfo[0] = '\0';
                                 strcat( strwfo, "ATTN...WFO...");
                                 jj = 0;
                                 for (jj = 0; jj < narea; jj++) {
                                   strcpy (str,    el.elem.lst.data.item[jj]);
                                   strcat (strwfo, str);
                                   strcat (strwfo, "..." );
                                   if ((jj+1)%9 == 0) {
                                     strcat (strwfo, "\n");
                                   }
                                 }
                                 strcat (strwfo, "\n");
                              }
                              if (areatype == 4) {
                                 wrtsttfg = G_TRUE;
                                 narea = el.elem.lst.data.nitems;
                                 strstt[0] = '\0';
                                 jj = 0;
                                 for (jj = 0; jj < narea; jj++) {
                                   strcpy (str,    el.elem.lst.data.item[jj]);
                                   strcat (strstt, str);
                                   strcat (strstt, "Z000-" );
                                 }
                                 strcat (strstt, "\n");
                              }
                            }
		           } 
                         }
	                 ne2++;
                        }
		    } 
		} 
            }
        }
	ne1++;
           /*
            *  Write to output file.
            */
        if (wrtoutfg == G_TRUE) {
          /*
           *  Wrap bufferlat such that only 6 pairs of lat,lons on one line
           */
            ilen = 55;
            cst_wrap( bufferlat, blank, &ilen, "\n", (char *)NULL, bufferlat, &ier );

            if (wrtsttfg == G_TRUE) {
              /*
               *  Wrap strstt such that only 7 STATE IDs on one line
               *  and dump strstt into buffer for further output
               */
              ilen = 49;
              cst_wrap( strstt, blank, &ilen, "\n", (char *)NULL, strstt, &ier );
              strcat ( buffer, strstt );
              strcat ( buffer, "\n");
            }
            if (wrtwfofg == G_TRUE) {
              /*
               *  Wrap strwfo such that only 9 WFOs on one line
               *  and dump strswo into buffer for further output
               */
               ilen = 67;
               cst_wrap( strwfo, blank, &ilen, "\n", (char *)NULL, strwfo, &ier );
               strcat ( buffer, strwfo );
               strcat ( buffer, "\n");
            }

            strcat ( buffer, bufferlat);
            strcat ( buffer, "\n");
            cfl_writ( ofptr, (int)strlen(buffer), (unsigned char *)buffer, &ier );
            bufferlat[0] = '\0';
        }
    }
    /*
     *  If no line is found, close files and exit.
     */
    cfl_clos ( ifptr, &ier );
    cfl_clos ( ofptr, &ier );
    return(0);

}
