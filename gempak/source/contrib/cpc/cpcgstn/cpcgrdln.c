#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"

#define	SPLTYP 	11


void cpcg_rdln ( char *fname, int *nlin, int npts[], int lcolr[], 
		 int close[], float llat[][MAXPTS], 
		 float llon[][MAXPTS], int *iret )
/************************************************************************
 * cpcg_rdln                                                            *
 *                                                                      *
 * This function reads a line from vg file and returns the lat/lon of   * 
 * points in the line.							*
 *                                                                      *
 * cpcg_rdln (fname, nlin, npts, lcolr, close, llat, llon, iret)	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *fname         		char	vg file name of the line	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *nlin			int     number of lines in the file	*
 *      npts[nlin]              int     number of points in a line	*
 *	lcolr[nlin]		int	line colors			*
 *	close[nlin]		int	closed flag  of the line	*
 *      llat[nlin][MAXPTS]      float   latitude of line		*
 *      llon[nlin][MAXPTS]      float   longitude of line		*
 *      *iret                   int     Return code                     *
 *                                      -2 = no lines found            	*
 *                                                                      *       
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           07/01                                           *
 * M. Li/SAIC		09/01	Added CV_PRMT				*
 ***********************************************************************/
{
int    	flag, nline, wrtflg, np, ier, ierr, nout, istrt, iend, mxpts;
float	dens, crvscl;
float	newlat[MAXPTS], newlon[MAXPTS];
char	vg_class, vg_type;
char	onfname[LLMXLN];
long	filesize;
int	more, curpos;

VG_DBStruct     el;

FILE    *fptr;
/*---------------------------------------------------------------------*/
      *iret = 0;
      istrt = 0;
      crvscl = 30.0;
      mxpts  = MAXPTS;
      dens   = 0;

      cfl_inqr ( fname, NULL, &filesize, onfname, &ier );
      wrtflg = 0;
      cvg_open ( onfname, wrtflg, &(fptr), &ier );
      if ( ier != 0 )  {
  	  er_wmsg ( "CPCG", &ierr, " ", &ier, 4, 1);
   	  *iret = -1;
	  return;
      }

      
      /*
       *  Scan the file reading the special line 
       */
      nline = 0;
      *nlin = 0;
      curpos = 0;
      more = G_TRUE;
      while ( ( more ) && ( curpos < filesize ) )  {
	
	cvg_rdhdr ( fname, fptr, curpos, (int)filesize, &el, &flag, &ier );

	if ( ier < 0 )  {
	    more = G_FALSE;
	}
	else if ( el.hdr.recsz > 0  && !el.hdr.delete ) { 

	    vg_class = el.hdr.vg_class;
	    vg_type  = el.hdr.vg_type;

	    if ( (int)vg_class == CLASS_LINES && vg_type == SPLN_ELM ) {
	        cvg_rdrec( fname, curpos, &el, &ier );

	        if (el.elem.spl.info.spltyp == SPLTYP) {
		    npts[nline]  = el.elem.spl.info.numpts;
		    lcolr[nline] = el.hdr.maj_col;
		    close[nline] = el.hdr.closed;
 
		    for (np = 0; np < npts[nline]; np++) {
		        llat[nline][np] = el.elem.spl.latlon[np]; 
		        llon[nline][np] = el.elem.spl.latlon[np+npts[nline]]; 
		    }

		    /*
		     * Parametric curve fitting
		     */
		    if (el.hdr.smooth == 0) {
			dens = 0.0;
		    } else if (el.hdr.smooth == 1) {
			dens = 1.0;
		    } else if (el.hdr.smooth == 2) {
			dens = 5.0;
		    } 

		    if (dens > 0) {
		       iend = npts[nline];
		       cv_prmt (&(npts[nline]), llat[nline], llon[nline], &dens, 
			   &mxpts, &crvscl, &istrt, &iend, &nout, newlat, newlon, &ier);

		       npts[nline] = nout;	
		       for (np = 0; np < npts[nline]; np++) {
                          llat[nline][np] = newlat[np];
                          llon[nline][np] = newlon[np];
                       }
		    }
 
	 	    nline++;     
		}

	     }   /*   if ( (int)vg_class == CLASS_LINES )  */
	}        /*  if ( ier < 0 )                        */ 
	
      curpos += el.hdr.recsz;
      
      }    /*   end of while */ 
	
      *nlin = nline;
      if (nline <=  0) *iret = -2;
}

/*=====================================================================*/
