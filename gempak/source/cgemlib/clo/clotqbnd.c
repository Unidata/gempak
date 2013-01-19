#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern  CLO_t           clo;

#define	DEFAULT_INFO	"-"

void clo_tqbnd ( char *name, float plat, float plon, char *strout, int *iret )
/************************************************************************
 * clo_tqbnd								*
 *									*
 * This function determines which bound contains the point		*
 * (plat,plon), if any.  If (plat,plon) is not contained within any	*
 * bound or bound part, then DEFAULT_INFO is returned as the 		*
 * bound information.							*
 *									*
 * clo_tqbnd ( name, plat, plon, strout, iret )				*
 *									*
 * Input parameters:							*
 *	*name		char		Name of bound type		*
 *	plat		float		Latitude			*
 *	plon		float		Longitude			*
 *									*
 * Output parameters:							*
 *	*strout		char		String w/ bound information	*
 *	*iret		int		Return code			*
 *					= 0 - normal			*
 *					= -1 - unable to open cnty tbl	*
 *					= +3 - not found in any poly	*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 7/00	Created					*
 * D.W.Plummer/NCEP	 8/00	Bug fix in performance check		*
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * D.W.Plummer/NCEP	 6/02	Remove performance enhancing code	*
 * m.gamazaychikov/SAIC	06/05	Changed the polygon inclusion selection	*
 * D.W.Plummer/NCEP	06/05	Mods to process holes			*
 * D.W.Plummer/NCEP	09/06	Remove limit on number of possibilities	*
 * S. Jacobs/NCEP	 6/11	Change default and success return code	*
 ***********************************************************************/
{
int	ii, jj, which, ier;
long	strec;

int	one=1, inout, nposs, npts;
int	*pbnd, *pprt;
float	lat1, lon1, lat2, lon2, mx1lon, mn1lon, mx1lat, mn1lat;
char	buff[128], *ptr;

char	fnm[FILE_FULLSZ];
FILE	*fp;

BInfo_t *tb;

/*---------------------------------------------------------------------*/
    *iret = 3;

    strcpy ( strout, DEFAULT_INFO );

    which = clo_which ( name );

    nposs = 0;

    /*
     *  Sort bounds by their maximum longitude from east-to-west.
     */
    clo_sortbnd ( name, BND_MXLON, &ier );

    ii = 0;
    while ( ii < clo.loc[which].bnd.nbnd  &&  
	    clo.loc[which].bnd.bound[ii].maxlon >= plon )  {

      if ( clo.loc[which].bnd.bound[ii].minlon <= plon  &&
	   clo.loc[which].bnd.bound[ii].minlat <= plat  &&
	   clo.loc[which].bnd.bound[ii].maxlat >= plat )  {

	/*
	 *  The point (plat,plon) may be in this bound.
	 */
	if ( clo.loc[which].bnd.bound[ii].nparts == 1 )  {
	    /*
	     *  If this bound has only one part,
	     *  save off the pointer information.
	     */
	    if ( nposs == 0 )  {
	        G_MALLOC ( pbnd, int, one, "Error re-allocating pbnd" );
	        G_MALLOC ( pprt, int, one, "Error re-allocating pprt" );
	    }
	    else  {
	        G_REALLOC ( pbnd, int, (nposs+1), "Error re-allocating pbnd" );
	        G_REALLOC ( pprt, int, (nposs+1), "Error re-allocating pprt" );
	    }
	    pbnd[nposs] = ii;
	    pprt[nposs] = 0;
	    nposs++;
	}
	else  {
	    /*
	     *  Bound has multiple parts; check each part individually.
	     */
	    for ( jj = 0; jj < clo.loc[which].bnd.bound[ii].nparts; jj++ )  {

	        if ( clo.loc[which].bnd.bound[ii].bndspt[jj].minlon <= plon && 
		     clo.loc[which].bnd.bound[ii].bndspt[jj].minlat <= plat &&
		     clo.loc[which].bnd.bound[ii].bndspt[jj].maxlon >= plon && 
		     clo.loc[which].bnd.bound[ii].bndspt[jj].maxlat >= plat ) {
	    	  /*
	     	   *  The point (plat,plon) may be in this bound's part.
	     	   */
	    if ( nposs == 0 )  {
	        G_MALLOC ( pbnd, int, one, "Error re-allocating pbnd" );
	        G_MALLOC ( pprt, int, one, "Error re-allocating pprt" );
	    }
	    else  {
	        G_REALLOC ( pbnd, int, (nposs+1), "Error re-allocating pbnd" );
	        G_REALLOC ( pprt, int, (nposs+1), "Error re-allocating pprt" );
	    }
	          pbnd[nposs] = ii;
	          pprt[nposs] = jj;
	          nposs++;
		}

	    }

	}

      }

      ii++;

    }

    /*
     *  Check each possible bound part for (plat,plon) inside; there
     *  are probably no more than one or two, occasionally 3 or 4 parts.
     *
     *
     *  Open boundaries file.
     */
    strcpy(fnm, clo.loc[which].bnd.filename);
    fp = (FILE *)cfl_tbop(fnm, "bounds", &ier);
    if ( fp == NULL  ||  ier != 0 )  {
        *iret = -1;
    }

    else  {

        nhot = 0;
        for ( ii = 0; ii < nposs; ii++ )  {

	    /*
	     *  Get starting record and read the poly from that location.
	     */
	    strec = clo.loc[which].bnd.bound[pbnd[ii]].bndspt[pprt[ii]].strec;

	    cfl_seek ( fp, strec, 0, &ier );
	    cfl_trln( fp, sizeof(buff), buff, &ier );

	    npBnd = 0;
	    sscanf ( buff, "%d %f %f %f %f %f %f", &npts,
                     &lat1, &lat2, &lon1, &lon2,
		     &(xpBnd[npBnd]), &(ypBnd[npBnd]) );
	    npts /= 2;
	    npBnd++;

	    while ( npBnd < npts )  {

                cfl_trln ( fp, sizeof(buff), buff, &ier );
	        /*
	         *  Points come in pairs of (lat,lon), 
	         *  i.e., they are never split between lines.
	         */
	        ptr = strtok ( buff, " " );
	        while ( ptr != '\0' )  {
		    sscanf ( ptr, "%f", &(xpBnd[npBnd]) );
	            ptr = strtok ( NULL, " " );
		    sscanf ( ptr, "%f", &(ypBnd[npBnd]) );
		    npBnd++;
	            ptr = strtok ( NULL, " " );
	        }

            }

	    /*
	     *  Check if this polygon contains the point (plat,plon).
	     */
	    cgr_inpoly ( sys_M, &one, &plat, &plon, sys_M, &npBnd,
		         xpBnd, ypBnd, &inout, &ier );

	    if ( inout == 1 )  {

	        /*
	         *  Input (plat,plon) inside this bounded area;
	         *  fill string, close boundaries file and return.
	         */

	        if ( nhot == 0 )  {

	            nhot = 1;
                    mx1lon = clo.loc[which].bnd.bound[pbnd[ii]].maxlon;
                    mn1lon = clo.loc[which].bnd.bound[pbnd[ii]].minlon;
                    mx1lat = clo.loc[which].bnd.bound[pbnd[ii]].maxlat;
                    mn1lat = clo.loc[which].bnd.bound[pbnd[ii]].minlat;

	            hotlist[0] = pbnd[ii];
	            hotstrec[0] = clo.loc[which].bnd.bound[pbnd[ii]].strec;

	            whichBnd = which;

	        }
            
	        else if ( nhot == 1 )  {

	            /*
	             *  Allow for the possibility of multiple enclosing polygons 
                     *  and select the innermost polygon based on the maximum
                     *  longitude of the polygon.  If max longitude of two 
                     *  polygons are the same, check the min longitudes
	             */
	       
	            if ( pbnd[ii] == hotlist[0] )  {

		        /*
		         * Inside another part of the same bound...
		         * either this one or the previous one must be a hole, 
		         * so ignore it completely.
		         *
		         * For this logic to work properly, it is assumed that 
		         * all parts of a particular bound are processed
		         * before moving on to the next bound.
		         */
		        nhot = 0;
	            }
	            else  {
		
		        tb = &(clo.loc[which].bnd.bound[pbnd[ii]]);

                        if ( tb->maxlon < mx1lon || tb->minlon > mn1lon ||
		             tb->maxlat < mx1lat || tb->minlat > mn1lat ) {

	                    hotlist[0] = pbnd[ii];
	                    hotstrec[0] = 
				clo.loc[which].bnd.bound[pbnd[ii]].strec;
                            mx1lon = tb->maxlon;
                            mn1lon = tb->minlon;
                            mx1lat = tb->maxlat;
                            mn1lat = tb->minlat;

	                    whichBnd = which;

	                }
	            }
	        }
    	    }
        }
    
        if ( nhot == 1 )  {
            sprintf( strout, "%s %-.2f %-.2f",
                 clo.loc[whichBnd].bnd.bound[hotlist[0]].name,
                 clo.loc[whichBnd].bnd.bound[hotlist[0]].cenlat,
                 clo.loc[whichBnd].bnd.bound[hotlist[0]].cenlon );
	    *iret = 0;
        }

        /*
         *  Input (plat,plon) not inside any bound or bound part;
         *  close boundaries file and return.
         */

        cfl_clos ( fp, &ier );

    }

    if ( nposs > 0 )  {
	G_FREE ( pbnd, int );
	G_FREE ( pprt, int );
    }

}
