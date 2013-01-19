#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

#define	MAX_BOUNDS	10000	
#define	MAX_NAMELEN	256

/************************************************************************
 * createbinfo.c                                                        *
 *                                                                      *
 * This module contains the main program of createbinfo.                *
 *                                                                      *
 * CONTENTS:                                                            *
 *      main()           main program of createbinfo.                   *
 ***********************************************************************/

/*=====================================================================*/

int main (int argc , char **argv)

/************************************************************************
 * main                                                                 *
 *                                                                      *
 * Main program of createbinfo.                                         *
 *                                                                      *
 * Output (from printf) must be re-directed to the proper info file 	*
 * within the script.        						*
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
 * D.W.Plummer/NCEP     12/98						* 
 * T. Piper/GSC		 8/00	Modified for new generic boundary info  *
 * D.W.Plummer/NCEP	 6/05	Incr accuracy from 2 decimal digits to 4*
 ***********************************************************************/

{
char	buff[256], id[7], name[64];
int	i, ier, ilat, ilon, k, length, nparts, nptot, npts, num, pst;
float	fltptr[20], lat1, lat2, lon1, lon2, minlat, minlon, maxlat, maxlon;

FILE	*fp;
long	lpos, lposp;

Bnd_t	boundary;
/*---------------------------------------------------------------------*/
    /*
     *  Print out the name of the BOUNDARY file.
     */
    boundary.filename = (char *)malloc( sizeof(char) * strlen(argv[1]) + 1);
    strcpy ( boundary.filename, argv[1] );
    printf("!\n!    BOUNDARIES FILENAME \n%s\n!\n", 
		boundary.filename );

    fp = (FILE *)cfl_tbop ( boundary.filename, "bounds", &ier );

    /*
     *  Allocate and initialize boundary location structure
     */
    boundary.nbnd = 0;
    boundary.bound = (BInfo_t *)malloc(MAX_BOUNDS*sizeof(BInfo_t));
    for ( i = 0; i < MAX_BOUNDS; i++ )  {
	boundary.bound[i].name = (char *)malloc( sizeof(char) * MAX_NAMELEN + 1);
	boundary.bound[i].name[0] = '\0';
	boundary.bound[i].info = (char *)malloc( sizeof(char) * MAX_NAMELEN + 1);
	boundary.bound[i].info[0] = '\0';
	boundary.bound[i].strec   = 0;
    	boundary.bound[i].cenlat  = RMISSD;
	boundary.bound[i].cenlon  = RMISSD;
	boundary.bound[i].minlat  = RMISSD;
	boundary.bound[i].minlon  = RMISSD;
	boundary.bound[i].maxlat  = RMISSD;
	boundary.bound[i].maxlon  = RMISSD;
	boundary.bound[i].nparts  = 0;
    }
    
    cfl_wher ( fp, &lpos, &ier );
    cfl_trln ( fp, sizeof(buff), buff, &ier );

    if ( ier == 0 )  {

	sscanf ( buff, "%s %s %d %d %d", id,
		     name, &ilat, &ilon, &nparts );

	boundary.bound[0].bndspt = 
	    (Bndsprt_t *)malloc(nparts*sizeof(Bndsprt_t));

	/*  Read the second header line  */
	cfl_trln ( fp, sizeof(buff), buff, &ier );
	strcpy ( boundary.bound[0].info, buff);
	
	for ( k = 0; k < nparts; k++ )  {

    	    cfl_wher ( fp, &lposp, &ier );
            cfl_trln ( fp, sizeof(buff), buff, &ier );
	
	    sscanf ( buff, "%d %f %f %f %f", &npts,
		    &lat1, &lat2, &lon1, &lon2 );
	    boundary.bound[0].bndspt[k].minlat = G_MIN ( lat1, lat2 );
	    boundary.bound[0].bndspt[k].maxlat = G_MAX ( lat1, lat2 );
	    boundary.bound[0].bndspt[k].minlon = G_MIN ( lon1, lon2 );
	    boundary.bound[0].bndspt[k].maxlon = G_MAX ( lon1, lon2 );
	    boundary.bound[0].bndspt[k].strec = lposp;
	    boundary.bound[0].bndspt[k].npts = npts / 2;
	    cst_rxbl ( buff, buff, &length, &ier );
	    cst_rlst ( buff, ' ', RMISSD, (int) (sizeof(fltptr)/sizeof(float)), 
		       fltptr, &num, &ier);
	    nptot = ( num - 5 );
	    while ( ier == 0 && nptot < npts )  {
                cfl_trln ( fp, sizeof(buff), buff, &ier );
	        if ( ier == 0 )  {
		    cst_rxbl ( buff, buff, &length, &ier );
		    cst_rlst ( buff, ' ', RMISSD, sizeof(fltptr)/sizeof(float), 
			       fltptr, &num, &ier);
		    nptot += num;
	        }   /*  Loop over all points in one part  */
	    }

	}  /*  Loop over all parts in one bound  */

	if ( ier == 0 )  {

	    strcpy ( boundary.bound[0].name, name );
	    boundary.bound[0].strec = lpos;
	    boundary.bound[0].cenlat = ilat / 100.0;
	    boundary.bound[0].cenlon = ilon / 100.0;
	    boundary.bound[0].nparts = nparts;

	    boundary.nbnd++;

	}

    }

    while ( ier == 0 )  {

        cfl_wher ( fp, &lpos, &ier );
	cfl_trln ( fp, sizeof(buff), buff, &ier );

        if ( ier == 0 )  {

	    boundary.nbnd++;
	    pst = boundary.nbnd - 1;
	    sscanf ( buff, "%s %s %d %d %d", id,
		   name,  &ilat, &ilon, &nparts );
	    strcpy ( boundary.bound[pst].name, name );
	    boundary.bound[pst].strec = lpos;
	    boundary.bound[pst].cenlat = ilat / 100.0;
            boundary.bound[pst].cenlon = ilon / 100.0;
	    boundary.bound[pst].nparts = nparts;

	/*  Read the second header line  */
	    cfl_trln ( fp, sizeof(buff), buff, &ier );
  	    strcpy ( boundary.bound[pst].info, buff);

            boundary.bound[pst].bndspt = 
		(Bndsprt_t *)malloc(nparts*sizeof(Bndsprt_t));

	    for ( k = 0; k < nparts; k++ )  {

    	      cfl_wher ( fp, &lposp, &ier );
              cfl_trln ( fp, sizeof(buff), buff, &ier );

	      sscanf ( buff, "%d %f %f %f %f", &npts,
		    &lat1, &lat2, &lon1, &lon2 );
	      boundary.bound[pst].bndspt[k].minlat = G_MIN(lat1,lat2);
	      boundary.bound[pst].bndspt[k].maxlat = G_MAX(lat1,lat2);
	      boundary.bound[pst].bndspt[k].minlon = G_MIN(lon1,lon2);
	      boundary.bound[pst].bndspt[k].maxlon = G_MAX(lon1,lon2);
	      boundary.bound[pst].bndspt[k].strec = lposp;
	      boundary.bound[pst].bndspt[k].npts = npts / 2;
	      cst_rxbl( buff, buff, &length, &ier );
	      cst_rlst( buff, ' ', RMISSD, (int) (sizeof(fltptr)/sizeof(float)),
		       fltptr, &num, &ier);
	      nptot = ( num - 5 );
	      while ( ier == 0 && nptot < npts )  {
                cfl_trln ( fp, sizeof(buff), buff, &ier );
	        if ( ier == 0 )  {
		    cst_rxbl ( buff, buff, &length, &ier );
		    cst_rlst ( buff, ' ', RMISSD, sizeof(fltptr)/sizeof(float), 
			       fltptr, &num, &ier);
		    nptot += num;
	        }
	      }

	    }

	}

    }

    boundary.maxpts = 0;
    for ( i = 0; i < boundary.nbnd; i++ )  {
        minlat =   90.0;
        minlon =  360.0;
        maxlat =  -90.0;
        maxlon = -360.0;
        for ( k = 0; k < boundary.bound[i].nparts; k++ )  {
	  minlat=G_MIN ( minlat, boundary.bound[i].bndspt[k].minlat );
	  minlon=G_MIN ( minlon, boundary.bound[i].bndspt[k].minlon );
	  maxlat=G_MAX ( maxlat, boundary.bound[i].bndspt[k].maxlat );
	  maxlon=G_MAX ( maxlon, boundary.bound[i].bndspt[k].maxlon );
	  boundary.maxpts = 
	    G_MAX ( boundary.maxpts, boundary.bound[i].bndspt[k].npts );
        }
	boundary.bound[i].minlat = minlat;
	boundary.bound[i].minlon = minlon;
	boundary.bound[i].maxlat = maxlat;
	boundary.bound[i].maxlon = maxlon;
   } 

    /*
     *  Print out number of bounds.
     */
    printf("!    TOTAL NUMBER OF BOUNDS\n%d\n!\n", boundary.nbnd );

    printf("!    MAX NUMBER OF POINTS per BOUND\n%d\n!\n", boundary.maxpts );

    printf("!    BOUNDARY STRUCTURE INFORMATION\n!\n" );

    /*
     *  Dump the information.
     */
    for ( i = 0; i < boundary.nbnd; i++ )  {
	printf("!\n%-s %-12ld %-.2f %-.2f %-.2f %-.2f %-.2f %-.2f %-5d\n",
		boundary.bound[i].name, 
		boundary.bound[i].strec, 
		boundary.bound[i].cenlat,
		boundary.bound[i].cenlon,
		boundary.bound[i].minlat, 
		boundary.bound[i].minlon, 
		boundary.bound[i].maxlat, 
		boundary.bound[i].maxlon,
		boundary.bound[i].nparts );
	printf("%s\n", boundary.bound[i].info);
	for ( k = 0; k < boundary.bound[i].nparts; k++ )  {
	    printf("\t%-12ld %-.2f %-.2f %-.2f %-.2f %-8d \n",
		boundary.bound[i].bndspt[k].strec, 
		boundary.bound[i].bndspt[k].minlat, 
		boundary.bound[i].bndspt[k].minlon, 
		boundary.bound[i].bndspt[k].maxlat, 
		boundary.bound[i].bndspt[k].maxlon, 
		boundary.bound[i].bndspt[k].npts );
        }
    }
    return(0);
}
