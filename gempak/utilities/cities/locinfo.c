#include	"geminc.h"
#include	"gemprm.h"
#include	"clocmn.h"

#define		MAX_STATES	60
#define		MAX_LONS	45
#define		MAX_LATS	15
#define		MAX_SIZES	10

/************************************************************************
 * locinfo.c                                                            *
 *                                                                      *
 * This module contains the main program of locinfo.                    *
 *                                                                      *
 * CONTENTS:                                                            *
 *      main()           main program of locinfo.                       *
 ***********************************************************************/

/*=====================================================================*/

int main (int argc, char **argv)
/************************************************************************
 * main                                                                 *
 *                                                                      *
 * Main program of locinfo.                                             *
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
 * D.W.Plummer/NCEP     12/98                                           *
 * T. Piper/SAIC	12/07	Removed unused variables		*
 ***********************************************************************/

{
char	buff[80];
char	st[3], sttest[3], country[3], abbr[16], name[64];
int	i, j, k, l, ier;
int	i1, size, sizetest, lontest, lattest;
int	pst, plt, pln, psz;
int	ilat, ilon, index;
float	flat, flon, minlat, minlon, maxlat, maxlon;

FILE	*fp;

CityLoc_t	cityloc;
/*---------------------------------------------------------------------*/

    /*
     *  Print out the name of the LOC city file.
     */
    strcpy ( cityloc.filename, argv[1] );
    printf("!\n!    CITY FILENAME (LOC)\n%s\n!\n", 
		cityloc.filename );

    fp = (FILE *)cfl_tbop ( cityloc.filename, "cities", &ier );

    cfl_trln ( fp, sizeof(buff), buff, &ier );

    cityloc.reclen = strlen(buff) + 1;
    cityloc.hdrlen = ftell( fp ) - cityloc.reclen;

    /*
     *  Print out header length and (fixed) record length.
     */
    printf("!    HEADER LENGTH (bytes)\n%d\n!\n", cityloc.hdrlen );
    printf("!    RECORD LENGTH (bytes)\n%d\n!\n", cityloc.reclen );

    /*
     *  Allocate and initialize state location structure
     */
    cityloc.ncities = 0;
    cityloc.nstates = 0;
    cityloc.state = (StateInfo_t *)malloc(MAX_STATES*sizeof(StateInfo_t));
    for ( i = 0; i < MAX_STATES; i++ )  {
	cityloc.state[i].strec   = 0;
	cityloc.state[i].ncities = 0;
	cityloc.state[i].minlat  = RMISSD;
	cityloc.state[i].minlon  = RMISSD;
	cityloc.state[i].maxlat  = RMISSD;
	cityloc.state[i].maxlon  = RMISSD;
	cityloc.state[i].nlons   = 0;
        cityloc.state[i].lon = 
	    (LonInfo_t *)malloc(MAX_LONS*sizeof(LonInfo_t));
	for ( j = 0; j < MAX_LONS; j++ )  {
	    cityloc.state[i].lon[j].value   = 0;
	    cityloc.state[i].lon[j].strec   = 0;
	    cityloc.state[i].lon[j].ncities = 0;
	    cityloc.state[i].lon[j].nlats   = 0;
            cityloc.state[i].lon[j].lat = 
		(LatInfo_t *)malloc(MAX_LATS*sizeof(LatInfo_t));
	    for ( k = 0; k < MAX_LATS; k++ )  {
	        cityloc.state[i].lon[j].lat[k].value   = 0;
	        cityloc.state[i].lon[j].lat[k].strec   = 0;
	        cityloc.state[i].lon[j].lat[k].ncities = 0;
	        cityloc.state[i].lon[j].lat[k].nsizes  = 0;
                cityloc.state[i].lon[j].lat[k].size = 
		    (SizeInfo_t *)malloc(MAX_SIZES*sizeof(SizeInfo_t));
	        for ( l = 0; l < MAX_SIZES; l++ )  {
		    cityloc.state[i].lon[j].lat[k].size[l].value   = 0;
		    cityloc.state[i].lon[j].lat[k].size[l].strec   = 0;
		    cityloc.state[i].lon[j].lat[k].size[l].ncities = 0;
		}
	    }
	}
    }
    
    sscanf ( buff, "%s %d %s %s %s %d %d %d %d", 
             abbr, &index, name, sttest, country, 
             &ilat, &ilon, &i1, &size );

    cityloc.ncities++;

    flat = ilat / 100.0;
    flon = ilon / 100.0;
    ilat = flat;
    ilon = flon;
    minlat = flat;
    maxlat = flat;
    minlon = flon;
    maxlon = flon;

    strcpy( cityloc.state[0].state, sttest );
    strcpy( st, sttest );
    cityloc.state[0].strec = 0;

    cityloc.state[0].ncities++;
    cityloc.state[0].lon[0].ncities++;
    cityloc.state[0].lon[0].lat[0].ncities++;
    cityloc.state[0].lon[0].lat[0].size[0].ncities++;

    cityloc.state[0].lon[0].lat[0].nsizes++;
    cityloc.state[0].lon[0].nlats++;
    cityloc.state[0].nlons++;
    cityloc.nstates++;

    cityloc.state[0].lon[0].value = ilon;
    cityloc.state[0].lon[0].lat[0].value = ilat;

    ier = 0;
    while ( ier == 0 )  {

	cfl_trln ( fp, sizeof(buff), buff, &ier );

	if ( ier == 0 )  {

	    cityloc.ncities++;
	    sscanf ( buff, "%s %d %s %s %s %d %d %d %d", 
		     abbr, &index, name, sttest, country, 
		     &lattest, &lontest, &i1, &sizetest );

	    flat = lattest / 100.0;
	    flon = lontest / 100.0;
	    lattest = flat;
	    lontest = flon;

    	    pst = cityloc.nstates - 1;
    	    pln = cityloc.state[pst].nlons - 1;
    	    plt = cityloc.state[pst].lon[pln].nlats - 1;
    	    psz = cityloc.state[pst].lon[pln].lat[plt].nsizes - 1;

	    if ( strcmp(sttest,st) == 0 )  {

		cityloc.state[pst].ncities++;
		minlat = G_MIN( minlat, flat );
		maxlat = G_MAX( maxlat, flat );
		minlon = G_MIN( minlon, flon );
		maxlon = G_MAX( maxlon, flon );

                if ( ilon == lontest )  {

                    cityloc.state[pst].lon[pln].ncities++;

                    if ( ilat == lattest )  {

                        cityloc.state[pst].lon[pln].lat[plt].ncities++;

                        if ( size == sizetest )  {

                            cityloc.state[pst].lon[pln].lat[plt].size[psz].ncities++;

                        }
                        else  {

                            cityloc.state[pst].lon[pln].lat[plt].nsizes++;

                            psz = cityloc.state[pst].lon[pln].lat[plt].nsizes - 1;
                            cityloc.state[pst].lon[pln].lat[plt].size[psz].value =
				sizetest;
                            cityloc.state[pst].lon[pln].lat[plt].size[psz].strec = 
			        cityloc.ncities - 1;
                            cityloc.state[pst].lon[pln].lat[plt].size[psz].ncities=1;

                            size = sizetest;

                        }

                    }
                    else  {

                        cityloc.state[pst].lon[pln].nlats++;

                        plt = cityloc.state[pst].lon[pln].nlats - 1;
    		        cityloc.state[pst].lon[pln].lat[plt].strec = cityloc.ncities-1;
    		        cityloc.state[pst].lon[pln].lat[plt].value = lattest;
		        cityloc.state[pst].lon[pln].lat[plt].ncities = 1;
		        cityloc.state[pst].lon[pln].lat[plt].nsizes = 1;

                        psz = cityloc.state[pst].lon[pln].lat[plt].nsizes - 1;
                        cityloc.state[pst].lon[pln].lat[plt].size[psz].value=sizetest;
                        cityloc.state[pst].lon[pln].lat[plt].size[psz].strec = 
			    cityloc.ncities-1;
                        cityloc.state[pst].lon[pln].lat[plt].size[psz].ncities = 1;

                        ilat = lattest;
                        size = sizetest;

                    }

                }
                else  {

                    cityloc.state[pst].nlons++;

    	            pln = cityloc.state[pst].nlons - 1;
    		    cityloc.state[pst].lon[pln].value = lontest;
    		    cityloc.state[pst].lon[pln].strec = cityloc.ncities-1;
		    cityloc.state[pst].lon[pln].ncities = 1;
		    cityloc.state[pst].lon[pln].nlats = 1;

                    plt = cityloc.state[pst].lon[pln].nlats - 1;
		    cityloc.state[pst].lon[pln].lat[plt].value = lattest;
		    cityloc.state[pst].lon[pln].lat[plt].strec = cityloc.ncities - 1;
		    cityloc.state[pst].lon[pln].lat[plt].ncities = 1;
		    cityloc.state[pst].lon[pln].lat[plt].nsizes = 1;

                    psz = cityloc.state[pst].lon[pln].lat[plt].nsizes - 1;
                    cityloc.state[pst].lon[pln].lat[plt].size[psz].value = sizetest;
                    cityloc.state[pst].lon[pln].lat[plt].size[psz].strec = 
			cityloc.ncities - 1;
                    cityloc.state[pst].lon[pln].lat[plt].size[psz].ncities = 1;

                    ilon = lontest;
                    ilat = lattest;
                    size = sizetest;

                }

	    }
	    else  {

		cityloc.state[pst].minlat = minlat;
		cityloc.state[pst].minlon = minlon;
		cityloc.state[pst].maxlat = maxlat;
		cityloc.state[pst].maxlon = maxlon;
    		minlat = flat;
    		maxlat = flat;
		minlon = flon;
		maxlon = flon;

		cityloc.nstates++;

    	        pst = cityloc.nstates - 1;
    		strcpy( cityloc.state[pst].state, sttest );
    		cityloc.state[pst].strec = cityloc.ncities-1;
		cityloc.state[pst].ncities = 1;
	        cityloc.state[pst].nlons = 1;

    	        pln = cityloc.state[pst].nlons - 1;
    		cityloc.state[pst].lon[pln].value = lontest;
    		cityloc.state[pst].lon[pln].strec = cityloc.ncities-1;
		cityloc.state[pst].lon[pln].ncities = 1;
		cityloc.state[pst].lon[pln].nlats = 1;

    	        plt = cityloc.state[pst].lon[pln].nlats - 1;
    		cityloc.state[pst].lon[pln].lat[plt].value = lattest;
    		cityloc.state[pst].lon[pln].lat[plt].strec = cityloc.ncities-1;
		cityloc.state[pst].lon[pln].lat[plt].ncities = 1;
    	        cityloc.state[pst].lon[pln].lat[plt].nsizes = 1;

    	        psz = cityloc.state[pst].lon[pln].lat[plt].nsizes - 1;
    		cityloc.state[pst].lon[pln].lat[plt].size[psz].value = sizetest;
    		cityloc.state[pst].lon[pln].lat[plt].size[psz].strec=cityloc.ncities-1;
		cityloc.state[pst].lon[pln].lat[plt].size[psz].ncities = 1;

		strcpy( st, sttest );
		ilon = lontest;
		ilat = lattest;
		size = sizetest;

	    }

	}

    }

    pst = cityloc.nstates - 1;
    cityloc.state[pst].minlat = minlat;
    cityloc.state[pst].minlon = minlon;
    cityloc.state[pst].maxlat = maxlat;
    cityloc.state[pst].maxlon = maxlon;

    /*
     *  Print out number of cities and states.
     */
    printf("!    TOTAL NUMBER OF CITIES\n%d\n!\n", cityloc.ncities );

    printf("!    NUMBER OF STATES\n%d\n!\n", cityloc.nstates );

    printf("!    CITIES (LOC) STRUCTURE INFORMATION\n!\n" );

    /*
     *  The following section is kept for future program diagnostics
     */
    /*
    for ( i = 0; i < cityloc.nstates; i++ )  {
	printf("!\n%-s strec=%-7d ncities=%-5d range=%-.2f;%-.2f;%-.2f;%-.2f nlons=%-2d\n",
		cityloc.state[i].state, 
		cityloc.state[i].strec, 
		cityloc.state[i].ncities, 
		cityloc.state[i].minlat, cityloc.state[i].minlon, 
		cityloc.state[i].maxlat, cityloc.state[i].maxlon, 
		cityloc.state[i].nlons );
        for ( j = 0; j < cityloc.state[i].nlons; j++ )  {
	    printf("\tvalue=%-6d strec=%-8d ncities=%-6d nlats=%-6d\n",
		    cityloc.state[i].lon[j].value, 
		    cityloc.state[i].lon[j].strec, 
		    cityloc.state[i].lon[j].ncities, 
		    cityloc.state[i].lon[j].nlats );
            for ( k = 0; k < cityloc.state[i].lon[j].nlats; k++ )  {
	        printf("\t\tvalue=%-6d strec=%-8d ncities=%-6d nsizes=%-6d\n",
			cityloc.state[i].lon[j].lat[k].value, 
			cityloc.state[i].lon[j].lat[k].strec, 
			cityloc.state[i].lon[j].lat[k].ncities, 
			cityloc.state[i].lon[j].lat[k].nsizes );
                for ( l = 0; l < cityloc.state[i].lon[j].lat[k].nsizes; l++ )  {
	            printf("\t\t\tvalue=%-6d strec=%-8d ncities=%-6d\n",
			    cityloc.state[i].lon[j].lat[k].size[l].value, 
			    cityloc.state[i].lon[j].lat[k].size[l].strec, 
			    cityloc.state[i].lon[j].lat[k].size[l].ncities );
                }
            }
        }
    }
     */

    /*
     *  This is the same as immediately above w/o the helpful printed text.
     */
    for ( i = 0; i < cityloc.nstates; i++ )  {
	printf("!\n%-s %-7d %-5d %-.2f %-.2f %-.2f %-.2f %-2d\n",
		cityloc.state[i].state, 
		cityloc.state[i].strec, 
		cityloc.state[i].ncities, 
		cityloc.state[i].minlat, cityloc.state[i].minlon, 
		cityloc.state[i].maxlat, cityloc.state[i].maxlon, 
		cityloc.state[i].nlons );
        for ( j = 0; j < cityloc.state[i].nlons; j++ )  {
	    printf("\t%-6d %-8d %-6d %-6d\n",
		    cityloc.state[i].lon[j].value, 
		    cityloc.state[i].lon[j].strec, 
		    cityloc.state[i].lon[j].ncities, 
		    cityloc.state[i].lon[j].nlats );
            for ( k = 0; k < cityloc.state[i].lon[j].nlats; k++ )  {
	        printf("\t\t%-6d %-8d %-6d %-6d\n",
			cityloc.state[i].lon[j].lat[k].value, 
			cityloc.state[i].lon[j].lat[k].strec, 
			cityloc.state[i].lon[j].lat[k].ncities, 
			cityloc.state[i].lon[j].lat[k].nsizes );
                for ( l = 0; l < cityloc.state[i].lon[j].lat[k].nsizes; l++ )  {
	            printf("\t\t\t%-6d %-8d %-6d\n",
			    cityloc.state[i].lon[j].lat[k].size[l].value, 
			    cityloc.state[i].lon[j].lat[k].size[l].strec, 
			    cityloc.state[i].lon[j].lat[k].size[l].ncities );
                }
            }
        }
    }
    return(0);
}
