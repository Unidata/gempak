#include "geminc.h"
#include "gemprm.h"

#include "na.h"

void	put_gemgrid(const int *iacss, char *varname, int gribid,
		int vcord, int lev1, int lev2,
		int year, int month, int day, int hour,
		int nlat, int nlon, float *grid, int nbits, int *iret)
{
    int ier, iy;
    int jtime[3], ivcord, level1, level2, ighdr[LLGDHD];
    char	gdattm1[DTTMSZ], gdattm2[DTTMSZ], vparm[5], parm[13];
    const int ipktyp=MDGGRB, rewrit=G_TRUE;
    static float *gridrow=NULL;
    static int rowsize=0;

    jtime[0] = ( ( year%100 * 100 ) + month ) * 100 + day;
    jtime[1] = hour * 100;
    jtime[2] = 0;
    gdattm1[0] = '\0';
    gdattm2[0] = '\0';
    ctg_itoc ( jtime, gdattm1, &ier );
    if ( ier != 0 ) {
	*iret = -17;
	return;
    }


    if ( _naparm.mparms[gribid-1][0] == '\0' ) {
        strcpy ( parm, varname );
    }
    else {
	strcpy ( parm, _naparm.mparms[gribid-1] );
    }

    if ( _navcrd.mvcord[vcord-1][0] == '\0' ) {
	*iret = +2;
	printf("Error: Could not determine vertical coordinate name [%d]\n", vcord);
	return;
    }
    else {
        clv_cord ( _navcrd.mvcord[vcord-1], vparm, &ivcord, &ier );

    }

    /*
     * Set the levels.
     */
    level1 = lev1 * pow(10, _navcrd.mvscal[vcord-1]);
    if ( lev2 == -1 ) {
        level2 = lev2;
    }
    else {
	level2 = lev2 * pow(10, _navcrd.mvscal[vcord-1]);
    }

    if ( nlon > rowsize )
        {
        if ( gridrow != NULL ) free(gridrow);
        gridrow = (float *)malloc(nlon * sizeof(float));
        if ( gridrow == NULL ) {
	    rowsize = 0;
            *iret = -1;
	    return;
	}
        rowsize = nlon;
	}
    for ( iy = 0; iy < nlat / 2; iy++)
        {
        memcpy ( (void *)gridrow, (void *)&grid[iy*nlon], nlon*sizeof(float) );
        memcpy ( (void *)&grid[iy*nlon], (void *)&grid[(nlat - 1 - iy)*nlon],
		nlon*sizeof(float) );
        memcpy ( (void *)&grid[(nlat - 1 - iy)*nlon], (void *)gridrow,
		nlon*sizeof(float) );
        }

    cgd_wpgd ( iacss, (const float * ) grid, (const int *) &nlon,
                (const int *) &nlat, (const int *)ighdr, (const char * ) gdattm1,
                ( const char * )gdattm2, (const int *) &level1, (const int *) &level2,
                (const int *) &ivcord, (const char *)parm, &rewrit,
                &ipktyp, (const int *) &nbits, iret );
    /* printf("write grid [%d]: %s %s %s %d %d\n",*iret, gdattm1, parm, vparm, level1, level2 );*/
}
