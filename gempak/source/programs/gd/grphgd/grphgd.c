#include "geminc.h"
#include "gemprm.h"

int main ( void )
/************************************************************************
 * GRPHGD								*
 *									*
 * This program runs the graph-to-grid algorithm.			*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 2/05	Re-write in C based on grphgd.f		*
 * R. Tian/SAIC		 3/05	Added dg_intl				*
 * M. Li/SAIC		 4/05	Modified ggdriv				*
 * D.W.Plummer/NCEP	 5/05	Move dg_intl into gg_driv driver	*
 * m.gamazaychikov/SAIC	12/05	Add ces_gtrtbl				*
 * T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
 ***********************************************************************/
{
int	respnd, iperr, done, one=1, ier, iret, rspflg=G_TRUE;
int	kx=0, ky=0, npoints;
float	*grid, *grid1, *hist, *work1, *work2, *work3, *buffer;
/*---------------------------------------------------------------------*/
/*
 *  Initialize TAE.
 */
    ip_init ( &respnd, &iperr );
    if ( iperr == G_NORMAL )  {
	ip_idnt ( "GRPHGD", &ier, strlen("GRPHGD") );

/*
 *  Initialize grid library common area grdcmn.cmn
 */
        gd_init  ( &ier );
	done = G_FALSE;

    }
    else  {

	iperr = -1;
	done = G_TRUE;

    }

/*
 *  Initialize GEMPLT in order to set grid navigation later.
 */
    if ( done == G_FALSE )  {

	gg_init ( &one, &iret );

	if ( iret != G_NORMAL )  {
	    iperr = -3;
	    done = G_TRUE;
	}
    }

/*
 *  Initialize device.
 */
    if ( done == G_FALSE )  {

	gg_sdev ( "GN", &ier, strlen("GN") );

	if ( ier != G_NORMAL )  {
	    done = G_TRUE;
	}
    }

/*
 *  Initialize the _grpTbl structure.
 */
    if ( done == G_FALSE )  {
                                                                                                        
        ces_gtrtbl( &ier );
                                                                                                        
        if ( ier != G_NORMAL )  {
            done = G_TRUE;
        }
    }
    
/*
 * Main loop.
 */
    while ( done == G_FALSE ) {

	npoints = LLMXTG;
	G_MALLOC ( grid,	float, npoints, "Error allocating grid" );
	G_MALLOC ( grid1,	float, npoints, "Error allocating grid1" );
	G_MALLOC ( hist,	float, npoints, "Error allocating hist" );
	G_MALLOC ( work1,	float, npoints, "Error allocating work1" );
	G_MALLOC ( work2,	float, npoints, "Error allocating work2" );
	G_MALLOC ( work3,	float, npoints, "Error allocating work3" );
	G_MALLOC ( buffer,	float, npoints, "Error allocating buffer" );

	ggdriv ( grid, grid1, &kx, &ky, hist, work1, work2, work3, 
		 buffer, &rspflg, &ier );

	G_FREE ( hist, float );
	G_FREE ( grid, float );
	G_FREE ( grid1, float );
	G_FREE ( work1, float );
	G_FREE ( work2, float );
	G_FREE ( work3, float );
	G_FREE ( buffer, float );

/*
 * Call dynamic tutor.
 */
	ip_dynm ( &done, &ier );

    }

/*
 * Final error messages.
 */
    if ( iperr != G_NORMAL )  {
	er_wmsg ( "GRPHGD", &iperr, " ", &ier, strlen("GRPHGD"), strlen(" ") );
    }

    ip_exit ( &iret );

    return 0;

}
