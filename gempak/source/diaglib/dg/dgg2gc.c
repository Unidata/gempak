#include "dg.h"

void dg_g2gc ( const int *inttyp, const int *glbwi, const int *kxi,
               const int *kyi, const float *grdi, const int *kxo,
	       const int *kyo, float *gixo, float *giyo, float *grdo,
	       int *iret )
/************************************************************************
 * dg_g2gc								*
 *									*
 * This subroutine remaps cosines of data from one grid to another grid.*
 *									*
 * A globe-wrapping grid must be configured so that the last column	*
 * and first column are identical.					*
 *									*
 * If INTTYP=0, the code determines automatically whether to use a	*
 * simple bi-linear interpolation or an area average preserving re-	*
 * mapping of data.  The method is determined locally and may vary	*
 * across the grid.  The transition to area average preserving remap-	*
 * ping occurs when one output grid box contains approximately four (4)	*
 * or more input grid boxes.  Since bilinear interpolation preserves	*
 * the area average when grid resolutions are comparable or when the	*
 * input grid is coarse compared to the output grid, this method always *
 * preserves area averages.						*
 *									*
 * If INTTYP=1, the code performs nearest neighbor assignment.		*
 * If INTTYP=2, the code performs only bi-linear interpolation.		*
 *									*
 * dg_g2gc ( inttyp, glbwi, kxi, kyi, grdi, kxo, kyo, gixo, giyo, grdo,	*
 *	     iret )							*
 *									*
 * Input parameters:							*
 *	*inttyp		const int	Remapping type			*
 *					 = 0 area average preserving	*
 *					 = 1 nearest point assignment	*
 *					 = 2 bi-linear interpolation	*
 *	*glbwi		const int	Flg for globe-wrapping input grd*
 *	*kxi		const int	Number of x pts on input grid	*
 *	*kyi		const int	Number of y pts on input grid	*
 *	*grdi		const float	Input grid			*
 *	*kxo		const int	Number of x pts on output grid	*
 *	*kyo		const int	Number of y pts on output grid	*
 *	*gixo		flaot		Input grd rltv x on output grid *
 *	*giyo		float		Input grd rltv y on output grid *
 *									*
 * Output parameters:							*
 *	*grdo		float		Output grid of results		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-68 = INTTYP is not valid	*
 *					-69 = grid rel position error	*
 **									*
 * Log:									*
 * K. Brill/HPC		 3/04	Created from DG_G2GI			*
 * K. Brill/HPC		 4/04	IF (k==0) k=1; add .005 to rad		*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    float rkio2, rdtst, xchk, rad2m, dx, dy, d2, x, y, xi, omx, omy,
        rad, rad2, radx2, sum, sumw, ri, r2, dr2, dr22, wt, tmp;
    int ir[4], jr[4], kio2, ityp, cidx, cidx1, i, j, k, io, jo, in, jn,
        idx1, idx2, idx3, idx4, ip, ib, jb, ie, je, ii, jj, jdif, npts;
    int rflct;
/*----------------------------------------------------------------------*/
    *iret  = 0;
    if ( (*inttyp) < 0 || (*inttyp) > 2 ) {
	*iret = -68;
	return;
    }

    kio2 = (*kxi) / 2;
    rkio2 = (float)kio2;
    rdtst = 4. / PI;

    if ( (*inttyp) == 0 && (*glbwi) == G_TRUE && ( (*kxi) % 2 ) == 1 ) {
	/*
	 * Check to relect points across the pole of a global grid.
	 * All points along bottom and top rows must be identical.
	 */
	rflct = G_TRUE;
	for ( j = 1; j <= (*kyi); j += (*kyi) - 1 ) {
	    cidx = (j - 1 ) * (*kxi);
	    xchk = cos ( grdi[cidx] );
	    i = 2;
	    while ( i <= (*kxi) && rflct == G_TRUE ) {
	        cidx = ( j - 1 ) * (*kxi) + i - 1;
		rflct = ( ! ERMISS ( grdi[cidx] ) ) ? G_TRUE : G_FALSE;
		if ( rflct == G_TRUE ) {
		    if ( G_ABS ( xchk - cos ( grdi[cidx] ) ) < RDIFFD ) {
		        rflct = G_FALSE;
		    }
		}
		i++;
	    }
	}
    } else {
	rflct = G_FALSE;
    }

    /*
     * Loop over all output grid points.
     */
    rad2m = -9999.;
    for ( jo = 1; jo <= (*kyo); jo++ ) {
	for ( io = 1; io <= (*kxo); io++ ) {
	    cidx = ( jo - 1 ) * (*kxo) + io - 1;
	    grdo[cidx] = RMISSD;
	    if ( ERMISS ( gixo[cidx] ) || ERMISS ( giyo[cidx] ) ) {
		ityp = 2;
	    } else if ( (*inttyp) == 1 ) {
		/*
		 * Assign value at nearest point.
		 */
		ityp = 2;
		in = G_NINT ( gixo[cidx] );
		if ( (*glbwi) == G_TRUE ) {
		    if ( in > (*kxi) ) in = in - (*kxi) + 1;
		    if ( in < 1 ) in = in + (*kxi) - 1;
		}
		jn = G_NINT ( giyo[cidx] );
		if ( in >= 1 && in <= (*kxi) &&
		     jn >= 1 && jn <= (*kyi) ) {
		    /*
		     * Nearest point value assignment.
		     */
		    cidx1 = ( jn - 1 ) * (*kxi) + in - 1;
		    grdo[cidx] = cos ( grdi[cidx1] );
		}
	    } else if ( (*inttyp) == 2 ) {
		ityp = 1;
	    } else {
		ityp = 0;

		/*
		 * Get radius in input grid units of the circle
		 * circumscribing the the diamond formed by the
		 * four points closest to the current point on
		 * the output grid.
		 */
                rad2 = -1.1E31;
                ir[0] = io - 1;
                jr[0] = jo;
                ir[1] = io;
                jr[1] = jo - 1;
                ir[2] = io + 1;
                jr[2] = jo;
                ir[3] = io;
                jr[3] = jo + 1;

                for ( ip = 0; ip < 4; ip++ ) {
                    cidx1 = ( jr[ip] - 1 ) * (*kxo) + ir[ip] - 1;
                    if ( ir[ip] >= 1 && ir[ip] <= (*kxo) &&
                         jr[ip] >= 1 && jr[ip] <= (*kyo) &&
                         ! ERMISS ( gixo[cidx1] ) &&
                         ! ERMISS ( giyo[cidx1] ) ) {
                        dx = gixo[cidx1] - gixo[cidx];
                        if ( (*glbwi) == G_TRUE && G_ABS (dx) > rkio2 ) {
                            /*
                             * Skip this point.
                             */
                        } else if ( G_ABS (dx) > rkio2 ) {
                            *iret = -69;
                            return;
                        } else {
                            dy = giyo[cidx1] - giyo[cidx];
                            d2 = dx * dx + dy * dy;
                            rad2 = G_MAX ( d2, rad2 );
                        }
                    }
                }

                /*
                 * Since RAD2 is the square of the radius of the
                 * circle circumscribing the output grid diamond
                 * in input grid units, multiply by .5 to get the
                 * the square of the radius of the inscribed circle.
                 * This circle circumscribes the output grid box.
                 */
                if ( rad2 > -1.0E30 ) {
                    rad2 *= .5;

                    /*
                     * If the radius is much larger than that at the
                     * adjacent point, then reduce it.
                     */
                    if ( rad2m > 0.0 && rad2 > 1.5 * rad2m ) {
                        rad2 = 1.5 * rad2m;
                    }
                    rad2m = rad2;
                } else {
                    rad2m = -9999.;
                }

                /*
                 * If the squared radius of the inscribed circle is
                 * small enough (4/PI), then there are less than four
                 * input grid boxes per output grid box and linear
                 * interpolation will suffice.
                 */
                if ( rad2 < rdtst ) {
                    ityp = 1;
                }
            }

            if ( ityp == 1 ) {
                /*
                 * Do bi-linear interpolation.
                 */
                i = (int)gixo[cidx];
                if ( (*glbwi) == G_TRUE ) {
                    if ( i >= (*kxi) ) {
                        i = i - (*kxi) + 1;
                        xi = gixo[cidx] - (float)(*kxi) + 1;
                    } else if ( i < 1 ) {
                        i = i + (*kxi) - 1;
                        xi = gixo[cidx] + (float)(*kxi) - 1;
                        if ( gixo[cidx] < 0.0 ) i--;
                    } else {
                        xi = gixo[cidx];
                    }
                } else {
                    xi = gixo[cidx];
                }

                j = (int)giyo[cidx];
                if ( i >= 1 && i <= (*kxi) &&
                     j >= 1 && j <= (*kyi) ) {
                    if ( i == (*kxi) ) i--;
                    if ( j == (*kyi) ) j--;
                    idx1 = ( j - 1 ) * (*kxi) + i - 1;
                    idx2 = ( j - 1 ) * (*kxi) + i;
                    idx3 = j * (*kxi) + i - 1;
                    idx4 = j * (*kxi) + i;
                    if ( ! ERMISS ( grdi[idx1] ) &&
                         ! ERMISS ( grdi[idx2] ) &&
                         ! ERMISS ( grdi[idx3] ) &&
                         ! ERMISS ( grdi[idx4] ) ) {
                        x = xi - (float)i;
                        y = giyo[cidx] - (float)j;
                        omx = 1. - x;
                        omy = 1. - y;
                        grdo[cidx] = ( cos ( grdi[idx1] ) * omx +
                                       cos ( grdi[idx2] ) * x ) * omy +
                                     ( cos ( grdi[idx3] ) * omx +
                                       cos ( grdi[idx4] ) * x ) * y;
                    }
                }
            } else if ( ityp == 0 ) {
                /*
                 * Do area average preserving interpolation.
                 *
                 * This integer truncation acts to pull the
                 * output grid box circumscribing circle in
                 * just a bit.
                 */
                tmp = sqrt ( rad2 );
                k = (int)tmp;
                if ( k == 0 ) k = 1;

                /*
                 * Reset rad2 accordingly.
                 */
                rad = (float)k + .005;
                rad2 = rad * rad;
                radx2 = 1. / ( 2. * rad );

                ib = G_NINT ( gixo[cidx] ) - k;
                jb = G_NINT ( giyo[cidx] ) - k;
                ie = G_NINT ( gixo[cidx] ) + k;
                je = G_NINT ( giyo[cidx] ) + k;
                sum = 0.0;
                sumw = 0.0;
                npts = 0;

                for ( j = jb; j <= je; j++ ) {
                    for ( i = ib; i <= ie; i++ ) {
                        jj = j;
                        ii = i;
                        ri = (float)i;
                        if ( (*glbwi) == G_TRUE ) {
                            if ( i > (*kxi) ) ii = i - (*kxi) + 1;
                            if ( i < 1 ) ii = i + (*kxi) - 1;
                        }

                        dx = ri - gixo[cidx];
                        dy = (float)j - giyo[cidx];
                        r2 = dx * dx + dy * dy;

                        if ( rflct == G_TRUE ) {
                            if ( j > (*kyi) ) {
                                jdif = j - (*kyi);
                                jj = (*kyi) - jdif;
                                if ( ii <= kio2 ) {
                                    ii += kio2;
                                } else {
                                    ii -= kio2;
                                }
                            } else if ( j < 1 ) {
                                jdif = 1 - j;
                                jj = 1 + jdif;
                                if ( ii <= kio2 ) {
                                    ii += kio2;
                                } else {
                                    ii -= kio2;
                                }
                            }
                        }

                        if ( r2 <= rad2 ) {
                            if ( ii >= 1 && ii <= (*kxi) &&
                                 jj >= 1 && jj <= (*kyi) ) {
                                cidx1 = ( jj - 1 ) * (*kxi) + ii - 1;
                                if ( ! ERMISS (grdi[cidx1] ) ) {
                                    /*
                                     * Compute a weighting factor based
                                     * on an estimate of how much of
                                     * the area of the input grid box
                                     * is contributing to the area
                                     * covered by the output grid box.
                                     * The criterion is (rad-r) < .5.
                                     * Using the approximation that
                                     * rad ~ r, then (rad2-r2) is nearly
                                     * equal to 2*rad(rad-r).  Substi-
                                     * tute into above inequality and
                                     * rearrange to get this criterion:
                                     * (rad2-r2)**2<rad2.  The weight is
                                     * either 1 or [.5+(rad2-r2)/rad*2].
                                     * The .5 is added on because a
                                     * point on the circle has about
                                     * half of its area inside.
                                     */
                                    dr2 = rad2 - r2;
                                    dr22 = dr2 * dr2;
                                    if ( dr22 < rad2 ) {
                                        wt = .5 + dr2 * radx2;
                                    } else {
                                        wt = 1.0;
                                    }
                                    sum += wt * cos ( grdi[cidx1] );
                                    sumw += wt;
                                    npts++;
                                }
                            }
                        }
                    }
                }

                if ( sumw >= 2.0 && npts >= 4 ) {
                    /*
                     * The value 2.0 is used as the criterion here
                     * because at least 4 points must contribute
                     * at least a half.
                     */
                    grdo[cidx] = sum / sumw;
                } else {
                    /*
                     * Perform linear interpolation, which itself
                     * preserves area averages when grid resolutions
                     * are comparable.
                     */
                    i = (int)gixo[cidx];
                    if ( (*glbwi) == G_TRUE ) {
                        if ( i >= (*kxi) ) {
                            i = i - (*kxi) + 1;
                            xi = gixo[cidx] - (float)(*kxi) + 1;
                        } else if ( i < 1 ) {
                            i = i + (*kxi) - 1;
                            xi = gixo[cidx] + (float)(*kxi) - 1;
                            if ( gixo[cidx] < 0.0 ) i--;
                        } else {
                            xi = gixo[cidx];
                        }
                    } else {
                        xi = gixo[cidx];
                    }

                    j = (int)giyo[cidx];
                    if ( i >= 1 && i <= (*kxi) && j >= 1 && j <= (*kyi) ) {
                        if ( i == (*kxi) ) i--;
                        if ( j == (*kyi) ) j--;
                        idx1 = ( j - 1 ) * (*kxi) + i - 1;
                        idx2 = ( j - 1 ) * (*kxi) + i;
                        idx3 = j * (*kxi) + i - 1;
                        idx4 = j * (*kxi) + i;
                        if ( ! ERMISS ( grdi[idx1] ) &&
                             ! ERMISS ( grdi[idx2] ) &&
                             ! ERMISS ( grdi[idx3] ) &&
                             ! ERMISS ( grdi[idx4] ) ) {
                            x = xi - (float)i;
                            y = giyo[cidx] - (float)j;
                            omx = 1. - x;
                            omy = 1. - y;
                            grdo[cidx] = ( cos ( grdi[idx1] ) * omx +
                                           cos ( grdi[idx2] ) * x ) * omy +
                                         ( cos ( grdi[idx3] ) * omx +
                                           cos ( grdi[idx4] ) * x ) * y;
                        }
                    }
                }
            }
        }
    }

    return;
}
