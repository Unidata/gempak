#include "pscmn.h"
#include "color.h"

void psltyp ( int *iltyp, int lpat[10][8], int *lpscal, int *iret )
/************************************************************************
 * PSLTYP								*
 *									*
 * This subroutine sets the hardware line type.				8
 *									*
 * PSLTYP  ( ILTYP, LPAT, LPSCAL, IRET )				*
 *									*
 * Input parameters:							*
 *	*ILTYP		INT		Line type			*
 *	LPAT [10][8]	INT		Array of all line patterns	*
 *	*LPSCAL		INT		Line dash scale			*
 *									*
 * Output parameters:							*
 *	*IRET		INT		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP 	 4/96						*
 ***********************************************************************/
{

	int	jpatno, i, numpat, done, dash, ipattt,
		ihwpat[20], ihalf, lenb, ier;
	float	psfac;
	char	buff[80], ssss[7];

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Make sure that plot file is open.  Put terminal in vector mode.
 */
	if  ( ! opnfil ) { 
	    psopen ( iret );
	    if  ( *iret != G_NORMAL )  return;
	}
	psplot = G_TRUE; 

/*
 *	Get pattern number (ones place)
 */
	jpatno = *iltyp % 10;

/*
 *	Exit for solid line.
 */
	if  ( jpatno == 1 )
	{
	    strcpy ( buff, " [] 0 setdash\n" );
	    lenb = strlen ( buff );
	    cfl_writ ( flun, lenb, (unsigned char *)buff, &ier );
	    return;
	}

/*
 *	Get pattern size factor ( ones = 1, 10's = 0.5,
 *	20's = 1, 30's = 2, 40's = 3, ... )
 */
	psfac = ( *iltyp / 10 ) - 1;
	if  ( G_DIFF(psfac, 0) )  psfac = 0.5;
	if  ( psfac <  0 )  psfac = 1.0;
/* 	psfac *= 4; */
	numpat = -1;
	done   = G_FALSE;
	dash   = G_FALSE;
	i      = 0;

	while ( ( i <= 7 ) && ( ! done ) )
	{
	    ipattt = lpat[jpatno][i] * psfac * *lpscal;

	    if  ( ipattt > 0 )
	    {
		numpat++;
		ihwpat [numpat] = ipattt;
	    }
	    else if  ( ipattt == 0 )
	    {
		done = G_TRUE;
	    }
	    else if  ( dash || ( i == 0 ) )
	    {
		ihalf = -ipattt / 2;
		ihwpat [numpat+1] = 2;
		ihwpat [numpat+2] = ihalf;
		numpat += 2;
		dash = G_TRUE;
	    }
	    else
	    {
		ihalf = -ipattt / 2;
		ihwpat [numpat+1] = ihalf;
		ihwpat [numpat+2] = 2;
		ihwpat [numpat+3] = ihalf;
		numpat += 3;
		dash = G_TRUE;
	    }

	    i++;
	}

	strcpy ( buff, " [ " );
	for ( i = 0; i <= numpat; i++ )
	{
	    sprintf ( ssss, "%6d ", ihwpat[i] );
	    strcat ( buff, ssss );
	}

	strcat ( buff, "] 0 setdash\n" );
	lenb = strlen ( buff );
	cfl_writ ( flun, lenb, (unsigned char *)buff, &ier );

}
