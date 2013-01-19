#include "dccmn.h"

void dc_gopt (
	char	*defprm,
	char	*defstn,
	char	*dfstn2,
	int	idfadd,
	int	idfmax,
	int	ndfhr1,
	int	ndfhr2,
	int	idfwdh,
	char	cprmfl[],
	char	cstntb[],
	char	cstnt2[],
	int	*nadstn,
	int	*ntimes,
	char	ccrtim[],
	int	*nhrbak,
	int	*txflag,
        int     *crcflag,
	int	*jwndht,
	int	*iret )
/************************************************************************
 * dc_gopt								*
 *                                                                      *
 * This routine gets the command line options and uses them with the	*
 * default values to set the parameters for the decoder.		*
 *                                                                      *
 * dc_gopt ( defprm, defstn, dfstn2, idfadd, idfmax, ndfhr1, ndfhr2,    *
 *	     idfwdh, cprmfl, cstntb, cstnt2, nadstn, ntimes, ccrtim,    *
 *	     nhrbak, txflag, crcflag, jwndht, iret )				*
 *                                                                      *
 * Input parameters:							*
 *	*defprm		char		Default parameter file name	*
 *	*defstn		char		Default station table		*
 *	*dfstn2		char		Default second station table	*
 *	idfadd		int		Default number of additional	*
 *					  stations			*
 *	idfmax		int		Default number of times		*
 *	ndfhr1		int		Default number of hours to	*
 *					  decode for real-time data	*
 *	ndfhr2		int		Default number of hours to	*
 *					  decode for non real-time data	*
 *      idfwdh		int		Default cutoff height for 	*
 *					"close-to-surface" winds	*
 *                                                                      *
 * Output parameters:							*
 *	cprmfl[]	char		Parameter packing file		*
 *	cstntb[]	char		Station table			*
 *	cstnt2[]	char		Second station table		*
 *	*nadstn		int		Number of additional stations	*
 *	*ntimes		int		Number of times			*
 *	ccrtim[]	char		Current time			*
 *	*nhrbak		int		Number of hours to decode	*
 *	*txflag		int		Flag to save text data		*
 *      *crcflag        int             Flag for circular files         *
 *      *jwndht		int		Dropsonde winds cutoff height 	*
 *	*iret		int		Return code			*
 *					   0 = normal return		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Jacobs/NCEP	 6/96						*
 * m.gamazaychikov/SAIC	07/05	Added surface winds cutoff height to CS	*
 * H. Zeng/SAIC		08/05	Added second station table		*
 * L. Hinson/AWC        06/08   Add crcflag for circular files          *
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = 0;

/*
**	Set the parameter packing file.
*/
	if  ( prmfil[0] == CHNULL )
	    strcpy ( cprmfl, defprm );
	else
	    strcpy ( cprmfl, prmfil );

/*
**	Set the station table.
*/
	if  ( stntbl[0] == CHNULL )
	    strcpy ( cstntb, defstn );
	else
	    strcpy ( cstntb, stntbl );

/*
**	Set the second station table.
*/
	if  ( stntb2[0] == CHNULL )
	    strcpy ( cstnt2, dfstn2 );
	else
	    strcpy ( cstnt2, stntb2 );

/*
**	Set the number of additional stations for the output file.
*/
	if  ( iadstn == IMISSD ) 
	    *nadstn = idfadd;
	else
	    *nadstn = iadstn;

/*
**	Set the number of times for the output file.
*/
	if  ( maxtim == IMISSD ) 
	    *ntimes = idfmax;
	else
	    *ntimes = maxtim;

/*
**	Set the current time string.
*/
	strcpy ( ccrtim, curtim );

/*
**	Set the number of hours to decode prior to the current time.
**	If this is real-time processing, use the default value NDFHR1,
**	otherwise use NDFHR2.
*/
	if  ( nhours == IMISSD )
	{
	    if  ( irltim )
		*nhrbak = ndfhr1;
	    else
		*nhrbak = ndfhr2;
	}
	else
	    *nhrbak = nhours;

/*
**	Set the text saving flag.
*/
	*txflag = txtflg;
        *crcflag = circflg;

/*
**	Set the "close-to-surface" cutoff height.
*/
        if ( iwndht == IMISSD ) 
	   *jwndht = idfwdh;
        else
	   *jwndht = iwndht;

}
