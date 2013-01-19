	SUBROUTINE SN_MGHT ( datain, nparms, tmpk, pres, hght,
     +			     npt, iret )
C************************************************************************
C* SN_MGHT								*
C*									*
C* This subroutine finds the pressure and height of the temperature,	*
C* TMPK, from moist-adiabatic method. If the sounding is stable, a	*
C* non-zero value is returned.						*
C*									*
C* SN_MGHT  ( DATAIN, NPARMS, TMPK, PRES, HGHT, NPT, IRET )		*
C*									*
C* Input parameters:							*
C*	DATAIN(NPARMS,*)REAL		Station data			*
C*	NPARMS		INTEGER		Number of parameters		*
C*	TMPK		REAL		Temperature in K		*
C*									*
C* Output parameters:							*
C*	PRES (NPT)	REAL		Pressure in mb			*
C*	HGHT (NPT)	REAL		Height in meters		*
C*	NPT		INTEGER		No of intersections		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 +5 = stable sounding		*
C*					-29 = invalid station data	*
C**									*
C* Log:									*
C* T. Lee/GSC		 4/00	Created					*
C* T. Lee/SAIC		 5/03	Initialized npt				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( JPRE=1, JTMP=2, JDWP=3, JHGT=6 )
	PARAMETER	( MAXSTN=50, NPMS=7, P600=600., P500=500. )
	REAL		datain ( nparms, * ) 
	REAL		pres ( * ), hght ( * )
	REAL		outdat (MMPARM) 
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	npt  = 0
C
C*	Moist-adiabatic cloud height methodology: Find the most
C*	unstable level below 600 mb. Lift the parcel to 500 mb.
C*	If the lifted index is less than 0, (air parcel is warmer
C*	than the environment), continue lifting the parcel
C*	moist-adiabatically until the parcel temperature is cooled 
C*	off to the cloud temperature, TMPK.
C
	tg = 0.
	CALL PS_USTB ( datain, nparms, P600, outdat, ier )
	pp = outdat ( JPRE )
	tt = outdat ( JTMP )
	td = outdat ( JDWP )
	te = PR_THTE ( pp, tt, td )
	tmst = PR_TMST ( te, P500, tg )
C
C*	Get environmental temperature at 500 mb and compute 
C*	the best lifted index.
C
	CALL PC_CMDT ( 5, 6, 7, P500, 1, datain, outdat, ier )
	t500 = outdat ( JTMP )
	bli  = RMISSD
	IF  ( ERMISS ( t500 ) .or. ERMISS ( tmst ) )  THEN
	    iret = -29
	    RETURN
C	
	  ELSE
	    bli = t500 - PR_TMKC ( tmst )
	END IF
C
C*	Go through moist-adiabatic. If the best lifted index 
C*	is greater than or equal to 0., return with a non-zero
C*	value.
C
	pmst = PR_PMST ( te, tmpk )
C
	CALL PC_CMDT  ( 5, 6, 7, pmst, 1, datain, outdat, iret )
	IF  ( iret .eq. 0 )  THEN
	    npt = 1
	    pres ( npt ) = outdat ( JPRE )
	    hght ( npt ) = outdat ( JHGT )
	    IF  ( bli .ge. 0. ) iret = +5
	  ELSE
	    iret = -29
	END IF
C*
	RETURN
	END
