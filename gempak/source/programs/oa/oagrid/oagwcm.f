	SUBROUTINE OAGWCM  ( filnam, source, gltln, ppp, rlevl,
     +			     time, dscomp, dsunif, number, deltan,
     +			     deltax, deltay, iret )
C************************************************************************
C* OAGWCM								*
C*									*
C* This subroutine stores the computed values in the common /OAGCMN/.	*
C* Rounded values for deltan, deltax and deltay are returned.		*
C*									*
C* OAGWCM  ( FILNAM, SOURCE, GLTLN, PPP, RLEVL, TIME, DSCOMP,		*
C*           DSUNIF, NUMBER, DELTAN, DELTAX, DELTAY, IRET )		*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		File name			*
C*	SOURCE		CHAR*		Data source			*
C*	GLTLN(*)	REAL		Grid lat/lon range		*
C*	PPP		CHAR*		Parameter			*
C*	RLEVL		REAL		Vertical level			*
C*	TIME		CHAR*		Date/time			*
C*	DSCOMP		REAL		Computed station spacing	*
C*	DSUNIF		REAL		Uniform station spacing		*
C*	NUMBER		INTEGER		Number of stations		*
C*									*
C* Output parameters:							*
C*	DELTAN		REAL		Rounded station			*
C*	DELTAX		REAL		Rounded deltax			*
C*	DELTAY		REAL		Rounded deltay			*
C*	IRET		INTEGER		Status				*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/85						*
C* S. Jacobs/NCEP	 4/97	Renamed IP_ULOC to IP_PUTV		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'oagcmn.cmn'
C*
	CHARACTER*(*)	filnam, source, ppp, time
	REAL		gltln (*)
C*
	CHARACTER	ccc*12
C------------------------------------------------------------------------
C*	Save the parameters in the common area.
C
	cmpflg = .true.
	file   = filnam
	src    = source
	parm   = ppp
	rlevel = rlevl
	date   = time
	compds = dscomp
	unifds = dsunif
	ngstn  = number
C
	DO  i = 1, 4
	    area (i) = gltln (i)
	END DO
C
	cdeltn = ( dscomp + dsunif ) / 2.
	cdelty = cdeltn / 2.
	cdeltx = cdelty / COS ( ( (gltln(1) + gltln(3) ) / 2. ) * DTR )
C
	deltan = cdeltn
	deltan = FLOAT ( NINT ( deltan * 100. )) / 100.
	deltay = cdelty
	deltay = FLOAT ( NINT ( deltay * 100. )) / 100.
	deltax = cdelty
	deltax = FLOAT ( NINT ( deltax * 100. )) / 100.
C
C*	Update the values in the local variable block.
C
	CALL ST_RLCH  ( deltan, 2, ccc, ier )
	CALL IP_PUTV  ( 'DELTAN', ccc, ier )
	CALL ST_RLCH  ( deltax, 2, ccc, ier )
	CALL IP_PUTV  ( 'DELTAX', ccc, ier )
	CALL ST_RLCH  ( deltax, 2, ccc, ier )
	CALL IP_PUTV  ( 'DELTAY', ccc, ier )
C*
	iret = 0
C*
	RETURN
	END
