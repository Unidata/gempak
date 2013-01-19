	SUBROUTINE VC_NBTS  ( adat, nxy, mxsgdg, nmbts, iret )
C************************************************************************
C* VC_NBTS								*
C*									*
C* This subroutine computes the number of packing bits given the	*
C* maximum number of significant digits to preserve.			*
C*									*
C* VC_NBTS ( ADAT, NXY, MXSGDG, NMBTS, IRET )				*
C*									*
C* Input parameters:							*
C*	ADAT (NXY)	REAL		Array of data values		*
C*	NXY		INTEGER		Number of data values		*
C*	MXSGDG		INTEGER		Maximum # of significant digits	*
C*									*
C* Output parameters:							*
C*	NMBTS		INTEGER		Number of bits for packing	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 +1 = all missing		*
C**									*
C* Log:									*
C* K. Brill/NMC		06/92						*
C* K. Brill/NMC		08/92	Add all missing check			*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	REAL		adat (*)
C*
	INCLUDE		'ERMISS.FNC'
	DATA		rln2/0.69314718/
C-----------------------------------------------------------------------
	iret = 0
	icnt = 0
C
C*	Find the maximum and minimum.
C
	amax = -1.e33
	amin =  1.e33
	DO i = 1, nxy
	    IF ( .not. ERMISS ( adat (i) ) ) THEN
		IF ( adat (i) .gt. amax ) amax = adat (i)
		IF ( adat (i) .lt. amin ) amin = adat (i)
	    ELSE
		icnt = icnt + 1
	    END IF
	END DO
	IF ( icnt .eq. nxy ) THEN
	   nmbts = 8
	   iret = +1
	   RETURN
	END IF
	range = amax - amin
	IF ( range .le. 0.00 ) THEN
	    nmbts = 8
	    RETURN
	END IF
	ipo = INT ( ALOG10 ( range ) )
	IF ( range .lt. 1.00 ) ipo = ipo - 1
	ipo = ipo - mxsgdg + 1
	rr = range * 10. ** ( -ipo )
	nmbts = INT ( ALOG ( rr ) / rln2 ) + 1
C*
	RETURN
	END
