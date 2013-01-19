	SUBROUTINE VI_MXMN  ( vc, vcv, nxy, rmx, rmn, iret )
C************************************************************************
C* VI_MXMN								*
C*									*
C* This subroutine finds the vertical coordinate value at highest	*
C* geometric height (RMX) and lowest geometric height (RMN) among	*
C* the values in VCV.							*
C*									*
C* VI_MXMN ( VC, VCV, NXY, RMX, RMN, IRET )				*
C*									*
C* Input parameters:							*
C*	VC		CHAR*4		Vertical coordinate name	*
C*	VCV (NXY)	REAL		Coordinate values		*
C*	NXY		INTEGER		Number of grid points		*
C*									*
C* Output parameters:							*
C*	RMX		REAL		Value at hightest geom. ht.	*
C*	RMN		REAL		Value at lowest geom. ht.	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 +1 = all missing		*
C**									*
C* Log:									*
C* K. Brill/NMC		07/92						*
C* K. Brill/NMC		08/92	Add all missing check			*
C* K. Brill/NMC		02/93	Change -1 to +1, the all missing code	*
C************************************************************************
	INCLUDE		'viprm.prm'
C*
	REAL		vcv (*)
	CHARACTER*4	vc
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
	icnt = 0
C
C*	Find the maximum and minimum.
C
	amax = -1.e33
	amin =  1.e33
	DO i = 1, nxy
	    IF ( .not. ERMISS ( vcv (i) ) ) THEN
		IF ( vcv (i) .gt. amax ) amax = vcv (i)
		IF ( vcv (i) .lt. amin ) amin = vcv (i)
	    ELSE
		icnt = icnt + 1
	    END IF
	END DO
	IF ( icnt .eq. nxy ) THEN
	    iret = +1
	    rmx = RMISSD
	    rmn = RMISSD
	    RETURN
	END IF
C*
	IF ( vc .eq. NSGMA .or. vc .eq. NETA .or.
     +	     vc .eq. NPRES ) THEN
	    rmx = amin
	    rmn = amax
	ELSE
	    rmx = amax
	    rmn = amin
	END IF
C*
	RETURN
	END
