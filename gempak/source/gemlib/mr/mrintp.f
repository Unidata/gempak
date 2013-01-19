	SUBROUTINE MR_INTP  ( sclhgt, iztype, nlev, ipt, stndat, iret )
C************************************************************************
C* MR_INTP								*
C*									*
C* This subroutine computes pressure for the significant wind data.	*
C* The data must be ordered according to height before calling this	*
C* subroutine.  The pressure is computed using the scale heights 	*
C* previously computed.  If scale heights are not available (i.e.,	*
C* iztype = 1), the subroutine will compute pressure from the 		*
C* bounding values of height assuming that height varies linearly 	*
C* with the logarithm of pressure.					*
C*									*
C* MR_INTP  ( SCLHGT, IZTYPE, NLEV, IPT, STNDAT, IRET )			*
C*									*
C* Input parameters:							*
C*	SCLHGT ( NLEV )	REAL		Scale height			*
C*	IZTYPE		INTEGER		Type of height interpolation	*
C*					  1 = int wrt log p		*
C*					  2 = moist hydrostatic comp	*
C*					  3 = scaled moist hydro comp	*
C*	NLEV		INTEGER		Number of levels		*
C*	IPT    ( NLEV )	INTEGER		Pointers to ordered data	*
C*									*
C* Input and output parameters:						*
C*	STNDAT (6,NLEV)	REAL		Station data			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/86						*
C* T. Piper/GSC		11/98	Updated prolog				*
C* T. Piper/SAIC	 1/02	Initialized pt & zt, not always set	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C
	REAL		sclhgt (*), stndat (6,*)
	INTEGER		ipt (*)
	LOGICAL		start
C
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Loop through all the levels using the scale height computed
C*	previously.  Start at the top of the sounding.
C
	pt = RMISSD
	zt = RMISSD
	IF ( ( iztype .eq. 2 ) .or. ( iztype .eq. 3 ) ) THEN
	  start = .false.
	  DO  i = nlev, 2, -1
	    p = stndat ( 1, ipt (i) )
	    h = stndat ( 6, ipt (i) )
	    s = sclhgt ( ipt (i) )
	    IF  ( (.not. ERMISS (p) ) .and. (.not. ERMISS (h) ) .and. 
     +		  (.not. ERMISS (s) ) )  THEN
		start = .true.
		pt    =  p
		st    =  s
		ht    =  h
	      ELSE IF ( start .and. ( .not. ERMISS (h) ) .and.
     +			( ERMISS ( p ) ) ) THEN
C
C*		Reverse the computation for the moist height to compute
C*		pressure.
C
		IF ( .not. ERMISS ( st ) ) THEN
		    stndat (1, ipt (i) ) = pt * EXP ( ( ht - h ) / st )
		END IF
	    END IF
	END DO
C
C*	If scale heights were not available for computation, interpolate
C*	log p linearly with z.
C
	  ELSE
	    i    = 1
	    ilev = 0
	    klev = 0
	    DO WHILE ( i .le. nlev )
C
C*		Check for both pressure and height data at this level.
C
		p = stndat ( 1, ipt (i) )
		z = stndat ( 6, ipt (i) )
		IF ((.not. ERMISS (p) ) .and. (.not. ERMISS (z) )) THEN
		    klev = i
		    pt   = p
		    zt   = z
		END IF
C
C*		If two bounding levels have been found, interpolate 
C*		missing values of p.
C
		IF ( (ilev .ne. 0 ) .and. (klev .ne. 0) ) THEN
		    DO  j = ilev+1, klev-1
			z = stndat ( 6, ipt (j) )
			IF ( .not. ERMISS (z) ) THEN
			    stndat ( 1, ipt (j) ) = pb *
     +				EXP ( ( z - zb ) * ALOG ( pt/ pb ) /
     +				( zt - zb ) )
			END IF
		    END DO
		END IF
C
C*		Change the top level to the bottom level.
C
		ilev = klev
		pb   = pt
		zb   = zt
C
C*		Increment current level.
C
		i = i + 1
	    END DO
	END IF
C*
	RETURN
	END
