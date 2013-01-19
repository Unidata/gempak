	SUBROUTINE ITRIF  ( ix, iy, iret )
C************************************************************************
C* ITRIF								*
C*									*
C* This subroutine fills a triangular shaped region.			*
C*									*
C* ITRIF  ( IX, IY, IRET )						*
C*									*
C* Input parameters:							*
C*	IX (3)		INTEGER		X coordinates in device units	*
C*	IY (3)		INTEGER		Y coordinates in device units	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* K. Brill/NMC		11/91						*
C* K. Brill/NMC		02/92	Added call to IFILL			*
C* S. Jacobs/NCEP	 9/97	Added call to DSFILL to set solid fill	*
C* T. Lee/GSC		 9/97	Included ERROR.PRM			*
C* S. Jacobs/NCEP	 3/98	Changed value of solid fill in DSFILL	*
C************************************************************************
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		ix (*), iy (*)
C*
	REAL		x (3), y (3)	
	INTEGER		ixo (2), iyo (2)
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Just do a simple device fill if it is available.
C
	IF ( filflg ) THEN
	    msvft = mfltyp
	    svfsz = tfilsz
	    CALL DSFILL ( 1.0, 1, fsize, itype, ier )
	    CALL IFILL ( 3, ix, iy, iret )
	    CALL DSFILL ( svfsz, msvft, fsize, itype, ier )
	    RETURN
	END IF
C
C*	Float the vertices.
C
	DO i = 1, 3
	    x (i) = FLOAT ( ix (i) )
	    y (i) = FLOAT ( iy (i) )
	END DO
C
C*	Draw radial fan from each of the three vertices to assure
C*	good coverage.
C
	DO i = 1, 3
C
C*	    Compute small vector displacement along one side.
C
	    a1 = x(3) - x(1)
	    a2 = y(3) - y(1)
	    aa = SQRT ( a1 * a1 + a2 * a2 )
	    ua1 = a1/aa 
	    ua2 = a2/aa
C
C*	    Draw lines at small intervals radiating from opposite vertex.
C
	    ixo (2) = NINT ( x (2) )
	    iyo (2) = NINT ( y (2) )
	    nmax = NINT ( aa ) + 1
	    rua1 = x(1)
	    rua2 = y(1)
	    DO n = 1, nmax
	    	ixo (1) = NINT ( rua1 )
	    	iyo (1) = NINT ( rua2 )
	    	CALL ILINE ( 2, ixo, iyo, ier )
	    	rua1 = rua1 + ua1
	    	rua2 = rua2 + ua2
	    END DO
C
C*	    Switch vertices.
C
	    xsav = x (i)
	    ysav = y (i)
	    ip1 = MOD ( ( i + 1 ), 3 )
	    IF ( ip1 .eq. 0 ) ip1 = 3
	    ip2 = MOD ( ( i + 2 ), 3 )
	    IF ( ip2 .eq. 0 ) ip2 = 3
	    x (i) = x (ip1)
	    y (i) = y (ip1)
	    x (ip1) = x (ip2)
	    y (ip1) = y (ip2)
	    x (ip2) = xsav
	    y (ip2) = ysav
	END DO
C*
	RETURN
	END
