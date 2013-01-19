	SUBROUTINE GDS_LCC  ( navchg, rnvblk, nnv, nbytes, cgds, iret )
C************************************************************************
C* GDS_LCC								*
C*									*
C* This subroutine uses the GEMPAK grid navigation for a Lambert	*
C* conic conformal projection to generate a GRIB GDS section for	*
C* this grid.								*
C*									*
C* GDS_LCC  ( NAVCHG, RNVBLK, NNV, NBYTES, CGDS, IRET )			*
C*									*
C* Input parameters:							*
C*	NAVCHG		LOGICAL		Flag for navigation change	*
C*	RNVBLK (NNV)	REAL		GEMPAK grid navigation block	*
C*	NNV		INTEGER		Size of the navigation block	*
C*									*
C* Input and output parameter:						*
C*	NBYTES		INTEGER		Input: # of bytes available in	*
C*					       CGDS			*
C*					Output: # of bytes filled in	*
C*					       CGDS			*
C*									*
C* Output parameters:							*
C*	CGDS (NBYTES)	CHAR*1		GRIB GDS section		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-61 = not enough GDS bytes	*
C*					-62 = # in i is too large	*
C*					-63 = # in j is too large	*
C*					-64 = latitude 1 is bad		*
C*					-65 = longitude 1 is bad	*
C*					-70 = DX grid incrmnt invalid	*
C*					-71 = DY grid incrmnt invalid	*
C*					-72 = central longitude is bad	*
C*					-73 = true latitudes are bad	*
C**									*
C* Log:									*
C* K. Brill/HPC		 8/99						*
C* K. Brill/HPC		 2/00	Set byte 17 for north rel wind comps	*
C* K. Brill/HPC		 3/00	Set byte 17 using NAVCHG flag		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	REAL		rnvblk (nnv)
	CHARACTER*1	cgds (*)
	LOGICAL		navchg
C*
	INTEGER		ibyts (3)
C------------------------------------------------------------------------
	IF ( nbytes .lt. 42 ) THEN
	    iret = -61
	    RETURN
	END IF
	iret = 0
	nbytes = 42
C
C*	Get information from the navigation block.
C
	kx = NINT ( rnvblk (5) )
	ky = NINT ( rnvblk (6) )
	rlat1 = rnvblk (7)
	rlon1 = rnvblk (8)
	rlat2 = rnvblk (9)
	rlon2 = rnvblk (10)
	phi1  = rnvblk (11)
	rlov  = rnvblk (12)
	phi2  = rnvblk (13)
C
C*	Start filling up the GDS array.
C
	cgds (1) = CHAR (0)
	cgds (2) = CHAR (0)
	cgds (3) = CHAR (42)
	cgds (4) = CHAR (0)
	cgds (5) = CHAR (255)
	cgds (6) = CHAR (3)
	ii = 6
C
C*	Fill bytes 7-8, 9-10.
C
	nb = 2
	CALL GDIGIT ( kx, 256, nb, ibyts, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -62
	    RETURN
	END IF
	DO i = 2, 1, -1
	    ii = ii + 1
	    cgds (ii) = CHAR ( ibyts (i) )
	END DO
	nb = 2
	CALL GDIGIT ( ky, 256, nb, ibyts, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -63
	    RETURN
	END IF
	DO i = 2, 1, -1
	    ii = ii + 1
	    cgds (ii) = CHAR ( ibyts (i) )
	END DO
C
C*	Fill bytes 11-13, 14-16.
C
	ilat = IABS ( NINT ( rlat1 * 1000. ) )
	nb = 3
	CALL GDIGIT ( ilat, 256, nb, ibyts, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -64
	    RETURN
	END IF
	IF ( rlat1 .lt. 0 ) ibyts (3) = ibyts (3) + 128
	DO i = 3, 1, -1
	    ii = ii + 1
	    cgds (ii) = CHAR ( ibyts (i) )
	END DO
	ilon = IABS ( NINT ( rlon1 * 1000. ) )
	nb = 3
	CALL GDIGIT ( ilon, 256, nb, ibyts, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -65
	    RETURN
	END IF
	IF ( rlon1 .lt. 0 ) ibyts (3) = ibyts (3) + 128
	DO i = 3, 1, -1
	    ii = ii + 1
	    cgds (ii) = CHAR ( ibyts (i) )
	END DO
C
C*	Fill byte 17.
C
	ii = ii + 1
	IF ( navchg ) THEN
	    cgds (ii) = CHAR (0)
	ELSE
	    cgds (ii) = CHAR (8)
	END IF
C
C*	Fill bytes 18-20.
C
	ilon = IABS ( NINT ( rlov * 1000. ) )
	nb = 3
	CALL GDIGIT ( ilon, 256, nb, ibyts, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -72
	    RETURN
	END IF
	IF ( rlov .lt. 0 ) ibyts (3) = ibyts (3) + 128
	DO i = 3, 1, -1
	    ii = ii + 1
	    cgds (ii) = CHAR ( ibyts (i) )
	END DO
C
C*	Compute the grid increments.
C
	IF ( ABS ( phi1 ) .gt. ABS ( phi2 ) ) THEN
	    trult1 = phi1
	    trult2 = phi2
	    IF ( phi1 .gt. 0.0 ) THEN
		sign = -1.0
	    ELSE
		sign = 1.0
	    END IF
	ELSE IF ( ABS ( phi2 ) .gt. ABS ( phi1 ) ) THEN
	    trult1 = phi2
	    trult2 = phi1
	    IF ( phi2 .gt. 0.0 ) THEN
		sign = -1.0
	    ELSE
		sign = 1.0
	    END IF
	ELSE
	    trult1 = phi1
	    trult2 = phi2
	    IF ( phi1 .gt. 0.0 ) THEN
		sign = -1.0
	    ELSE
		sign = 1.0
	    END IF
	END IF
	rnx = rnvblk (5)
	rny = rnvblk (6)
	rlat1 = rlat1 * DTR / 2.
	rlat2 = rlat2 * DTR / 2.
	rlon1 = rlon1 * DTR
	rlon2 = rlon2 * DTR
	clon = rlov * DTR
C
C*	Compute the cone constant.
C
	psi1 = HALFPI + sign * phi1 * DTR
	psi2 = HALFPI + sign * phi2 * DTR
	IF ( phi1 .eq. phi2 ) THEN
	    cc = COS ( psi1 )
	ELSE
	    cc = ALOG ( SIN ( psi2 ) / SIN ( psi1 ) )
	    cc = cc /
     +		 ALOG ( TAN ( psi2 / 2. ) / TAN ( psi1 / 2. ) )
	END IF
C*
	re = RADIUS / cc
	tan1 = TAN ( PI4TH + sign * rlat1 ) ** cc
	tan2 = TAN ( PI4TH + sign * rlat2 ) ** cc
	dlon1 = ( rlon1 - clon ) * cc
	dlon2 = ( rlon2 - clon ) * cc
	x1 = re * tan1 * sin ( dlon1 )
	y1 = sign * re * tan1 * cos ( dlon1 )
	x2 = re * tan2 * sin ( dlon2 )
	y2 = sign * re * tan2 * cos ( dlon2 )
	alfa = SIN ( psi1 ) / ( TAN ( psi1 / 2. ) ** cc )
	dx = ( x2 - x1 ) * alfa / ( rnx - 1. )
	dy = ( y2 - y1 ) * alfa / ( rny - 1. )
C
C*	Fill bytes 21-23, 24-26.
C
	idx = NINT ( dx )
	nb = 3
	CALL GDIGIT ( idx, 256, nb, ibyts, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -70
	    RETURN
	END IF
	DO i = 3, 1, -1
	    ii = ii + 1
	    cgds (ii) = CHAR ( ibyts (i) )
	END DO
	idy = NINT ( dy )
	nb = 3
	CALL GDIGIT ( idy, 256, nb, ibyts, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -71
	    RETURN
	END IF
	DO i = 3, 1, -1
	    ii = ii + 1
	    cgds (ii) = CHAR ( ibyts (i) )
	END DO
C
C*	Fill bytes 27 and 28.
C
	IF ( sign .lt. 0.0 ) THEN
	    cgds (27) = CHAR (0)
	ELSE
	    cgds (27) = CHAR (128)
	END IF
	cgds (28) = CHAR (64)
C
C*	Fill bytes 29-31, 32-34 with true latitudes.
C
	ii = 28
	ilat = IABS ( NINT ( trult1 * 1000. ) )
	nb = 3
	CALL GDIGIT ( ilat, 256, nb, ibyts, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -73
	    RETURN
	END IF
	IF ( trult1 .lt. 0 ) ibyts (3) = ibyts (3) + 128
	DO i = 3, 1, -1
	    ii = ii + 1
	    cgds (ii) = CHAR ( ibyts (i) )
	END DO
	ilat = IABS ( NINT ( trult2 * 1000. ) )
	nb = 3
	CALL GDIGIT ( ilat, 256, nb, ibyts, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -73
	    RETURN
	END IF
	IF ( trult2 .lt. 0 ) ibyts (3) = ibyts (3) + 128
	DO i = 3, 1, -1
	    ii = ii + 1
	    cgds (ii) = CHAR ( ibyts (i) )
	END DO
C
C*	No bipolar projections are supported.  Fill remaining
C*	bytes with zero.
C
	DO ii = 35, 42
	    cgds (ii) = CHAR (0)
	END DO
C*
	RETURN
	END
