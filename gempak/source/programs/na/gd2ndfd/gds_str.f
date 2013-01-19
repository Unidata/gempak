	SUBROUTINE GDS_STR  ( navchg, rnvblk, nnv, nbytes, cgds, iret )
C************************************************************************
C* GDS_STR								*
C*									*
C* This subroutine uses the GEMPAK grid navigation for an unrotated	*
C* polar stereographic projection to generate a GRIB GDS section for	*
C* this grid.								*
C*									*
C* GDS_STR  ( NAVCHG, RNVBLK, NNV, NBYTES, CGDS, IRET )			*
C*									*
C* Input parameters:							*
C*	NAVCHG		LOGICAL		Navigation change flag		*
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
C*					-69 = rotated STR not supported *
C*					-70 = DX grid incrmnt invalid	*
C*					-71 = DY grid incrmnt invalid	*
C*					-72 = center longitude is bad	*
C**									*
C* Log:									*
C* K. Brill/HPC		 8/99						*
C* K. Brill/HPC		 2/00	Doc byte #'s; Set GDS 17 for north rel	*
C* K. Brill/HPC		 3/00	Set GDS 17 using NAVCHG flag		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	REAL		rnvblk (nnv)
	CHARACTER*1	cgds (*)
	LOGICAL		navchg
C*
	INTEGER		ibyts (3)
C------------------------------------------------------------------------
	IF ( nbytes .lt. 32 ) THEN
	    iret = -61
	    RETURN
	END IF
	IF ( rnvblk (13) .ne. 0.0 .or.
     +       ABS ( 90.0 - ABS ( rnvblk (11) ) ) .gt. .005 ) THEN
	    iret = -69
	    RETURN
	END IF
	iret = 0
	nbytes = 32
C
C*	Get information from the navigation block.
C
	kx = NINT ( rnvblk (5) )
	ky = NINT ( rnvblk (6) )
	rlat1 = rnvblk (7)
	rlon1 = rnvblk (8)
	rlat2 = rnvblk (9)
	rlon2 = rnvblk (10)
	polat = rnvblk (11)
	rlov  = rnvblk (12)
C
C*	Start filling up the GDS array.
C
	cgds (1) = CHAR (0)
	cgds (2) = CHAR (0)
	cgds (3) = CHAR (32)
	cgds (4) = CHAR (0)
	cgds (5) = CHAR (255)
	cgds (6) = CHAR (5)
	ii = 6
C*
	nb = 2
	CALL GDIGIT ( kx, 256, nb, ibyts, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -62
	    RETURN
	END IF
C
C*				Set bytes 7-8.
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
C
C*				Set bytes 9-10.
	DO i = 2, 1, -1
	    ii = ii + 1
	    cgds (ii) = CHAR ( ibyts (i) )
	END DO
C*
	ilat = IABS ( NINT ( rlat1 * 1000. ) )
	nb = 3
	CALL GDIGIT ( ilat, 256, nb, ibyts, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -64
	    RETURN
	END IF
	IF ( rlat1 .lt. 0 ) ibyts (3) = ibyts (3) + 128
C
C*				Set bytes 11-13.
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
C
C*				Set bytes 14-16.
	DO i = 3, 1, -1
	    ii = ii + 1
	    cgds (ii) = CHAR ( ibyts (i) )
	END DO
C
C*				Set byte 17.
	ii = ii + 1
	IF ( navchg ) THEN
	    cgds (ii) = CHAR (0)
	ELSE
	    cgds (ii) = CHAR (8)
	END IF
C*
	ilon = IABS ( NINT ( rlov * 1000. ) )
	nb = 3
	CALL GDIGIT ( ilon, 256, nb, ibyts, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -72
	    RETURN
	END IF
	IF ( rlov .lt. 0 ) ibyts (3) = ibyts (3) + 128
C
C*				Set bytes 18-20.
	DO i = 3, 1, -1
	    ii = ii + 1
	    cgds (ii) = CHAR ( ibyts (i) )
	END DO
C
C*	Compute the grid increments.
C
	IF ( polat .gt. 0.0 ) THEN
	    sign = -1.0
	ELSE
	    sign = 1.0
	END IF
	rnx = rnvblk (5)
	rny = rnvblk (6)
	rlat1 = rlat1 * DTR / 2.
	rlat2 = rlat2 * DTR / 2.
	rlon1 = rlon1 * DTR
	rlon2 = rlon2 * DTR
	clon = rlov * DTR
	re = RADIUS
	tan1 = TAN ( PI4TH + sign * rlat1 )
	tan2 = TAN ( PI4TH + sign * rlat2 )
	dlon1 = rlon1 - clon
	dlon2 = rlon2 - clon
	x1 = re * tan1 * sin ( dlon1 )
	y1 = sign * re * tan1 * cos ( dlon1 )
	x2 = re * tan2 * sin ( dlon2 )
	y2 = sign * re * tan2 * cos ( dlon2 )
	dx = ( x2 - x1 ) * 1.8660254 / ( rnx - 1. )
	dy = ( y2 - y1 ) * 1.8660254 / ( rny - 1. )
	idx = NINT ( dx )
	nb = 3
	CALL GDIGIT ( idx, 256, nb, ibyts, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -70
	    RETURN
	END IF
C
C*				Set bytes 21-23.
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
C
C*				Set bytes 24-26.
	DO i = 3, 1, -1
	    ii = ii + 1
	    cgds (ii) = CHAR ( ibyts (i) )
	END DO
C*
	IF ( polat .gt. 0.0 ) THEN
	    cgds (27) = CHAR (0)
	ELSE
	    cgds (27) = CHAR (128)
	END IF
	cgds (28) = CHAR (64)
C*
	cgds (29) = CHAR (0)
	cgds (30) = CHAR (0)
	cgds (31) = CHAR (0)
	cgds (32) = CHAR (0)
C*
	RETURN
	END
