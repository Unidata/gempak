	SUBROUTINE GDS_MER  ( rnvblk, nnv, nbytes, cgds, iret )
C************************************************************************
C* GDS_MER								*
C*									*
C* This subroutine uses the GEMPAK grid navigation for a CED grid to	*
C* generate a GRIB GDS section for this grid.				*
C*									*
C* GDS_MER  ( RNVBLK, NNV, NBYTES, CGDS, IRET )				*
C*									*
C* Input parameters:							*
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
C*					-66 = latitude 2 is bad		*
C*					-67 = longitude 2 is bad	*
C*					-70 = DX grid incrmnt invalid	*
C*					-71 = DY grid incrmnt invalid	*
C*					-74 = rotated MER not supported *
C**									*
C* Log:									*
C* K. Brill/HPC		 8/99						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	REAL		rnvblk (nnv)
	CHARACTER*1	cgds (*)
C*
	INTEGER		ibyts (3)
C------------------------------------------------------------------------
	IF ( nbytes .lt. 42 ) THEN
	    iret = -61
	    RETURN
	END IF
	IF ( rnvblk (13) .ne. 0.0 .or.
     +	     rnvblk (11) .ne. 0.0 ) THEN
	    iret = -74
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
C
C*	Start filling up the GDS array.
C
	cgds (1) = CHAR (0)
	cgds (2) = CHAR (0)
	cgds (3) = CHAR (42)
	cgds (4) = CHAR (0)
	cgds (5) = CHAR (255)
	cgds (6) = CHAR (1)
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
	cgds (ii) = CHAR (128)
C
C*	Fill bytes 18-20, 21-23.
C
	ilat = IABS ( NINT ( rlat2 * 1000. ) )
	nb = 3
	CALL GDIGIT ( ilat, 256, nb, ibyts, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -66
	    RETURN
	END IF
	IF ( rlat2 .lt. 0 ) ibyts (3) = ibyts (3) + 128
	DO i = 3, 1, -1
	    ii = ii + 1
	    cgds (ii) = CHAR ( ibyts (i) )
	END DO
	ilon = IABS ( NINT ( rlon2 * 1000. ) )
	nb = 3
	CALL GDIGIT ( ilon, 256, nb, ibyts, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -67
	    RETURN
	END IF
	IF ( rlon2 .lt. 0 ) ibyts (3) = ibyts (3) + 128
	DO i = 3, 1, -1
	    ii = ii + 1
	    cgds (ii) = CHAR ( ibyts (i) )
	END DO
C
C*	True latitude is always 0.0.
C
	cgds (24) = CHAR (0)
	cgds (25) = CHAR (0)
	cgds (26) = CHAR (0)
C
C*	Byte 27 is reserved.
C*	Byte 28 is the scanning mode flag.
C
	cgds (27) = CHAR (0)
	cgds (28) = CHAR (64)
C
C*	Fill bytes 29-31, 32-34, the increments.
C
	ii = 28
	rnx = rnvblk (5)
	rny = rnvblk (6)
	rlat1 = rlat1 * DTR / 2.
	rlat2 = rlat2 * DTR / 2.
	IF ( rlon2 .lt. rlon1 ) THEN
	    rlon2 = rlon2 + 360.
	END IF
	rlon1 = rlon1 * DTR
	rlon2 = rlon2 * DTR
	x1 = 0.0
	y1 = RADIUS * ALOG ( TAN ( PI4TH + rlat1 ) )
	x2 = RADIUS * ( rlon2 - rlon1 )
	y2 = RADIUS * ALOG ( TAN ( PI4TH + rlat2 ) )
	dx = ( x2 - x1 ) / ( rnx - 1. )
	dy = ( y2 - y1 ) / ( rny - 1. )
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
C*	Bytes 35--42 are reserved.
C
	DO ii = 35, 42
	    cgds (ii) = CHAR (0)
	END DO
C*
	RETURN
	END
