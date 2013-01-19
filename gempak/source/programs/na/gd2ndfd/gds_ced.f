	SUBROUTINE GDS_CED  ( rnvblk, nnv, nbytes, cgds, iret )
C************************************************************************
C* GDS_CED								*
C*									*
C* This subroutine uses the GEMPAK grid navigation for a CED grid to	*
C* generate a GRIB GDS section for this grid.				*
C*									*
C* GDS_CED  ( RNVBLK, NNV, NBYTES, CGDS, IRET )				*
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
C*					-68 = rotated CED not supported *
C**									*
C* Log:									*
C* K. Brill/HPC		 8/99						*
C* K. Brill/HPC		 2/00	Document GDS byte numbers		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	REAL		rnvblk (nnv)
	CHARACTER*1	cgds (*)
C*
	INTEGER		ibyts (3)
C------------------------------------------------------------------------
	IF ( nbytes .lt. 32 ) THEN
	    iret = -61
	    RETURN
	END IF
	IF ( rnvblk (13) .ne. 0.0 ) THEN
	    iret = -68
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
C
C*	Start filling up the GDS array.
C
	cgds (1) = CHAR (0)
	cgds (2) = CHAR (0)
	cgds (3) = CHAR (32)
	cgds (4) = CHAR (0)
	cgds (5) = CHAR (255)
	cgds (6) = CHAR (0)
	ii = 6
C*
	nb = 2
	CALL GDIGIT ( kx, 256, nb, ibyts, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -62
	    RETURN
	END IF
C*					Set bytes 7-8.
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
C*					Set bytes 9-10.
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
C*					Set bytes 11-13.
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
C*					Set bytes 14-16.
	DO i = 3, 1, -1
	    ii = ii + 1
	    cgds (ii) = CHAR ( ibyts (i) )
	END DO
C*					Set byte 17.
	ii = ii + 1
	cgds (ii) = CHAR (0)
C*
	ilat = IABS ( NINT ( rlat2 * 1000. ) )
	nb = 3
	CALL GDIGIT ( ilat, 256, nb, ibyts, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -66
	    RETURN
	END IF
	IF ( rlat2 .lt. 0 ) ibyts (3) = ibyts (3) + 128
C
C*					Set bytes 18-20.
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
C
C*					Set bytes 21-23.
	DO i = 3, 1, -1
	    ii = ii + 1
	    cgds (ii) = CHAR ( ibyts (i) )
	END DO
	cgds (24) = CHAR (255)
	cgds (25) = CHAR (255)
	cgds (26) = CHAR (255)
	cgds (27) = CHAR (255)
	cgds (28) = CHAR (64)
C*
	cgds (29) = CHAR (0)
	cgds (30) = CHAR (0)
	cgds (31) = CHAR (0)
	cgds (32) = CHAR (0)
C*
	RETURN
	END
