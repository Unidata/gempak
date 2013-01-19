	SUBROUTINE BMS_MAK  ( grid, igx, igy, nbyts, cbms, iret )
C************************************************************************
C* BMS_MAK								*
C*									*
C* This subroutine makes the GRIB BMS from an input grid of data.	*
C*									*
C* BMS_MAK ( GRID, IGX, IGY, NBYTS, CBMS, IRET )			*
C*									*
C* Input parameters:							*
C*	GRID (IGX,IGY)	REAL		Grid data			*
C*	IGX		INTEGER		Number of points in x dir	*
C*	IGY		INTEGER		Number of points in y dir	*
C*									*
C* Input and output parameters:						*
C*	NBYTS		INTEGER		Input:  Max # of bytes for BMS	*
C*					Output: # of bytes in the BMS	*
C*									*
C* Output parameters:							*
C*	CBMS (NBYTS)	CHAR*1		Binary data section		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-41 = BMS section too long	*
C*					-42 = BMS allocation too small	*
C**									*
C* Log:									*
C* K. Brill/HPC		08/99						*	
C* K. Brill/HPC		 3/00	Avoid character assignment to itself	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		grid  (*)
	CHARACTER*1	cbms (*)
C*
	INTEGER		ibyts (3)
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	kxky = igx * igy
C
C*	Compute the length of the bit map section.
C
	nb = kxky / 8
	IF ( MOD ( kxky, 8 ) .ne. 0 ) nb = nb + 1
	num0 = nb * 8 - kxky
	nb = nb + 6
	IF ( MOD ( nb, 2 ) .ne. 0 ) THEN
	    nb = nb + 1
	    num0 = num0 + 8
	END IF
	IF ( nb .gt. nbyts ) THEN
	    iret = -42
	    RETURN
	ELSE
	    nbyts = nb
	END IF
C
C*	Initialize all bytes to zero.
C
	DO i = 1, nbyts
	    cbms (i) = CHAR (0)
	END DO
C
C*	Set the length in bytes 1--3.
C
	ii = 0
	nb = 3
	CALL GDIGIT ( nbyts, 256, nb, ibyts, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -41
	    RETURN
	ELSE
	    DO i = 3, 1, -1
		ii = ii + 1
		cbms (ii) = CHAR ( ibyts (i) )
	    END DO
	END IF
C
C*	Set the number of unused bits in byte 4.
C
	ii = ii + 1
	cbms (ii) = CHAR ( num0 )
C
C*	Set flag for in-message bit map (bytes 5-6 = 0).
C
	DO i = 1, 2
	    ii = ii + 1
	    cbms (ii) = CHAR (0)
	END DO
C
C*	Now generate the bit map itself.
C
	icnt = 0
	DO i = 1, kxky
	    IF ( MOD ( icnt, 8 ) .eq. 0 ) THEN
		ii = ii + 1
		ipwr = 8
	    END IF
	    ipwr = ipwr - 1
	    IF ( .not. ERMISS ( grid (i) ) ) THEN
		ixx = ICHAR ( cbms (ii) )
		cbms (ii) = CHAR ( ixx + 2 ** ipwr )
	    END IF
	    icnt = icnt + 1
	END DO
C*
	RETURN
	END
