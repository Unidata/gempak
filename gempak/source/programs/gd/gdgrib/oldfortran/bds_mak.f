	SUBROUTINE BDS_MAK  ( igx, igy, precsn, grid, nbyts, cbds,
     +			      ip10sc, misval, iret )
C************************************************************************
C* BDS_MAK								*
C*									*
C* This subroutine makes the GRIB BDS from an input grid of data.	*
C*									*
C* PRECSN contains the user input for the required precision.  It may	*
C* be entered as B/n, where n is the power of 2 to which data is to be	*
C* rounded, or D/r, where r is the number of decimal significant digits	*
C* to be preserved.							*
C*									*
C* BDS_MAK ( IGX, IGY, PRECSN, GRID, NBYTS, CBDS, IP10SC, MISVAL, IRET )*
C*									*
C* Input parameters:							*
C*	IGX		INTEGER		Number of points in x dir	*
C*	IGY		INTEGER		Number of points in y dir	*
C*	PRECSN		CHAR*		Precision parm			*
C*									*
C* Input and output parameters:						*
C*	GRID (IGX,IGY)	REAL		Input: gridded data values	*
C*					Output: values * 10 ** IP10SC	*
C*	NBYTS		INTEGER		Input:  Max # of bytes for BDS	*
C*					Output: # of bytes in BDS	*
C* Output parameters:							*
C*	CBDS (NBYTS)	CHAR*1		Binary data section		*
C*	IP10SC		INTEGER		Power of 10 scaling (for PDS)	*
C*	MISVAL		LOGICAL		Flag for missing data on grid	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-21 = BDS section too long	*
C*					-24 = invalid binary scaling	*
C*					-25 = cannot compute reference  *
C*					-26 = BDS array size too small  *
C*					-27 = nbit calculation failed	*
C*					-28 = all data is missing	*
C**									*
C* Log:									*
C* K. Brill/HPC		08/99						*	
C* K. Brill/HPC		 9/99	Check for all missing data		*
C* K. Brill/HPC		 3/00	Write out warning for default precision *
C* K. Brill/HPC		 5/00	Allow bit maps for constant grids	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	precsn
	REAL		grid  (*)
	CHARACTER*1	cbds (*)
	LOGICAL		misval
C*
	CHARACTER	cprc*16
	CHARACTER*8	part (2)
	INTEGER		ibyts (3)
	CHARACTER*1	cref (4)
	EQUIVALENCE	( cref, rref )
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	misval = .false.
	kxky = igx * igy
	mxln = nbyts - 11
C
C*	Read through the grid finding the minimum and maximum values.
C
	qmin = 1.0E+31
	qmax = -1.0E+31
	icntms = 0
	DO  i = 1, kxky
	    IF  ( .not. ERMISS ( grid (i) ) )  THEN
		IF  ( grid (i) .lt. qmin )  qmin = grid (i)
		IF  ( grid (i) .gt. qmax )  qmax = grid (i)
	    ELSE
		icntms = icntms + 1
	    END IF
	END DO
	IF ( icntms .eq. kxky ) THEN
	    iret = -28
	    RETURN
	END IF
	IF ( qmax .eq. qmin ) THEN
C
C*	    Constant field.
C
	    nbits = 0
	    ip10sc = 0
	    num0 = 8
	    ip2scl = 0
	    nbyts = 12
	    cbds (12) = CHAR (0)
	    misval = ( icntms .gt. 0 )
	ELSE
C
C*	    Use the precision to compute the number of bits, the power
C*	    of 10 scaling, and the rounded minimum.
C
	    CALL ST_LCUC ( precsn, cprc, ier )
	    CALL ST_CLST ( cprc, '/', ' ', 2, part, num, ier )
	    IF ( part (1) .eq. 'D' ) THEN
		CALL ST_CRNM ( part (2), prec, ier )
		IF ( ERMISS ( prec ) ) prec = 4.0
	    ELSE IF ( part (1) .eq. 'B' ) THEN
		CALL ST_NUMB ( part (2), iprec, ier )
		IF ( iprec .eq. IMISSD ) THEN
		    prec = 5.0
		ELSE
		    IF ( iprec .gt. 0 ) THEN
			prec = FLOAT ( 50 + iprec )
		    ELSE
			prec = FLOAT ( iprec )
		    END IF
		END IF
	    ELSE
		WRITE (6,*)
     +		  ' WARNING -- Default precision D/5 used.'
		prec = 5.0
	    END IF
C*
	    CALL BDS_NBT ( qmin, qmax, prec, nbits, ip10sc, rmn,
     +			   iret )
	    IF ( iret .ne. 0 ) THEN
		iret = -27
		RETURN
	    END IF
C
C*	    Apply power of 10 scaling.
C
	    factor = 10. ** ip10sc
	    qmin = rmn * factor
	    qmax = qmax * factor
	    DO  i = 1, kxky
		IF  ( .not. ERMISS ( grid (i) ) )  THEN
		    grid (i) = grid (i) * factor
		END IF
	    END DO
C
C*	    Get the lenth for the BDS.
C
	    n = nbits * ( kxky - icntms )
	    ncdat = n / 8
	    IF ( MOD (n,8) .ne. 0 ) ncdat = ncdat + 1
	    num0 = ncdat * 8 - n
	    ilen = ncdat + 11
	    IF ( MOD ( ilen, 2 ) .ne. 0 ) THEN
		ilen = ilen + 1
		num0 = num0 + 8
		ncdat = ncdat + 1
	    END IF
	    IF ( ilen .gt. nbyts ) THEN
		iret = -26
		RETURN
	    END IF
	    nbyts = ilen
	    misval = ( icntms .gt. 0 )
C
C*	    Initialize entire BDS.
C
	    DO i = 1, nbyts
		cbds (i) = CHAR (0)
	    END DO
C
C*	    Pack the scaled gridded data.
C
	    lendat = mxln
	    CALL BDS_PGB  ( grid, igx, igy, qmin, qmax, nbits,
     +			    lendat, cbds (12), ip2scl, iret )
	    IF ( iret .ne. 0 ) RETURN
	END IF
C
C*	Now set the BDS header byte values.
C
	ii = 0
	nb = 3
	CALL GDIGIT ( nbyts, 256, nb, ibyts, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -21
	    RETURN
	ELSE
	    DO i = 3, 1, -1
		ii = ii + 1
		cbds (ii) = CHAR ( ibyts (i) )
	    END DO
	END IF
C
C*	Set # of unused bits in octet 4.
C
	ii = ii + 1
	cbds (ii) = CHAR ( num0 )
C
C*	Set binary scale factor in octets 5-6.
C
	ival = IABS ( ip2scl )
	nb = 2
	CALL GDIGIT ( ival, 256, nb, ibyts, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -24
	    RETURN
	ELSE
	    IF ( ip2scl .lt. 0 ) ibyts (2) = ibyts (2) + 128
	    DO i = 2, 1, -1
		ii = ii + 1
		cbds (ii) = CHAR ( ibyts (i) )
	    END DO
	END IF
C
C*	Compute and load the reference value.
C
	CALL BDS_IBM ( qmin, rref, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -25
	    RETURN
	ELSE
	    DO i = 1, 4
		ii = ii + 1
		cbds (ii) = cref (i)
	    END DO
	END IF
C
C*	Load in the number of bits used to pack data.
C
	ii = ii + 1
	cbds (ii) = CHAR ( nbits )
C*
	RETURN
	END
