	SUBROUTINE BDS_NBT  ( rmin, rmax, rdb, nmbts, iscale, rmn, iret )
C************************************************************************
C* BDS_NBT								*
C*									*
C* This subroutine computes the number of packing bits given the	*
C* maximum number (< 50) of significant digits to preserve or the	*
C* binary precision to store the data.  The binary precision is given	*
C* as zero, a negative integer, or as a postitive integer greater than	*
C* or equal to 50.  If the binary precision is given, ISCALE will	*
C* always be zero in this case.						*
C*									*
C* The binary precision translates as follows:				*
C*     53  =>  store data to nearest 8					*
C*     52  =>  store data to nearest 4					*
C*     51  =>  store data to nearest 2					*
C*     50  =>  store data to nearest 1					*
C*      0  =>  store data to nearest 1					*
C*     -1  =>  store data to nearest 1/2				*
C*     -2  =>  store data to nearest 1/4				*
C*     -3  =>  store data to nearest 1/8				*
C*									*
C* Note that RDB - 50 give the nearest whole power of two for binary	*
C* precision.								*
C*									*
C* Note that a fractional number of significant digits is allowed.	*
C*									*
C* BDS_NBT ( RMIN, RMAX, RDB, NMBTS, ISCALE, RMN, IRET )		*
C*									*
C* Input parameters:							*
C*	RMIN 		REAL		Minimum value			*
C*	RMAX		REAL		Maximum value			*
C*	RDB		REAL		Maximum # of significant digits	*
C*					  OR binary precision if < 0	*
C*									*
C* Output parameters:							*
C*	NMBTS		INTEGER		Number of bits for packing	*
C*	ISCALE		INTEGER		Power of 10 scaling to use	*
C*	RMN		REAL		Rounded miniumum		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC		06/92						*
C* K. Brill/EMC		12/95	Added binary precision; added RMN	*
C* K. Brill/EMC		 1/97	Add .5 in rr= & rng2= for better rnd off*
C* K. Brill/EMC		 1/97	Use 10**iscale in rounding the min	*
C* K. Brill/HPC		 8/99	Name change for use in GDGRIB		*
C************************************************************************
C*
	DATA		rln2/0.69314718/
C-----------------------------------------------------------------------
	iret = 0
	icnt = 0
	iscale = 0
	rmn = rmin
	range = rmax - rmin
	IF ( range .le. 0.00 ) THEN
	    nmbts = 8
	    RETURN
	END IF
C*
	IF ( rdb .gt. 0.0 .and. rdb .lt. 50. ) THEN
	    po = FLOAT ( INT ( ALOG10 ( range ) ) )
	    IF ( range .lt. 1.00 ) po = po - 1.
	    po = po - rdb + 1.
	    iscale = - INT ( po )
	    rr = range * 10. ** ( -po ) + .5
	    nmbts = INT ( ALOG ( rr ) / rln2 ) + 1
	ELSE
	    ibin = NINT ( -rdb )
	    IF ( ibin .le. -50. ) ibin = ibin + 50
	    rng2 = range * 2. ** ibin + .5
	    nmbts = INT ( ALOG ( rng2 ) / rln2 ) + 1
	END IF
	IF ( nmbts .le. 0 ) THEN
	    iret = 1
	    nmbts = 8
	END IF
C
C*	Compute RMN, the first packable value less than or equal to
C*	RMIN.
C
	tp = 10. ** iscale
	x = ( ALOG ( range * tp ) - ALOG ( 2 ** nmbts - 1. ) ) / rln2
	ixp = INT ( x )
	IF ( FLOAT ( ixp ) .ne. x .and. x .gt. 0. ) ixp = ixp + 1
	irmn = NINT ( ( rmin * tp ) / ( 2. ** ixp ) )
	rmn = FLOAT ( irmn ) * ( 2. ** ixp )
	IF ( rmn .gt. rmin * tp ) rmn = rmn - ( 2. ** ixp )
	rmn = rmn / tp
C*
	RETURN
	END
