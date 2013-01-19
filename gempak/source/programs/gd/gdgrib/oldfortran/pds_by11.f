	SUBROUTINE PDS_BY11  ( level, iscale, byte10, ibyt10,
     +				byte11, ibyt11,
     +				byte12, ibyt12, iret )
C************************************************************************
C* PDS_BY11								*
C*									*
C* This subroutine uses the GEMPAK LEVEL contents to set PDS bytes 11	*
C* and 12.								*
C*									*
C* PDS_BY11  ( LEVEL, ISCALE, BYTE10, IBYT10, BYTE11, IBYT11, BYTE12, 	*
C*							IBYT12, IRET )	*
C*									*
C* Input parameters:							*
C*	LEVEL (2)	INTEGER		GEMPAK level			*
C*									*
C* Input and output parameters:						*
C*	ISCALE		INTEGER		Power of 10 scaling from GEMPAK	*
C*	BYTE10		CHAR*1		CHAR vertical coordinate type	*
C*	IBYT10		INTEGER		INT vertical coordinate type	*
C*									*
C* Output parameters:							*
C*	BYTE11		CHAR*1		Byte with GRIB LEVEL 1		*
C*	IBYT11		INTEGER		Integer value of byte 11	*
C*	BYTE12		CHAR*1		Byte with GRIB LEVEL 2		*
C*	IBYT12		INTEGER		Integer value of byte 12	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-87 = level value too large	*
C*					-88 = level is less than zero	*
C**									*
C* Log:									*
C* K. Brill/HPC		 9/99						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	CHARACTER*(*)	byte10, byte11, byte12
	INTEGER		level (2)
C*
C------------------------------------------------------------------------
	iret = 0
	byte11 = CHAR ( 255 )
	ibyt11 = 255
	byte12 = CHAR ( 255 )
	ibyt12 = 255
	IF ( level (2) .eq. -1 ) THEN
C
C*	    Only one level to store in both bytes.
C
	    lvl = level (1) / ( 10 ** iscale )
	    IF ( lvl .lt. 0 ) THEN
		iret = -88
		RETURN
	    END IF
	    ib2 = MOD ( lvl, 256 )
	    ib1 = lvl / 256
	    IF ( ib2 .gt. 255 ) THEN
		iret = -87
		RETURN
	    ELSE
		ibyt11 = ib1
		ibyt12 = ib2
		byte11 = CHAR ( ibyt11 )
		byte12 = CHAR ( ibyt12 )
	    END IF
	ELSE
C
C*	    Reset vertical coordinate type for two-level type.
C
	    IF ( ibyt10 .eq. 100 ) THEN
		ibyt10 = 101
		byte10 = CHAR ( ibyt10 )
		iscale = 1
	    ELSE IF ( ibyt10 .eq. 103 ) THEN
		ibyt10 = 104
		byte10 = CHAR ( ibyt10 )
		iscale = 2
	    ELSE IF ( ibyt10 .eq. 105 ) THEN
		ibyt10 = 106
		byte10 = CHAR ( ibyt10 )
		iscale = 2
	    ELSE IF ( ibyt10 .eq. 107 ) THEN
		ibyt10 = 108
		byte10 = CHAR ( ibyt10 )
		iscale = 2
	    ELSE IF ( ibyt10 .eq. 109 ) THEN
		ibyt10 = 110
		byte10 = CHAR ( ibyt10 )
		iscale = 0
	    ELSE IF ( ibyt10 .eq. 111 ) THEN
		ibyt10 = 112
		byte10 = CHAR ( ibyt10 )
		iscale = 0
	    ELSE IF ( ibyt10 .eq. 113 ) THEN
		ibyt10 = 114
		byte10 = CHAR ( ibyt10 )
		iscale = 0
	    ELSE IF ( ibyt10 .eq. 128 ) THEN
		iscale = 1
	    END IF
	    lvl = level (1) / ( 10 ** iscale )
	    IF ( lvl .lt. 0 ) THEN
		iret = -88
		RETURN
	    END IF
	    IF ( lvl .gt. 255 ) THEN
		iret = -87
		RETURN
	    ELSE
		ibyt11 = lvl
		byte11 = CHAR ( ibyt11 )
	    END IF
	    lvl = level (2) / ( 10 ** iscale )
	    IF ( lvl .lt. 0 ) THEN
		iret = -88
		RETURN
	    END IF
	    IF ( lvl .gt. 255 ) THEN
		iret = -87
		RETURN
	    ELSE
		ibyt12 = lvl
		byte12 = CHAR ( ibyt12 )
	    END IF
	END IF
C*
	RETURN
	END
