	SUBROUTINE AF_RPHT  ( sjhhh, iret )
C************************************************************************
C* AF_RPHT								*
C*									*
C* This subroutine decodes and stores pressure and height from the	*
C* /JHHH group within a RECCO report.					*
C*									*
C* AF_RPHT  ( SJHHH, IRET )						*
C*									*
C* Input parameters:							*
C*	SJHHH		CHAR*		/JHHH group 			*
C*									*
C* Output parameters:							*
C*	RIVALS (IRPMSL)	REAL		Mean sea level pressure in mb	*
C*	RIVALS (IRVSIG)	REAL		Vertical sounding significance	*
C*	RIVALS (IRPRES)	REAL		Pressure in mb			*
C*	RIVALS (IRHGTM)	REAL		Height in meters		*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		11/96						*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	sjhhh
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
C
C*	Check the length of the input string.
C
	lsjhhh = LEN ( sjhhh )
	IF  ( lsjhhh .ne. 5 )  THEN
	    RETURN
	END IF
C
C*	Check that the first character is a "/".
C
	IF  ( sjhhh (1:1) .ne. '/' )  THEN
	    RETURN
	END IF
C
C*	Convert the encoded values to integer.
C
	CALL ST_INTG  ( sjhhh (2:2), j, ier1 )
	CALL ST_INTG  ( sjhhh (3:5), ihhh, ier2 )
	IF  ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) )  THEN
	    RETURN
	END IF
C
	IF  ( j .eq. 0 )  THEN
C
C*	    Mean sea level pressure was reported in lieu of
C*	    a height.  Decode and store this value.
C
	    IF  ( ihhh .le. 100 )  THEN
		pmsl = FLOAT ( 1000 + ihhh )
	    ELSE
		pmsl = FLOAT ( ihhh )
	    END IF
	    rivals ( irpmsl ) = pmsl
	    RETURN
	END IF
C
C*	Decode the pressure and height.
C
	pres = RMISSD
	hgtm = FLOAT ( ihhh )
C
	IF  ( j .eq. 1 )  THEN
	    pres = 200.0
	    hgtm = hgtm * 10.0
	    IF  ( hgtm .lt. 7000 )  THEN
		hgtm = hgtm + 10000.0
	    END IF
	ELSE IF  ( j .eq. 2 )  THEN
	    pres = 850.0
	    IF  ( hgtm .lt. 900 )  THEN
		hgtm = hgtm + 1000.0
	    END IF
	ELSE IF  ( j .eq. 3 )  THEN
	    pres = 700.0
	    IF  ( hgtm .lt. 500 )  THEN
		hgtm = hgtm + 3000.0
	    ELSE
		hgtm = hgtm + 2000.0
	    END IF
	ELSE IF  ( j .eq. 4 )  THEN
	    pres = 500.0
	    hgtm = hgtm * 10.0
	ELSE IF  ( j .eq. 5 )  THEN
	    pres = 400.0
	    hgtm = hgtm * 10.0
	ELSE IF  ( j .eq. 6 )  THEN
	    pres = 300.0
	    hgtm = hgtm * 10.0
	    IF  ( hgtm .lt. 3000 )  THEN
		hgtm = hgtm + 10000.0
	    END IF
	ELSE IF  ( j .eq. 7 )  THEN
	    pres = 250.0
	    hgtm = hgtm * 10.0
	    IF  ( hgtm .lt. 5000 )  THEN
		hgtm = hgtm + 10000.0
	    END IF
	ELSE IF  ( j .eq. 9 )  THEN
	    pres = 925.0
	END IF
C
	IF  ( .not. ERMISS ( pres ) )  THEN
C
C*	    Store the pressure and height along with a vertical
C*	    sounding significance value.
C
C*	    Vertical sounding significance values are stored in the
C*	    interface format as bit flags in accordance with
C*	    WMO BUFR Table 0 08 001.
C
	    rivals ( irvsig ) = 32.0
	    rivals ( irpres ) = pres
	    rivals ( irhgtm ) = hgtm
	END IF
C*
	RETURN
	END
