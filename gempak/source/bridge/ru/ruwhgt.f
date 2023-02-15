	SUBROUTINE RU_WHGT  ( field, nwnd, hght, iret )
C************************************************************************
C* RU_WHGT								*
C*									*
C* This subroutine decodes the height from a significant wind field.	*
C* The field is of the form  atuuu  where a is 1 if the height is 	*
C* above 100000 feet and 9 or 8 otherwise.  t  is the ten-thousands	*
C* digit of the height and the u's are up to three thousands-of-feet	*
C* fields.  The heights are converted from feet to meters.		*
C*									*
C* RU_WHGT  ( FIELD, NWND, HGHT, IRET )					*
C*									*
C* Input parameters:							*
C*	FIELD		CHAR*		Height field			*
C*									*
C* Output parameters:							*
C*	NWND		INTEGER		Number of height fields		*
C*	HGHT (NWND)	REAL		Height in meters		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/86						*
C* S. Guan   NCEP/NCO	11/22		Improve to handle all missing	*
C*					data case.			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	field
	REAL		hght (*)
C------------------------------------------------------------------------
	iret = 0
	nwnd = 0
C
C*	Check that the first character of the field is '9' or '1'.
C
	IF (( field (1:1) .eq. '9' ) .or. ( field (1:1) .eq. '8')) THEN
	    iadd = 0
	  ELSE IF  ( field (1:1) .eq. '1' ) THEN
	    iadd = 100000
	  ELSE
	    RETURN
	END IF
C
C*	Convert the tens digit to an integer.
C
	CALL ST_INTG ( field (2:2), iten, ier )
	IF  ( ier .eq. 0 ) THEN
	    iten = iten * 10
	  ELSE
	    RETURN
	END IF
C
C*	Convert the unit digits to integers and compute height.
C
	DO WHILE ( ( nwnd .lt. 3 ) )
	    i = nwnd + 3
	    CALL ST_INTG ( field (i:i), iunit, ier )
	    nwnd = nwnd + 1
	    IF  ( ier .eq. 0 ) THEN
	        z    = ( (iten + iunit) * 1000 + iadd )
	        hght ( nwnd ) = PR_HGFM ( z )
	    ELSE
	        hght ( nwnd ) = RMISSD
	    END IF
	END DO
C
C*	Determine nwnd.
C
	DO  i = 5, 3, -1
	    CALL ST_INTG ( field (i:i), iunit, ier )
	    IF  ( ier .eq. 0 ) THEN
	        nwnd = i - 2
	        RETURN
	    END IF
	END DO
	nwnd = 0

	RETURN
	END
