	SUBROUTINE RU_PLVL  ( field, above, level, pres, iret )
C************************************************************************
C* RU_PLVL								*
C*									*
C* This subroutine gets the level number and pressure from a group	*
C* which is in the form LLPPP.  LL must be the same integer, repeated;	*
C* for example, 11 corresponds to level 1.				*
C*									*
C* RU_PLVL  ( FIELD, ABOVE, LEVEL, PRES, IRET )				*
C*									*
C* Input parameters:							*
C*	FIELD		CHAR*		Input field			*
C*	ABOVE		LOGICAL		Above 100 mb flag		*
C*									*
C* Output parameters:							*
C*	LEVEL		INTEGER		Level number			*
C*					 -1 = level not found		*
C*					  0 = valid surface level	*
C*					  1 - 9 = valid levels		*
C*	PRES		REAL		Pressure			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	LOGICAL		above
	CHARACTER*(*)	field
	CHARACTER	clev(10)*2 
	CHARACTER	cc*2
	DATA		clev  / '00','11','22','33','44','55','66',
     +				     '77','88','99' /
C------------------------------------------------------------------------
	iret  = 0
	level = -1
	pres  = RMISSD
C
C*	Check first two character for level number.
C
	cc = field ( 1:2 )
	DO  i = 1, 10
	    IF ( cc .eq. clev (i) ) level = i - 1
	END DO
C
C*	If a level was found, decode the pressure.
C
	IF  ( level .ne. -1 )  THEN
	    CALL ST_INTG  ( field (3:5), ipres, ier )
C
C*	    Save the pressure if it could be decoded.
C
	    IF ( ier .eq. 0 ) THEN
C
C*		Pressures above 100 mb are in tenths; below 100 mb are
C*		in units.
C
		IF ( above ) THEN
		    pres = FLOAT ( ipres ) / 10.
		  ELSE
		    pres = FLOAT ( ipres )
		    IF ( pres .lt. 100. ) pres = pres + 1000.
		END IF
C
C*		If the pressure is missing, reset the level to -1.
C
	      ELSE
		level = -1
	    END IF
	END IF
C*
	RETURN
	END
