	SUBROUTINE RU_ADJT  ( iotarr, dattim, iret )
C************************************************************************
C* RU_ADJT								*
C*									*
C* This subroutine adjusts the time to the nearest 3-hourly interval.	*
C*									*
C* RU_ADJT  ( IOTARR, DATTIM, IRET )					*
C*									*
C* Input parameters:							*
C*	IOTARR (5)	INTEGER		Observation time		*
C*									*
C* Output parameters:							*
C*	DATTIM		CHAR*		GEMPAK time rounded to 3 hours	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/86						*
C* M. desJardins/GSFC	12/87	Changed TI subroutines			*
C************************************************************************
	CHARACTER*(*)	dattim
	INTEGER		iotarr (*)
C*
	INTEGER		newarr (5)
C*
	EQUIVALENCE	( newarr (1), iyear ), ( newarr (2), imnth ),
     +			( newarr (3), iday  ), ( newarr (4), ihour )
C------------------------------------------------------------------------
	iret  = 0
C
C*	Move time into internal array which is equivalenced to variables
C*	iyear, imnth, iday and ihour.
C
	DO  i = 1, 5
	    newarr (i) = iotarr (i)
	END DO
C
C*	Find the current hour and adjust to nearest 3 hourly interval.
C
	idiff = MOD ( ihour, 3 )
C
C*	If the hour is one hour after 3 hourly time, subtract 1 hour.
C
	IF  ( idiff .eq. 1 )  THEN
	    ihour = ihour - 1
	  ELSE IF  ( idiff .eq. 2 )  THEN
C
C*	    If hour is one hour early, add hour and change day if hour
C*	    is 24.
C
	    ihour = ihour + 1
	    IF  ( ihour .eq. 24 )  THEN
		ihour  = 0
		CALL TI_ADDD  ( newarr, newarr, ier )
	    END IF
	END IF
C
C*	Convert to GEMPAK time.
C
	CALL TI_ITOC  ( newarr, dattim, ier )
C*
	RETURN
	END
