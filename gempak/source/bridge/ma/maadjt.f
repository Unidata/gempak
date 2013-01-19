	SUBROUTINE MA_ADJT  ( iotarr, iret )
C************************************************************************
C* MA_ADJT								*
C*									*
C* This subroutine adjusts the time to the preceding 6-hourly interval.	*
C*									*
C* MA_ADJT  ( IOTARR, IRET )						*
C*									*
C* Input parameters:							*
C*	None								*
C*									*
C* Output parameters:							*
C*	IOTARR (5)	INTEGER		Obs time to preceding 6-hr int. *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 5/00	From RU_ADJT                            *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'macmn.cmn'
C*
	INTEGER		iotarr (*)
C*
	INTEGER		newarr (5)
C*
	EQUIVALENCE	( newarr (4), nhour ), ( newarr (5), nmin  )
C------------------------------------------------------------------------
	iret  = 0
C
C*	Move time into internal array whose last two elements are
C*      equivalenced to variables nhour and nmin.
C
	DO  i = 1, 5
	    newarr (i) = irptdt (i)
	END DO
C
C*	Adjust the time to the preceding 6-hourly interval.
C
	idiff = MOD ( nhour, 6 )
	nhour = ( nhour / 6 ) * 6
	IF  ( idiff .eq. 5 )  THEN
	    IF ( nmin .ge. 45 ) nhour = nhour + 6
	END IF
	IF ( nhour .ge. 24 ) THEN
	    nhour = 0
	    CALL TI_ADDD  ( newarr, newarr, ier )
	END IF
C
	nmin = 0
	DO  i = 1, 5
	    iotarr (i) = newarr (i)
	END DO
C*
	RETURN
	END
