	SUBROUTINE DC_RTIM  ( irundt, irptdy, irpthr, irptmn, ndyb,
     +			      irptdt, iret )
C************************************************************************
C* DC_RTIM								*
C*									*
C* This subroutine combines a run date-time, report day, report hour,	*
C* and report minute into a report date-time.  The report day must be	*
C* within NDYB days before the run day or one day after the run day.	*
C* The maximum allowable value for NDYB is 25.				*
C*									*
C* DC_RTIM  ( IRUNDT, IRPTDY, IRPTHR, IRPTMN, NDYB, IRPTDT, IRET )	*
C*									*
C* Input parameters:							*
C*	IRUNDT (*)	INTEGER		Run date-time(YYYY,MM,DD,HH,MM)	*
C*	IRPTDY		INTEGER		Report day 			*
C*	IRPTHR		INTEGER		Report hour 			*
C*	IRPTMN		INTEGER		Report minute 			*
C*	NDYB 		INTEGER		Maximum number of days before	*
C*                                      run day that report day can be	*
C*									*
C* Output parameters:							*
C*	IRPTDT (*)	INTEGER	Report date-time(YYYY,MM,DD,HH,MM) 	*
C*	IRET		INTEGER		Return code 			*
C*					  0 = Normal return 		*
C*					 -1 = Could not compute the	*
C*					      report date-time 		*
C**									*
C* Log:									*
C* J. Ator/NCEP		03/96						*
C* R. Hollern/NCEP      06/96   Added logic to compute report date/time *
C*                              for reports more than 24 hours old      *
C* J. Ator/NCEP		06/96	Corrected logic for reports more than	*
C*				one day old but which are in same month	*
C*				as run day, and restricted NDYB <= 25 	*
C* J. Ator/NCEP		07/96	Renamed from TI_RTIM 			*
C* J. Ator/NCEP		09/96	Added check for bad return codes from	*
C*				TI_SUBD and TI_ADDD; restructured logic *
C* D. Kidwell/NCEP	04/97	Changed name from UT_RTIM, cleaned up   *
C* D. Kidwell/NCEP	10/97	Cleaned up                              *
C* D. Kidwell/NCEP	12/97	Renamed from MA_RTIM                    *
C************************************************************************
	INTEGER		irundt (*), irptdt (*)
C*
	INTEGER		iwrkdt (5)
C*
	PARAMETER	( MXDYB = 25 )
C*---------------------------------------------------------------------
	iret = -1
C
C*	Store default values for the report date-time.
C
	irptdt ( 1 ) = irundt ( 1 )
	irptdt ( 2 ) = irundt ( 2 )
	irptdt ( 3 ) = irptdy
	irptdt ( 4 ) = irpthr
	irptdt ( 5 ) = irptmn
C
	IF ( irundt ( 3 ) .eq. irptdy ) THEN
C
C*	    The report day and run day are the same, so no further
C*	    action is required.
C
	    iret = 0
	    RETURN
	END IF
C
C*	Subtract as many as MXDYB days from the run date-time.
C*	For each resulting date-time that is computed, check if the
C*	day matches the report day and, if so, then use the resulting
C*	year and month as the report year and month.
C
	DO i = 1, 5
	    iwrkdt ( i ) = irundt ( i )
	END DO
C
	IF ( ndyb .lt. MXDYB ) THEN
	    nb = ndyb
	  ELSE
	    nb = MXDYB 
	END IF
C
        DO ii = 1, nb 
	    CALL TI_SUBD  ( iwrkdt, iwrkdt, iersbd )
	    IF ( iersbd .ne. 0 ) THEN
		RETURN
	      ELSE IF ( iwrkdt ( 3 ) .eq. irptdy ) THEN
		irptdt ( 1 ) = iwrkdt ( 1 )
		irptdt ( 2 ) = iwrkdt ( 2 )
		iret = 0
		RETURN
	    END IF
	END DO
C
C*	Add a day to the run date-time and check if the day from
C*	the resulting date-time matches the report day.  If so,
C*	then use the resulting year and month as the report year
C*	and month. 
C
	DO i = 1, 5
	    iwrkdt ( i ) = irundt ( i )
	END DO
C 
	CALL TI_ADDD  ( iwrkdt, iwrkdt, ieradd )
	IF ( ieradd .ne. 0 ) THEN
	    RETURN
	  ELSE IF ( iwrkdt ( 3 ) .eq. irptdy ) THEN
	    irptdt ( 1 ) = iwrkdt ( 1 )
	    irptdt ( 2 ) = iwrkdt ( 2 )
	    iret = 0
	    RETURN
	END IF
C*
	RETURN
	END
