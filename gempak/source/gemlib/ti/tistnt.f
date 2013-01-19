	SUBROUTINE TI_STNT  ( dattim, ihhmm, stntim, iret )
C************************************************************************
C* TI_STNT								*
C*									*
C* This subroutine returns the station observation time given the 	*
C* standard GEMPAK nominal time and the hour and minute of the 		*
C* observation.								*
C*									*
C* TI_STNT  ( DATTIM, IHHMM, STNTIM, IRET )				*
C*									*
C* Input parameters:							*
C*	DATTIM		CHAR*		Nominal time			*
C*	IHHMM		INTEGER		Observation hour/minute		*
C*									*
C* Output parameters:							*
C*	STNTIM		CHAR*		Station time			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -7 = invalid year		*
C*					 -8 = invalid month		*
C*					 -9 = invalid day		*
C*					-10 = invalid hour		*
C*					-11 = invalid minute		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* I. Graffman/RDS	12/87	Added TI_ITOC call			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	dattim, stntim
	INTEGER		idtarr (5)
C------------------------------------------------------------------------
	iret = 0
C
C*	Return assigned time as station time if observation is missing.
C
	IF  ( ihhmm .eq. IMISSD )  THEN
	    stntim = dattim
	    RETURN
	END IF
C
C*	Changed assigned time to integers.
C
	CALL TI_CTOI  (  dattim, idtarr, iret )
	IF (iret .ne. 0) RETURN
C
C*	Find assigned hour and observation hour.
C
	ihour = idtarr (4)
	nhour = ihhmm / 100
C
C*	Check to see if observation was taken the day before the 
C*	assigned time.
C
	IF  ( ( ihour .lt. 5 )  .and.  ( nhour .ge. 20 ) )  THEN
C
C*	    Subtract a day.
C
	    CALL TI_SUBD  ( idtarr, idtarr, ier )
	END IF
C
C*	Compute station time from the two parts.
C
	idtarr (4) = nhour
	idtarr (5) = MOD (ihhmm, 100)
	CALL TI_ITOC  ( idtarr, stntim, ier )
C*
	RETURN
	END
