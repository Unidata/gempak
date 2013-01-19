	SUBROUTINE AF_DUPE  ( lunf, rpid, dattim, xlat, xlon, xelv,
     +				iret )
C************************************************************************
C* AF_DUPE								*
C*									*
C* This routine will check for a duplicate station entry in the ship	*
C* format data file. A duplicate station entry is a report at the same	*
C* lat/lon location and flight level (station elevation). 		*
C*									*
C* AF_DUPE  ( LUNF, RPID, DATTIM, XLAT, XLON, XELV, IRET )		*
C*									*
C* Input parameters:							*
C*	LUNF		INTEGER		Surface file number		*
C*	RPID		CHAR*8		Station identifier		*
C*	DATTIM		CHAR*		GEMPAK time			*
C*	XLAT		REAL		Station latitude		*
C*	XLON		REAL		Station longitude		*
C*	XELV		REAL		Station elevation		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					   1 = duplicate found		*
C**									*
C* Log:									*
C* S. Chiswell/Unidata	 5/01	Initial development			*
C* S. Danz/AWC	 	 4/04	Changed epsilon to 0.011 to catch round *
C*				off in values coming back from the db   *
C************************************************************************
	CHARACTER*(*)	rpid, dattim
	REAL		xlat, xlon, xelv
	INTEGER		lunf, iret
C
	INTEGER		isdupe
	CHARACTER	stid*9, stn*8, area*12
C------------------------------------------------------------------------
	iret = 0
	isdupe = 0
C
C*	See if this time already exists in the output file
C
	CALL SF_STIM ( lunf, dattim, ier )
	IF ( ier .ne. 0 ) RETURN
C
	stid = '@' // rpid
	area = ' '
	CALL SF_UARE ( lunf, stid, .true., area, stn, ier )

	DO WHILE ( ( ier .eq. 0 ) .and. ( isdupe .eq. 0 ) )
	   
	   CALL SF_SNXT ( lunf, stn, istnm, slat, slon,
     +			selv, ispri, ier )
	   IF ( ier .eq. 0) THEN
	      dlat = abs(slat - xlat)
	      dlon = abs(slon - xlon)
	      delv = abs(selv - xelv)
	      IF(( dlat .lt. 0.011 ) .and. ( dlon .lt. 0.011 ) .and.
     +		 ( delv .lt. 1 ) ) THEN
		 isdupe = 1
	      END IF
	   END IF
	END DO

	IF ( isdupe .eq. 1 ) THEN
	   iret = isdupe
	END IF

	RETURN
	END
