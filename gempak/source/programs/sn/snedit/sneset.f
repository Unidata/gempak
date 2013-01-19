	SUBROUTINE SNESET  ( isnfln, time, stid, istnm, slat, slon, 
     +			     selv, stname, iret )
C************************************************************************
C* SNESET								*
C*									*
C* This subroutine sets the station and time if necessary for 		*
C* preparation for writing to the sounding dataset			*
C*									*
C* SNESET  ( ISNFLN, TIME, STID, ISTNM, SLAT, SLON, SELV, 		*
C*           STNAME, IRET )						*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	TIME		CHAR*		Nominal station time		*
C*	STID		CHAR*		Station identifier		*
C*	ISTNM		INTEGER		Station number			*
C*	SLAT		REAL		Station latitude		*
C*	SLON		REAL		Station longitude		*
C*	SELV		REAL		Station elevation		*
C* Output parameters:							*
C*	STNAME		CHAR*		Station name			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					-1 = error setting time/station	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/88						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	stid, time, stname
C------------------------------------------------------------------------
	iret   = 0
C
C*	See if time can be set.
C
	CALL SN_STIM  ( isnfln, time, ier )
C
C*	If time is not set, add to file.
C
	IF  ( ier .ne. 0 )  THEN
	    CALL SN_ATIM  ( isnfln, time, ier )
	    IF  ( ier .eq. 0 )  THEN
		CALL SN_STIM  ( isnfln, time, ier )
	      ELSE
		CALL ER_WMSG  ( 'SN', ier, time, ierr )
		iret = -1
		CALL ER_WMSG  ( 'SNEDIT', irett, time, ierr )
		RETURN
	    END IF
	END IF
C
C*	Check for station in file.
C
	IF  ( stid .ne. ' ' )  THEN
	    stname = stid
	  ELSE
	    CALL ST_INCH  ( istnm, stname, ier )
	END IF
	CALL SN_SSTN ( isnfln, stname, stid, istnm, slat, slon, 
     +                 selv, ier )
	IF  ( ier .ne. 0 )  THEN
	    CALL SN_ASTN  ( isnfln, 1, stid, istnm, slat, slon, selv, 
     +			    ' ', ' ', n, ier )
	    IF  ( ier .ne. 0 )  THEN
		CALL ER_WMSG  ( 'SN', ier, stname, ierr )
		irett = -1
		CALL ER_WMSG  ( 'SNEDIT', irett, stname, ierr )
		RETURN
	      ELSE
		CALL SN_SSTN ( isnfln, stname, stid, istnm, slat, slon,
     +			       selv, ier )
	    END IF
	END IF
C*
	RETURN
	END
