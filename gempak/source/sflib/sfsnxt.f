	SUBROUTINE SF_SNXT  ( isffln, stid, istnm, slat, slon, selv, 
     +			      ispri, iret )
C************************************************************************
C* SF_SNXT								*
C*									*
C* This subroutine selects the next station in a surface file.		*
C* SF_STIM must be called to set the time before this subroutine 	*
C* is called.  Stations to be found can be set in SF_UARE.  Data	*
C* for this station may be read or written by calling SF_RDAT or	*
C* SF_WDAT, respectively.						*
C*									*
C* SF_SNXT  ( ISFFLN, STID, ISTNM, SLAT, SLON, SELV, ISPRI, IRET )	*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*									*
C* Output parameters:							*
C*	STID		CHAR*8		Station identifier		*
C*	ISTNM		INTEGER		Station number			*
C*	SLAT		REAL		Station latitude		*
C*	SLON		REAL		Station longitude		*
C*	SELV		REAL		Station elevation		*
C*	ISPRI		INTEGER		Station priority code           *
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -3 = file not open		*
C*					  -8 = no more stations		*
C*				 	 -17 = time not set		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* M. desJardins/GSFC	 4/90	Single station search			*
C* K. Brill/NMC		 8/93	Changes for 8-char id 			*
C* A. Hardy/GSC          3/99   Changed calling sequence, SF_QSTN       *
C* A. Hardy/GSC          3/99   Removed ispri = 0			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C*
	CHARACTER* (*)	stid 
C*
	INTEGER		iheadr (MMKEY), istid (2)
	CHARACTER	stat*4, coun*4
C------------------------------------------------------------------------
	CALL SF_CHKF  ( isffln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Check for single station search.
C
	IF  ( ( ftmset (isffln) ) .and. ( onestn (isffln) ) )  THEN
C
C*	    Check that this station has not already been returned.
C
	    IF  ( ( onefnd (isffln) ) .and. ( krow (isffln) .ne. 0 )
     +			.and. ( kcol (isffln) .ne. 0 ) )  THEN
		iret = -8
		RETURN
	    END IF
C
C*	    Get information about station.
C
	    CALL SF_QSTN ( isffln, stid, istnm, slat, slon, selv, ispri,
     +			   stat, coun, iret )
C
C*	    Reset pointer so that it will not get this station again.
C
	    onefnd (isffln) = .true.
	    RETURN
	END IF
C
C*	Check that the time has been set.
C
	IF  ( .not. timset (isffln) )  THEN
	    iret = -17
	    RETURN
	END IF
C
C*	Obtain next row/column.
C
	CALL DM_NEXT  ( isffln, irow, icol, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -8
	    RETURN
	END IF
C
C*	Determine data location (row or col) and read from file.
C
	IF  ( sttype (isffln) .eq. 'ROW' )  THEN
	    CALL DM_RRWH  ( isffln, irow, iheadr, ier1 )
	  ELSE
	    CALL DM_RCLH  ( isffln, icol, iheadr, ier1 )
	END IF
C
C*	Retrieve output values from headers.
C
	knstd = 0
	IF  ( kstid (isffln) .gt. 0 )  THEN
	    istid (1) = iheadr ( kstid (isffln) )
	    knstd = 1
	END IF
	IF  ( kstd2 (isffln) .gt. 0 )  THEN
	    istid (2) = iheadr ( kstd2 (isffln) )
	    knstd = 2
	END IF
	IF ( knstd .ne. 0 ) THEN
	    CALL ST_ITOS  ( istid, knstd, nc, stid, ier )
	  ELSE
	    stid = ' '
	END IF
C*
	IF  ( kstnm (isffln) .gt. 0 )  THEN
	    istnm = iheadr ( kstnm (isffln) )
	  ELSE
	    istnm = IMISSD
	END IF
C*
	IF  ( kslat (isffln) .gt. 0 )  THEN
	    ilat = iheadr ( kslat (isffln) )
	    IF  ( ilat .eq. IMISSD )  THEN
	        slat = RMISSD
	      ELSE
	        slat = ilat / 100.
	    END IF
	END IF
C*
	IF  ( kslon (isffln) .gt. 0 )  THEN
	    ilon = iheadr ( kslon (isffln) )
	    IF  ( ilon .eq. IMISSD )  THEN
	        slon = RMISSD
	      ELSE
	        slon = ilon / 100.
	    END IF
	END IF
C*
	IF  ( kselv (isffln) .gt. 0 )  THEN
	    ielv = iheadr ( kselv (isffln) )
	    IF  ( ielv .eq. IMISSD )  THEN
	        selv = RMISSD
	      ELSE
	        selv = ielv
	    END IF
	  ELSE
	    selv = RMISSD
	END IF
	IF ( kspri (isffln) .gt. 0 ) THEN
	    ispri =  iheadr ( kspri (isffln) )
	  ELSE
	    ispri = IMISSD
	END IF
C
C*	Save the row and column.
C
	krow (isffln)   = irow
	kcol (isffln)   = icol
	curstn (isffln) = stid
C*
	RETURN
	END
