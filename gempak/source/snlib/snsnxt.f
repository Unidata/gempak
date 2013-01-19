	SUBROUTINE SN_SNXT  ( isnfln, stid, istnm, slat, slon, selv, 
     +			      iret )
C************************************************************************
C* SN_SNXT								*
C*									*
C* This subroutine selects the next station in a sounding file. 	*
C* SN_STIM must be called to set the time before this subroutine 	*
C* is called.  Stations to be found may be set in LC_SARE or		*
C* LC_UARE.  Data for this station may be returned or written by	*
C* calling SN_RDAT or SN_WDAT, respectively.				*
C*									*
C* SN_SNXT  ( ISNFLN, STID, ISTNM, SLAT, SLON, SELV, IRET )		*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER	 	Sounding file number		*
C*									*
C* Output parameters:							*
C*	STID  		CHAR*8		Station identifier		*
C*	ISTNM 		INTEGER		Station number			*
C*	SLAT  		REAL	 	Station latitude		*
C*	SLON  		REAL		Station longitude		*
C*	SELV  		REAL		Station elevation		*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C*				   	  -4 = file not open		*
C*				   	  -9 = no more stations		*
C*				  	 -19 = time not set		*
C**									*
C* Log:									*
C* I. Graffman/RDS	5/87						*
C* K. Brill/NMC		8/93	Changes for 8-char stn ID		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'
C*
	CHARACTER* (*)	stid 
C*
	INTEGER		iheadr (MMKEY), istid (2)
C------------------------------------------------------------------------
	CALL SN_CHKF  ( isnfln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Check that the time has been set.
C
	IF  ( .not. timset ( isnfln ) )  THEN
	    iret = -19
	    RETURN
	END IF
C
C*	Obtain next row/column.
C
	CALL DM_NEXT  ( isnfln, irow, icol, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -9
	    krow   ( isnfln ) = 0
	    kcol   ( isnfln ) = 0
	    curstn ( isnfln ) = ' '
	    RETURN
	END IF
C
C*	Determine data location (row or col) and read from file.
C
	IF  ( sttype ( isnfln ) .eq. 'ROW' )  THEN
	    CALL DM_RRWH  ( isnfln, irow, iheadr, ier )
	  ELSE
	    CALL DM_RCLH  ( isnfln, icol, iheadr, ier )
	END IF
C
C*	Retrieve output values from headers.
C
	IF  ( kstid ( isnfln ) .gt. 0 )  THEN
	    istid (1) = iheadr ( kstid (isnfln) )
	    nwd = 1
	    IF ( kstd2 (isnfln) .gt. 0 ) THEN
		istid (2) = iheadr ( kstd2 (isnfln) )
		nwd = 2
	    END IF
	    CALL ST_ITOS  ( istid, nwd, nc, stid, ier )
	  ELSE
	    stid = ' '
	END IF
	IF  ( kstnm ( isnfln ) .gt. 0 )  THEN
	    istnm = iheadr ( kstnm ( isnfln ) )
	  ELSE
	    istnm = IMISSD
	END IF
	ilat = iheadr ( kslat ( isnfln ) ) 
	IF  ( ilat .eq. IMISSD )  THEN
	    slat = RMISSD
	  ELSE
	    slat = ilat / 100.
	END IF
	ilon = iheadr ( kslon ( isnfln ) ) 
	IF  ( ilon .eq. IMISSD )  THEN
	    slon = RMISSD
	  ELSE
	    slon = ilon / 100.
	END IF
	IF  ( kselv ( isnfln ) .gt. 0 )  THEN
	    ielv = iheadr ( kselv ( isnfln ) ) 
	    IF (ielv .eq. IMISSD) THEN
	        selv = RMISSD
	      ELSE
	        selv = ielv 
	    END IF
	  ELSE
	    selv = RMISSD
	END IF
	shght ( isnfln ) = selv
C
C*	Save the row and column.
C
	krow ( isnfln ) = irow
	kcol ( isnfln ) = icol
C*
	RETURN
	END
