	SUBROUTINE SN_QSTN  ( isnfln, stid, istnm, slat, slon, selv,
     +			      stat,   coun, iret )
C************************************************************************
C* SN_QSTN								*
C*									*
C* This subroutine gets station information for the current station.	*
C* Both the time and station must be set before this subroutine is	*
C* called.								*
C*									*
C* SN_QSTN  ( ISNFLN, STID, ISTNM, SLAT, SLON, SELV, STAT, COUN,	*
C*            IRET )							*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*									*
C* Output parameters:							*
C*	STID		CHAR*8		Station identifier		*
C*	ISTNM		INTEGER		Station number			*
C*	SLAT		REAL		Station latitude		*
C*	SLON		REAL		Station longitude		*
C*	SELV		REAL		Station elevation		*
C*	STAT		CHAR*2		State				*
C*	COUN		CHAR*2		Country				*
C*	IRET		INTEGER	 	Return code			*
C*				    	   0 = normal return		*
C*				   	  -4 = file not open		*
C*				   	  -8 = location not set		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* K. Brill/NMC		 8/93	Changes for 8-char stn ID		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'
C*
	CHARACTER*(*)	stid, stat, coun
C*
	INTEGER		iheadr (MMKEY), istid (2)
C------------------------------------------------------------------------
C*	Check the file number.
C
	CALL SN_CHKF ( isnfln, iret )
	IF ( iret .ne. 0 ) RETURN
C
C*	Check that station is set.
C
	IF  ( ( krow ( isnfln ) .le. 0 ) .or.
     +        ( kcol ( isnfln ) .le. 0 ) )  THEN
	    iret = -8
	END IF
C
C*	Determine station type (row or col) and read header from file.
C
	IF ( sttype ( isnfln ) .eq. 'ROW' ) THEN
	    CALL DM_RRWH  ( isnfln, krow ( isnfln ), iheadr, ier )
	  ELSE
	    CALL DM_RCLH  ( isnfln, kcol ( isnfln ), iheadr, ier )
	END IF
C
C*	Retrieve output values from headers.
C
	IF  ( kstid ( isnfln ) .gt. 0 )  THEN
	    nwd = 1
	    istid (1) = iheadr ( kstid (isnfln) )
	    IF ( kstd2 (isnfln) .gt. 0 ) THEN
		istid (2) = iheadr ( kstd2 (isnfln) )
		nwd = 2
	    END IF
	    CALL ST_ITOS ( istid, nwd, nc, stid, ier )
	  ELSE
	    stid = ' '
	END IF
	IF ( kstnm ( isnfln ) .gt. 0 ) THEN
	    istnm = iheadr ( kstnm ( isnfln ) )
	  ELSE
	    istnm = IMISSD
	END IF
	ilat = iheadr ( kslat ( ISNFLN ) ) 
	IF (ilat .eq. IMISSD) THEN
	    slat = RMISSD
	  ELSE
	    slat = ilat / 100.
	END IF
	ilon = iheadr ( kslon ( isnfln ) ) 
	IF (ilon .eq. IMISSD) THEN
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
	IF  ( kstat ( isnfln ) .gt. 0 )  THEN
	    CALL ST_ITOC ( iheadr ( kstat ( isnfln ) ), 1, stat, ier )
	  ELSE
	    stat = ' '
	END IF
	IF  ( kcoun ( isnfln ) .gt. 0 )  THEN
	    CALL ST_ITOC ( iheadr ( kcoun ( isnfln ) ), 1, coun, ier )
	  ELSE
	    coun = ' '
	END IF
C*
	RETURN
	END
