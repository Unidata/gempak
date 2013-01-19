	SUBROUTINE SF_QSTN  ( isffln, stid, istnm, slat, slon, selv,
     +			      ispri, stat, coun, iret )
C************************************************************************
C* SF_QSTN								*
C*									*
C* This subroutine gets station information for the current station.	*
C* Both the time and station must be set before this subroutine is	*
C* called.								*
C*									*
C* SF_QSTN  ( ISFFLN, STID, ISTNM, SLAT, SLON, SELV, ISPRI, STAT,       *
C*            COUN, IRET )						*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Sounding file number		*
C*									*
C* Output parameters:							*
C*	STID		CHAR*8		Station identifier		*
C*	ISTNM		INTEGER		Station number			*
C*	SLAT		REAL		Station latitude		*
C*	SLON		REAL		Station longitude		*
C*	SELV		REAL		Station elevation		*
C*	ISPRI		INTEGER		Station priority code           *
C*	STAT		CHAR*2		State				*
C*	COUN		CHAR*2		Country				*
C*	IRET		INTEGER	 	Return code			*
C*				    	   0 = normal return		*
C*				   	  -4 = file not open		*
C*				   	  -7 = location not set		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* K. Brill/NMC		 8/93	Changes for 8-char stn id (STD2) 	*
C* A. Hardy/GSC          3/99   Changed calling sequence; added ispri   *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C*
	CHARACTER*(*)	stid, stat, coun
C*
	INTEGER		iheadr (MMKEY), istid (2)
C------------------------------------------------------------------------
C*	Check the file number.
C
	CALL SF_CHKF  ( isffln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Check that station is set.
C
	IF  ( ( krow ( isffln ) .le. 0 ) .or.
     +        ( kcol ( isffln ) .le. 0 ) )  THEN
	    iret = -7
	END IF
C
C*	Determine station type (row or col) and read header from file.
C
	IF  ( sttype ( isffln ) .eq. 'ROW' )  THEN
	    CALL DM_RRWH  ( isffln, krow ( isffln ), iheadr, ier )
	  ELSE
	    CALL DM_RCLH  ( isffln, kcol ( isffln ), iheadr, ier )
	END IF
C
C*	Retrieve output values from headers.
C
	knstd = 0
	IF  ( kstid ( isffln ) .gt. 0 )  THEN
	    istid (1) = iheadr ( kstid (isffln) )
	    knstd = 1
	END IF
	IF  ( kstd2 ( isffln ) .gt. 0 )  THEN
	    istid (2) = iheadr ( kstd2 (isffln) )
	    knstd = 2
	END IF
	IF ( knstd .ne. 0 ) THEN
	    CALL ST_ITOS ( istid, knstd, nc, stid, ier )
	  ELSE
	    stid = ' '
	END IF
	IF  ( kstnm ( isffln ) .gt. 0 )  THEN
	    istnm = iheadr ( kstnm ( isffln ) )
	  ELSE
	    istnm = IMISSD
	END IF
	ilat = iheadr ( kslat ( isffln ) ) 
	IF  ( ilat .eq. IMISSD )  THEN
	    slat = RMISSD
	  ELSE
	    slat = ilat / 100.
	END IF
	ilon = iheadr ( kslon ( isffln ) ) 
	IF  ( ilon .eq. IMISSD )  THEN
	    slon = RMISSD
	  ELSE
	    slon = ilon / 100.
	END IF
	IF  ( kselv ( isffln ) .gt. 0 )  THEN
	    ielv = iheadr ( kselv ( isffln ) ) 
	    IF (ielv .eq. IMISSD) THEN
	        selv = RMISSD
	      ELSE
	        selv = ielv
	    END IF
	  ELSE
	    selv = RMISSD
	END IF
	IF  ( kspri ( isffln ) .gt. 0 )  THEN
	    ispri = iheadr ( kspri ( isffln ) )
	  ELSE
	    ispri = IMISSD
	END IF
	IF  ( kstat ( isffln ) .gt. 0 )  THEN
	    CALL ST_ITOC ( iheadr ( kstat ( isffln ) ), 1, stat, ier )
	  ELSE
	    stat = ' '
	END IF
	IF  ( kcoun ( isffln ) .gt. 0 )  THEN
	    CALL ST_ITOC ( iheadr ( kcoun ( isffln ) ), 1, coun, ier )
	  ELSE
	    coun = ' '
	END IF
C*
	RETURN
	END
