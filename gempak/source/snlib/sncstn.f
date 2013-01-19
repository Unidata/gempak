	SUBROUTINE SN_CSTN  ( isnfln, stid, istid, keynam, staflg, 
     +			      irowcl, iret )
C************************************************************************
C* SN_CSTN								*
C*									*
C* This subroutine determines if the specified station exists in	*
C* the sounding file.							*
C*									*
C* SN_CSTN  ( ISNFLN, STID, ISTID, KEYNAM, STAFLG, IROWCL, IRET )	*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER	 	Sounding file number		*
C*	STID		CHAR*		Station id or number		*
C*									*
C* Output parameters:							*
C*	ISTID (2)	INTEGER		Stn number or encoded id	*
C*	KEYNAM		CHAR*4		Station key name		*
C*	STAFLG		LOGICAL		Station found flag		*
C*	IROWCL		INTEGER		Row or column of station	*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* K. Brill/NMC		 8/93	Changes for 8-char stn ID		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'
C*
	CHARACTER*(*)	stid, keynam
	INTEGER		istid (2)
	LOGICAL		staflg
C*
	INTEGER		keyloc (2)
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for keyword to search.
C
	CALL SN_STID  ( stid, istid, keynam, ier )
C
C*	Check for location to search.
C
	nkey = 1
	IF  ( keynam .eq. 'STID' )  THEN
	    keyloc (1) = kstid ( isnfln )
	    IF ( kstd2 (isnfln) .gt. 0 ) THEN
		keyloc (2) = kstd2 ( isnfln )
		nkey = 2
	    END IF
	  ELSE
	    keyloc (1) = kstnm ( isnfln )
	END IF
C
C*	Search for first instance.
C
	CALL DM_SRCH ( isnfln, sttype ( isnfln ), nkey, keyloc, istid,
     +		       irowcl, ier )
C
C*	Set staflg.
C
	IF ( ier .ne. 0 ) THEN
	    staflg = .false.
	  ELSE
	    staflg = .true.
	END IF
C*
	RETURN
	END
