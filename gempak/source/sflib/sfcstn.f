	SUBROUTINE SF_CSTN  ( isffln, stid, istid, keynam, staflg, 
     +			      irowcl, iret )
C************************************************************************
C* SF_CSTN								*
C*									*
C* This subroutine checks for the specified station in the surface	*
C* file.  It is called internally by the SF routines.			*
C*									*
C* SF_CSTN  ( ISFFLN, STID, ISTID, KEYNAM, STAFLG, IROWCL, IRET )	*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER	 	Surface file number		*
C*	STID		CHAR*		Station id or number		*
C*									*
C* Output parameters:							*
C*	ISTID (2)	INTEGER		Stn number or encoded id	*
C*	KEYNAM 		CHAR*4		Station key name		*
C*	STAFLG		LOGICAL		Station found flag		*
C*	IROWCL		INTEGER		Row or column of station	*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* M. desJardins/GSFC	 6/88	Adapted from SN_CSTN			*
C* K. Brill/NMC		 8/93	Changes for STD2			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
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
	CALL SF_STID  ( stid, istid, keynam, ier )
C
C*	Check for location to search.
C
	IF  ( keynam .eq. 'STID' )  THEN
	    keyloc (1) = kstid ( isffln )
	    keyloc (2) = kstd2 ( isffln )
	    IF ( keyloc (2) .gt. 0 ) THEN
		nkey = 2
	      ELSE
		nkey = 1
	    END IF
	  ELSE
	    keyloc (1) = kstnm ( isffln )
	    nkey = 1
	END IF
C
C*	Search for first instance.
C
	CALL DM_SRCH ( isffln, sttype ( isffln ), nkey, keyloc, istid,
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
