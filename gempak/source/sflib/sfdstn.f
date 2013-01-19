	SUBROUTINE SF_DSTN  ( isffln, stn, iret )
C************************************************************************
C* SF_DSTN								*
C*									*
C* This subroutine deletes a station from a surface file.  All the	*
C* data corresponding to the station will be deleted along with the 	*
C* station header.							*
C*									*
C* SF_DSTN  ( ISFFLN, STN, IRET )					*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*	STN		CHAR*		Station number or id		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C*					  -3 = file not open		*
C*				  	 -13 = delete error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* M. desJardins/GSFC	 6/88	From SN_DSTN				*
C* K. Brill/NMC		 8/93	Add STD2				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C*
	CHARACTER* (*) 	stn
C*
	CHARACTER	keynam (2)*4, stid*8
	INTEGER		istid (2)
C------------------------------------------------------------------------
	CALL SF_CHKF ( isffln, iret )
	IF  (iret .ne. 0 ) RETURN
C
C*	Check the station id.
C
	CALL ST_LCUC  ( stn, stid, ier )
	CALL SF_STID  ( stid, istid, keynam (1), ier )
C
C*	Delete data at station.
C
	IF ( keynam (1) .eq. 'STID' .and. kstd2 (isffln) .gt. 0 ) THEN
	    keynam (2) = 'STD2'
	    nkey = 2
	ELSE
	    nkey = 1
	END IF
	CALL DM_DALL  ( isffln, nkey, keynam, istid, istid, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -13
	END IF
C*
	RETURN
	END
