	SUBROUTINE SN_DSTN  ( isnfln, stn, iret )
C************************************************************************
C* SN_DSTN								*
C*									*
C* This subroutine deletes a station from a sounding file.  All the	*
C* data corresponding to the station will be deleted along with the 	*
C* station headers.							*
C*									*
C* SN_DSTN  ( ISNFLN, STN, IRET )					*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	STN		CHAR*		Station number or id		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C*					  -4 = file not open		*
C*				  	 -15 = delete error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* K. Brill/NMC		 8/93	Changes for 8-char stn ID		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'
C*
	CHARACTER* (*) 	stn
C*
	CHARACTER	keynam (2)*4, stid*8
	INTEGER		istid (2)
C------------------------------------------------------------------------
	CALL SN_CHKF ( isnfln, iret )
	IF  (iret .ne. 0 ) RETURN
C
C*	Check the station id.
C
	CALL ST_LCUC  ( stn, stid, ier )
	CALL SN_STID  ( stid, istid, keynam (1), ier )
C
C*	Delete data at station.
C
	IF ( keynam (1) .eq. 'STID' .and. kstd2 (isnfln) .gt. 0 ) THEN
	    keynam (2) = 'STD2'
	    nkey = 2
	ELSE
	    nkey = 1
	END IF
	CALL DM_DALL  ( isnfln, nkey, keynam, istid, istid, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -15
	END IF
C*
	RETURN
	END
