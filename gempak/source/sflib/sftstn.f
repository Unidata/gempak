	SUBROUTINE SF_TSTN  ( isffln, stn, iret )
C************************************************************************
C* SF_TSTN								*
C*									*
C* This subroutine sets the station in a surface file.  All later	*
C* time searches will return times corresponding to this station.	*
C*									*
C* SF_TSTN  ( ISFFLN, STN, IRET )					*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*	STN		CHAR*	 	Station number or id		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C*				   	  -3 = file not open		*
C*				  	 -10 = station not in file	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* M. desJardins/GSFC	 6/88	From SN_TSTN				*
C* K. Brill/NMC		 8/93	Changes for 8-char ID			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C*
	CHARACTER* (*)	stn
C*
	LOGICAL 	staflg
	CHARACTER	keynam (2)*4, stid*8
	INTEGER		istid (2)
C------------------------------------------------------------------------
	CALL SF_CHKF  ( isffln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Check that the station is in the file.
C
	CALL ST_LCUC  ( stn, stid, ier )
	CALL SF_CSTN  ( isffln, stid, istid, keynam (1), staflg, irwcl, 
     +			ier )
	IF  ( .not. staflg )  THEN
	    iret = -10
	    RETURN
	END IF
C
C*	Reset search.
C
	CALL SF_BEGS  ( isffln, ier )
C
C*	Set primary search for station.
C
	IF ( keynam (1) .eq. 'STID' .and. kstd2 (isffln) .gt. 0 ) THEN
	    keynam (2) = 'STD2'
	    nkey = 2
	ELSE
	    nkey = 1
	END IF
	CALL DM_PSRC  ( isffln, nkey, keynam, istid, istid, ier )
C
C*	Save station in common.
C
	stnset ( isffln ) = .true.
	curstn ( isffln ) =  stn
C
C*	Reset current row, column and search type.
C
	krow   ( isffln ) = 0
	kcol   ( isffln ) = 0
	timset ( isffln ) = .false.
	ftmset ( isffln ) = .false.
	curtim ( isffln ) = ' '
C*
	RETURN
	END
