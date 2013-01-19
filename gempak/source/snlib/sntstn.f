	SUBROUTINE SN_TSTN  ( isnfln, stn, iret )
C************************************************************************
C* SN_TSTN								*
C*									*
C* This subroutine sets the station in a sounding file.  All later	*
C* time searches will return times corresponding to this station.	*
C*									*
C* SN_TSTN  ( ISNFLN, STN, IRET )					*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	STN		CHAR*	 	Station number or id		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C*				   	  -4 = file not open		*
C*				  	 -11 = station not in file	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* K. Brill/NMC		 8/93	Changes for 8-char stn ID		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'
C*
	CHARACTER* (*)	stn
C*
	LOGICAL 	staflg
	CHARACTER	keynam (2)*4, stid*8
	INTEGER		iheadr (MMKEY), istid (2)
C------------------------------------------------------------------------
	CALL SN_CHKF ( isnfln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Check that the station is in the file.
C
	CALL ST_LCUC  ( stn, stid, ier )
	CALL SN_CSTN  ( isnfln, stid, istid, keynam (1), staflg, irwcl, 
     +			ier )
	IF  ( .not. staflg )  THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Save the station elevation in common.
C
	IF  ( sttype ( isnfln ) .eq. 'ROW' )  THEN
	    CALL DM_RRWH  ( isnfln, irwcl, iheadr, ier )
	  ELSE
	    CALL DM_RCLH  ( isnfln, irwcl, iheadr, ier )
	END IF
	IF  ( kselv ( isnfln ) .gt. 0 )  THEN
	    ielv = iheadr ( kselv ( isnfln ) )
	    IF  ( ielv .eq. IMISSD )  THEN
		shght ( isnfln ) = RMISSD
	      ELSE
		shght ( isnfln ) = ielv
	    END IF
	  ELSE
	    shght ( isnfln ) = 0.
	END IF
C
C*	Reset search.
C
	CALL SN_BEGS ( isnfln, ier )
C
C*	Set primary search for station.
C
	IF ( keynam (1) .eq. 'STID' .and. kstd2 (isnfln) .gt. 0 ) THEN
	    keynam (2) = 'STD2'
	    nkey = 2
	ELSE
	    nkey = 1
	END IF   
	CALL DM_PSRC ( isnfln, nkey, keynam, istid, istid, ier )
C
C*	Save station in common.
C
	stnset ( isnfln ) = .true.
	curstn ( isnfln ) = stn
	icrstn ( isnfln, 1 ) = istid (1)
	icrstn ( isnfln, 2 ) = istid (2)
C
C*	Reset current row, column and search type.
C
	krow   ( isnfln ) = 0
	kcol   ( isnfln ) = 0
	timset ( isnfln ) = .false.
	ftmset ( isnfln ) = .false.
	curtim ( isnfln ) = ' '
C*
	RETURN
	END
