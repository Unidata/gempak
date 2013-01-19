	SUBROUTINE SN_SSTN  ( isnfln, stn, stid, istnm, slat, slon, 
     +			      selv,   iret )
C************************************************************************
C* SN_SSTN								*
C*									*
C* This subroutine selects a station in a sounding file.  SN_STIM 	*
C* must be called before this subroutine is called.  This subroutine 	*
C* will delete any searches set by LC_SARE.  Data for this station 	*
C* can be returned or written by calling SN_RDAT or SN_WDAT,		*
C* respectively.							*
C*									*
C* SN_SSTN  ( ISNFLN, STN, STID, ISTNM, SLAT, SLON, SELV, IRET )	*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	STN		CHAR*		Station id or number		*
C*									*
C* Output parameters:							*
C*	STID 		CHAR*8	 	Station identifier		*
C*	ISTNM 		INTEGER		Station number			*
C*	SLAT 		REAL		Station latitude		*
C*	SLON 		REAL		Station longitude		*
C*	SELV 		REAL		Station elevation		*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C*				   	  -4 = file not open		*
C*				  	 -11 = station not in file	*
C*				  	 -19 = time not set		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/87						*
C* M. desJardins/GSFC	 8/87						*
C* K. Brill/NMC		 8/93	Changes for 8-char stn ID		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'
C*
	CHARACTER*(*)	stn, stid 
C*
	CHARACTER	keynam (2)*4, stncap*8
	INTEGER		istn (2)
C------------------------------------------------------------------------
	CALL SN_CHKF  ( isnfln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Check that time has been set.
C
	IF  ( .not. timset ( isnfln ) )  THEN
	    iret = -19
	    RETURN
	END IF
C
C*	Reset search conditions.
C
	CALL SN_BEGS  ( isnfln, ier )
C
C*	Delete existing conditional search.
C
	CALL DM_DCSR  ( isnfln, ier )
C
C*	Check for station id or number.
C
	CALL ST_LCUC  ( stn, stncap, ier )
	CALL SN_STID  ( stncap, istn, keynam (1), ier )
	IF ( keynam (1) .eq. 'STID' .and. kstd2 (isnfln) .gt. 0 ) THEN
	    keynam (2) = 'STD2'
	    nkey = 2
	ELSE
	    nkey =1
	END IF
C
C*	Set conditional search for station.
C
	CALL DM_CSRC  ( isnfln, .true., nkey, keynam, istn, istn, ier )
C
C*	Get next station meeting search criteria
C
	CALL SN_SNXT  ( isnfln, stid, istnm, slat, slon, selv, iret )
	IF  ( iret .ne. 0 )  THEN
	    iret = -11
	END IF
C
C*	Delete conditional search.
C
	CALL DM_DCSR  ( isnfln, ier )
C*
	RETURN
	END
