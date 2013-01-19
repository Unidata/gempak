	SUBROUTINE SF_SSTN  ( isffln, stn, stid, istnm, slat, slon, 
     +			      selv, ispri, iret )
C************************************************************************
C* SF_SSTN								*
C*									*
C* This subroutine selects a station in a surface file.  SF_STIM 	*
C* must be called before this subroutine is called.  This subroutine	*
C* will delete any searches previously set.  Data for this station 	*
C* can be read or written by calling SF_RDAT or SF_WDAT, respectively.	*
C*									*
C* SF_SSTN  ( ISFFLN, STN, STID, ISTNM, SLAT, SLON, SELV, ISPRI, IRET )	*
C*									*
C* Input parameters:							*
C* 	ISFFLN		INTEGER		Surface file number		*
C*	STN		CHAR*		Station id or number		*
C*									*
C* Output parameters:							*
C*	STID 		CHAR*4	 	Station identifier		*
C*	ISTNM 		INTEGER		Station number			*
C*	SLAT 		REAL		Station latitude		*
C*	SLON 		REAL		Station longitude		*
C*	SELV 		REAL		Station elevation		*
C*	ISPRI 		INTEGER         Station priority parameter      *
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C*				   	  -3 = file not open		*
C*				  	 -10 = station not in file	*
C*				  	 -17 = time not set		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* M. desJardins/GSFC	 6/88	From SN_SSTN				*
C* K. Brill/NMC		 8/93	Changes for 8-char id			*
C* A. Hardy/GSC		 3/99   Changed calling sequence; added ispri   *
C* A. Hardy/GSC		 3/99   Removed ispri = 0 			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C*
	CHARACTER*(*)	stn, stid 
C*
	CHARACTER	keynam (2)*4, stncap*8
	INTEGER		istn (2)
C------------------------------------------------------------------------
	CALL SF_CHKF  ( isffln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Check that time has been set.
C
	IF  ( .not. timset ( isffln ) )  THEN
	    iret = -17
	    RETURN
	END IF
C
C*	Reset search conditions.
C
	CALL SF_BEGS  ( isffln, ier )
C
C*	Delete existing conditional search.
C
	CALL DM_DCSR  ( isffln, ier )
C
C*	Check for station id or number.
C
	CALL ST_LCUC  ( stn, stncap, ier )
	CALL SF_STID  ( stncap, istn, keynam (1), ier )
C
C*	Set conditional search for station.
C
	IF ( keynam (1) .eq. 'STID' .and. kstd2 (isffln) .gt. 0 ) THEN
	    keynam (2) = 'STD2'
	    nkey = 2
	ELSE
	    nkey = 1
	END IF
C*
	CALL DM_CSRC  ( isffln, .true., nkey, keynam, istn, istn, ier )
C
C*	Get next station meeting search criteria
C
	CALL SF_SNXT  ( isffln, stid, istnm, slat, slon, selv, ispri, 
     +			iret )
	IF  ( iret .ne. 0 )  iret = -10
C
C*	Delete conditional search.
C
	CALL DM_DCSR  ( isffln, ier )
C*
	RETURN
	END
