	SUBROUTINE SN_STID  ( stn, istn, keynam, iret )
C************************************************************************
C* SN_STID								*
C*									*
C* This subroutine takes a station id and checks to see if it is a	*
C* character station identifier or a station number.  If it is a 	*
C* station identifier, it is encoded into a number and returned in	*
C* ISTN.  The keynam ('STID' or 'STNM') corresponding to the station	*
C* type is also returned.						*
C*									*
C* SN_STID  ( STN, ISTN, KEYNAM, IRET )					*
C*									*
C* Input parameters:							*
C*	STN		CHAR*		Station id or number		*
C*									*
C* Output parameters:							*
C*	ISTN (2)	INTEGER		Stn number or encoded id	*
C*	KEYNAM		CHAR*4		Key name			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* K. Brill/NMC		 8/93	Changes for 8-char stn ID		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	stn, keynam
	INTEGER		istn (2)
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for station number.
C
	CALL ST_NUMB  ( stn, istn, ier )
C
C*	Set proper key name.
C
	IF  ( ier .eq. 0 )  THEN
	    keynam = 'STNM'
	  ELSE
C
C*	    For station id, encode in integer.
C
	    CALL ST_STOI  ( stn, 8, nv, istn, ier )
	    keynam = 'STID'
	END IF
C*
	RETURN
	END
