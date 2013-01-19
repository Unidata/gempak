	SUBROUTINE SF_STAT  ( isffln, stcn, iret )
C************************************************************************
C* SF_STAT								*
C*									*
C* This subroutine selects a state or country.  Later calls to 		*
C* SF_SNXT will return stations in the state or country.  SF_STIM	*
C* must be called before this subroutine is called.  This subroutine	*
C* is included for compatibility with earlier versions of GEMPAK.  A	*
C* search for stations should be set using SF_UARE.			*
C*									*
C* SF_STAT  ( ISFFLN, STCN, IRET )					*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*	STCN		CHAR*2		State or country abbreviation	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -3 = file not open		*
C*					 -15 = no state/country info	*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C************************************************************************
	CHARACTER*(*)	stcn
C*
	CHARACTER	keyn*4
	LOGICAL		cnflag
C------------------------------------------------------------------------
	CALL SF_CHKF  ( isffln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Check to see if this is a country.
C
	CALL LC_COUN  ( stcn, cnflag, iret )
	IF  ( cnflag )  THEN
	    keyn = 'COUN'
	  ELSE
	    keyn = 'STAT'
	END IF
C
C*	Delete existing conditional searches.
C
	CALL DM_DCSR  ( isffln, ier )
C
C*	Reset search conditions.
C
	CALL SF_BEGS  ( isffln, ier )
C
C*	Set new search condition.
C
	CALL ST_CTOI  ( stcn, 1, istcn, ier )
	CALL DM_CSRC  ( isffln, .true., 1, keyn, istcn, istcn, ier )
	IF  ( ier .ne. 0 )  iret = -15
C*
	RETURN
	END
