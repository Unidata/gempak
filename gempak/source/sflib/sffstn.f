	SUBROUTINE SF_FSTN  ( isffln, stn, iret )
C************************************************************************
C* SF_FSTN								*
C*									*
C* This subroutine finds the location of the specified station in	*
C* a file.  The first occurrence of the station is saved.  This 	*
C* subroutine may only be used when times and stations are not mixed 	*
C* in row or column headers in the file.  It will execute faster than	*
C* the SF_Sxxx or SF_Txxx subroutines, but is intended to be used	*
C* only for real-time ingest programs where the structure of the file 	*
C* is known by the programmer.  The time may be set using SF_FTIM.  	*
C* These subroutines may be called in either order.			*
C*									*
C*									*
C* SF_FSTN  ( ISFFLN, STN, IRET )					*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER	 	Surface file number		*
C*	STN 		CHAR*		Station number or id		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C*				   	  -3 = file not open		*
C*				  	 -10 = station not found	*
C*				  	 -19 = non-standard file	*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/87						*
C* M. desJardins/GSFC	 2/88	Fixed truncation problems and added	*
C*				station number capability		*
C* M. desJardins/GSFC	 4/90	Set row/col to 0 if stn not found	*
C* K. Brill/NMC		 8/93	Changes for STD2 (8 char id)		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C*
	CHARACTER*(*)	stn 
C*
	CHARACTER	stid*8, keynam*4
	INTEGER		istid (2), keyloc (2)
C------------------------------------------------------------------------
	CALL SF_CHKF  ( isffln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Check for distinct date/time and station types.
C
	IF  ( dttype (isffln) .eq. sttype (isffln) )  THEN
	    iret = -19
	    RETURN
	END IF
C
C*	Check search type and reset row and column if necessary.
C
	IF ( .not. ftmset ( isffln ) )  THEN
	    timset (isffln) = .false.
	    stnset (isffln) = .false.
	    ftmset (isffln) = .true.
	    krow   (isffln) = 0
	    kcol   (isffln) = 0
	    curtim (isffln) = ' '
	    curstn (isffln) = ' '
	END IF
C
C*	Convert the station id to caps and integer.
C
	CALL ST_LCUC  ( stn, stid, ier )
	CALL SF_STID  ( stid, istid, keynam, ier )
	IF  ( keynam .eq. 'STID' )  THEN
	    keyloc (1) = kstid ( isffln )
	    IF ( kstd2 ( isffln ) .gt. 0 ) THEN
		keyloc (2) = kstd2 ( isffln )
		nkey = 2
	    ELSE
		nkey = 1
	    END IF
	  ELSE
	    keyloc (1) = kstnm ( isffln )
	    nkey = 1
	END IF
C
C*	Find the row or column.
C
	CALL DM_SRCH  ( isffln, sttype (isffln), nkey, keyloc, istid,
     +			irwcl, iret )
C
C*	Save row or column in common.
C
	IF  ( sttype ( isffln ) .eq. 'ROW' )  THEN
	    krow (isffln) = irwcl
	  ELSE
	    kcol (isffln) = irwcl 
	END IF
C
C*	Set station and error message.
C
	IF  ( iret .eq. 0 )  THEN
	    curstn (isffln) = stn
	  ELSE
	    iret = -10
	END IF
C*
	RETURN
	END
