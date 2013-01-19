	SUBROUTINE SN_TNXT  ( isnfln, dattim, iret )
C************************************************************************
C* SN_TNXT								*
C*									*
C* This subroutine selects the next time in a sounding file.  SN_TSTN	*
C* must be called to set the station before this subroutine is called.	*
C* The times will be returned in the order in which they appear in	*
C* the file rather than in chronological order.  Data for this time	*
C* may be returned or written by calling SN_RDAT or SN_WDAT,		*
C* respectively.							*
C*									*
C* SN_TNXT  ( ISNFLN, DATTIM, IRET )					*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER	 	Sounding file number		*
C*									*
C* Output parameters:							*
C*	DATTIM		CHAR*		GEMPAK date/time		*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C*				   	  -4 = file not open		*
C*				   	 -10 = no more times		*
C*				  	 -20 = station not set		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/87						*
C* M. desJardins/GSFC	 8/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'
C*
	CHARACTER* (*)	dattim
C*
	INTEGER		iheadr (MMKEY)
C------------------------------------------------------------------------
	CALL SN_CHKF ( isnfln, iret )
	IF ( iret .ne. 0 ) RETURN
C
C*	Check that station is set.
C
	IF ( .not. stnset ( isnfln ) ) THEN
	    iret = -20
	    RETURN
	END IF
C
C*	Find next time.
C
	CALL DM_NEXT  ( isnfln, irow, icol, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -10
	    krow   ( isnfln ) = 0
	    kcol   ( isnfln ) = 0
	    curtim ( isnfln ) = ' '
	    RETURN
	END IF
C
C*	Determine location of time (row or col) and read header.
C
	IF  ( dttype ( isnfln ) .eq. 'ROW' )  THEN
	    CALL DM_RRWH  ( isnfln, irow, iheadr, ier )
	  ELSE
	    CALL DM_RCLH  ( isnfln, icol, iheadr, ier )
	END IF
C
C*	Retrieve output values from headers.
C
	CALL TI_CDTM ( iheadr ( kdate ( isnfln ) ), 
     +		       iheadr ( ktime ( isnfln ) ), dattim, ier )
	curtim ( isnfln ) = dattim
C
C*	Save the row and column.
C
	krow ( isnfln ) = irow
	kcol ( isnfln ) = icol
C*
	RETURN
	END
