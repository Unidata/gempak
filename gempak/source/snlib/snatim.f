	SUBROUTINE SN_ATIM ( isnfln, dattim, iret )
C************************************************************************
C* SN_ATIM								*
C*									*
C* This subroutine adds a time to a sounding data file.  This		*
C* subroutine can only be used if times and stations are not mixed	*
C* in row or column headers.						*
C*									*
C* SN_ATIM  ( ISNFLN, DATTIM, IRET )					*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	DATTIM		CHAR*		Date/time			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C*				   	  -4 = file not open		*
C*				   	  -5 = time cannot be added	*
C*				  	 -21 = non-standard file	*
C*					 -23 = time is invalid		*
C**									*
C* Log:									*
C* I. Graffman/RDS	5/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'
C*
	INTEGER		iheadr (MMKEY)
	CHARACTER* (*)	dattim
C------------------------------------------------------------------------
	CALL SN_CHKF ( isnfln, iret )
	IF ( iret .ne. 0 )  RETURN
C
C*	Check that time and station headers are not intermixed.
C
	IF ( dttype ( isnfln ) .eq. sttype ( isnfln ) ) THEN	
	    iret = -21
	    RETURN
	END IF
C
C*	Convert time to integer.
C
	CALL TI_IDTM ( dattim, iheadr ( kdate ( isnfln ) ), 
     +			       iheadr ( ktime ( isnfln ) ), ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -23
	    RETURN
	END IF
C
C*	Determine data location (row or col) and add to file.
C
	IF ( dttype ( isnfln ) .eq. 'ROW' ) THEN
	    CALL DM_WRWH ( isnfln, 0, iheadr, jp, ier )
	  ELSE
	    CALL DM_WCLH ( isnfln, 0, iheadr, jp, ier )
	END IF
C
C*	Return error if time cannot be added.
C
	IF  ( ier .ne. 0 )  THEN
	    iret = -5
	    RETURN
	END IF
C*
	RETURN
	END
