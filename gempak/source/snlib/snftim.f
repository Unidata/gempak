	SUBROUTINE SN_FTIM  ( isnfln, dattim, iret )
C************************************************************************
C* SN_FTIM								*
C*									*
C* This subroutine finds the location of the specified date/time	*
C* in a DM file.  The first row or column containing the time is set in	*
C* the common area.  This subroutine may only be used when times and 	*
C* stations are not mixed in row or column headers in the file.  	*
C* It will execute faster than the SN_Sxxx or SN_Txxx subroutines,	*
C* but is intended to be used only for real-time ingest programs 	*
C* where the structure of the file is known by the programmer.		*
C* The station may be set using SN_FSTN.  These subroutines may be	*
C* called in either order.						*
C*									*
C* SN_FTIM  ( ISNFLN, DATTIM, IRET )					*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	DATTIM		CHAR*		Date/time			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C*				   	  -4 = file not open		*
C*				  	 -15 = time not set		*
C*				  	 -21 = non-standard file	*
C**									*
C* Log:									*
C* I. Graffman/RDS	5/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'
C*
	CHARACTER* (*) 	dattim
C*
	INTEGER		keyloc (2), keyval (2)
C------------------------------------------------------------------------
	CALL SN_CHKF  ( isnfln, iret )
	IF  ( iret .ne. 0 ) RETURN
C
C*	Check for distinct date/time and station types.
C
	IF  ( dttype ( isnfln ) .eq. sttype ( isnfln ) ) THEN
	    iret = -21
	    RETURN
	END IF
C
C*	Check search type and reset row and column if necessary.
C
	IF ( .not. ftmset ( isnfln ) ) THEN
	    timset ( isnfln ) = .false.
	    stnset ( isnfln ) = .false.
	    ftmset ( isnfln ) = .true.
	    krow   ( isnfln ) = 0
	    kcol   ( isnfln ) = 0
	    curstn ( isnfln ) = ' '
	END IF
C
C*	Break the time into 2 integers.
C
	CALL TI_IDTM  ( dattim, keyval (1), keyval (2), ier )
C
C*	Search for time in file.
C
	keyloc (1) = kdate ( isnfln )
	keyloc (2) = ktime ( isnfln )
	CALL DM_SRCH  ( isnfln, dttype ( isnfln ), 2, keyloc, keyval,
     +			irwcl, iret  )
C
C*	Set row or column in common.
C
	IF  ( iret .eq. 0 )  THEN
	    IF  ( dttype ( isnfln ) .eq. 'ROW' )  THEN
		krow ( isnfln ) = irwcl
	      ELSE
		kcol ( isnfln ) = irwcl
	    END IF
	    curtim ( isnfln ) = dattim
	  ELSE
	    iret = -12
	END IF
C*
	RETURN
	END
