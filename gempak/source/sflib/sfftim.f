	SUBROUTINE SF_FTIM  ( isffln, dattim, iret )
C************************************************************************
C* SF_FTIM								*
C*									*
C* This subroutine finds the location of the specified date/time	*
C* in a file.  The first occurrence containing the time is saved.	*
C* This subroutine may only be used when times and stations are not 	*
C* mixed in row or column headers in the file.  It will execute faster 	*
C* than the SF_Sxxx or SF_Txxx subroutines, but is intended to 		*
C* be used only for real-time ingest programs where the structure of 	*
C* the file is known by the programmer.  The station may be set using 	*
C* SF_FSTN.  These subroutines may be called in either order.		*
C*									*
C* SF_FTIM  ( ISFFLN, DATTIM, IRET )					*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*	DATTIM		CHAR*		Date/time			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C*				   	  -3 = file not open		*
C*				  	 -11 = time not found		*
C*				  	 -19 = non-standard file	*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* M. desJardins/GSFC	 4/90	Set row/col to 0 if time is not found	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C*
	CHARACTER* (*) 	dattim
C*
	INTEGER		keyloc (2), keyval (2)
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
	IF  ( .not. ftmset (isffln ) )  THEN
	    timset (isffln) = .false.
	    stnset (isffln) = .false.
	    ftmset (isffln) = .true.
	    krow (isffln)   = 0
	    kcol (isffln)   = 0
	    curstn (isffln) = ' '
	    curtim (isffln) = ' '
	END IF
C
C*	Break the time into 2 integers.
C
	CALL TI_IDTM  ( dattim, keyval (1), keyval (2), ier )
C
C*	Search for row or column.
C
	keyloc (1) = kdate (isffln)
	keyloc (2) = ktime (isffln)
	CALL DM_SRCH  ( isffln, dttype (isffln), 2, keyloc, keyval,
     +			irwcl, iret )
C
C*	Save row/column with time.
C
	IF  ( dttype ( isffln ) .eq. 'ROW' )  THEN
	    krow ( isffln ) = irwcl
	  ELSE
	    kcol ( isffln ) = irwcl
	END IF
C
C*	Save current time or set error message.
C
	IF  ( iret .eq. 0 )  THEN
	    curtim ( isffln ) = dattim
	  ELSE
	    iret = -11
	END IF
C*
	RETURN
	END
