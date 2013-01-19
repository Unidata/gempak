	SUBROUTINE SF_TNXT  ( isffln, dattim, iret )
C************************************************************************
C* SF_TNXT								*
C*									*
C* This subroutine selects the next time in a surface file.  SF_TSTN	*
C* must be called to set the station before this subroutine is called.	*
C* The times will be returned in the order in which they appear in	*
C* the file, rather than in chronological order.  Data for this time	*
C* can be read or written by calling SF_RDAT or SF_WDAT, respectively.	*
C*									*
C* SF_TNXT  ( ISFFLN, DATTIM, IRET )					*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER	 	Surface file number		*
C*									*
C* Output parameters:							*
C*	DATTIM		CHAR*		GEMPAK date/time		*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C*				   	  -3 = file not open		*
C*				   	  -9 = no more times		*
C*				  	 -18 = station not set		*
C**									*
C* Log:									*
C* I. Graffman/RDS	5/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C*
	CHARACTER*(*)	dattim
C*
	INTEGER		iheadr (MMKEY)
C------------------------------------------------------------------------
	CALL SF_CHKF  ( isffln, iret )
	IF  ( iret .ne. 0 )  RETURN
	IF  ( .not. stnset (isffln) )  THEN
	    iret = -18
	    RETURN
	END IF
C
C*	Obtain next row/column.
C
	CALL DM_NEXT  ( isffln, irow, icol, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret =  -9
	    curtim (isffln) = ' '
	    RETURN
	END IF
C
C*	Determine data location (row or col) and read from file.
C
	IF  ( dttype (isffln) .eq. 'ROW' )  THEN
	    CALL DM_RRWH  ( isffln, irow, iheadr, ier1 )
	  ELSE
	    CALL DM_RCLH  ( isffln, icol, iheadr, ier1 )
	END IF
C
C*	Retrieve output values from headers.
C
     	CALL TI_CDTM  ( iheadr ( kdate (isffln) ), 
     +			iheadr ( ktime (isffln) ), dattim, ier )
C
C*	Save the row and column.
C
	krow (isffln)   = irow
	kcol (isffln)   = icol
	curtim (isffln) = dattim
C*
	RETURN
	END
