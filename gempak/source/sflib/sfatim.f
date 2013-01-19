	SUBROUTINE SF_ATIM  ( isffln, dattim, iret )
C************************************************************************
C* SF_ATIM								*
C*									*
C* This subroutine adds a time to a surface data file.  This		*
C* subroutine can only be used if times and stations are not mixed	*
C* in row or column headers.  If data are to be added for this time,	*
C* SF_STIM must be called first.					*
C*									*
C* SF_ATIM  ( ISFFLN, DATTIM, IRET )					*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*	DATTIM		CHAR*		Date/time			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C*				   	  -3 = file not open		*
C*				   	  -4 = too many times		*
C*				  	 -19 = non-standard file	*
C*					 -20 = time is invalid		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C*
	CHARACTER* (*)	dattim
C*
	INTEGER		iheadr (MMKEY)
C------------------------------------------------------------------------
	CALL SF_CHKF  ( isffln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Check that times and stations are not intermixed.
C
	IF  ( dttype (isffln) .eq. sttype (isffln) )  THEN	
	    iret = -19
	    RETURN
	END IF
C
C*	Convert time to integer.
C
	CALL TI_IDTM  ( dattim, iheadr ( kdate (isffln) ),
     +				iheadr ( ktime (isffln) ), ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -20
	    RETURN
	END IF
C
C*	Determine data location (row or col.) and add to file.
C
	IF  ( dttype (isffln) .eq. 'ROW' )  THEN
	    CALL DM_WRWH  ( isffln, 0, iheadr, jp, ier1 )
	  ELSE
	    CALL DM_WCLH  ( isffln, 0, iheadr, jp, ier1 )
	END IF
	IF  ( ier1 .ne. 0 )  iret = -4
C*
	RETURN
	END
