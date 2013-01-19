	SUBROUTINE TI_STAN ( time, lastim, dattim, iret )
C************************************************************************
C* TI_STAN								*
C*									*
C* This subroutine takes a user input time and converts it into a 	*
C* standard GEMPAK time.  The parts of the date and time that are	*
C* not entered by the user are taken from the LASTIM, which must	*
C* already be in the standard format.					*
C*									*
C* TI_STAN  ( TIME, LASTIM, DATTIM, IRET )				*
C*									*
C* Input parameters:							*
C*	TIME		CHAR*		Input time			*
C*	LASTIM		CHAR*		Last time in GEMPAK format	*
C*									*
C* Output parameters:							*
C*	DATTIM		CHAR*		GEMPAK standard time		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return 		*
C*					 -1 = invalid input string	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/84						*
C* M. desJardins/GSFC	11/87	Cleaned up				*
C* M. desJardins/GSFC	 1/88	Eliminated setting minutes from lasttm	*
C************************************************************************
	CHARACTER*(*)	time, lastim, dattim
C*
	CHARACTER	newtim*20
C------------------------------------------------------------------------
	CALL TI_FORM  ( time, newtim, iret )
	IF  ( iret .ne. 0 )  RETURN
C*
	IF  ( newtim .eq. 'LAST' )  THEN
	    dattim = lastim
	  ELSE
	    DO  i = 1, 9
	      IF  ( newtim (i:i) .eq. ' ' ) 
     +				newtim (i:i) = lastim (i:i)
	    ENDDO
	    DO  i = 10, 11
	      IF  ( newtim (i:i) .eq. ' ' ) 
     +				newtim (i:i) = '0'
	    ENDDO
	    dattim = newtim
	ENDIF
C*
	RETURN
	END
