	SUBROUTINE TM_WLIN  ( messg, newlin, iret )
C************************************************************************
C* TM_WLIN								*
C*									*
C* This subroutine writes a line of fewer than 80 characters to the	*
C* user's terminal.  This subroutine does not wait for a user response.	*
C*									*
C* TM_WLIN  ( MESSG, NEWLIN, IRET )					*
C*									*
C* Input parameters:							*
C*	MESSG		CHAR*		Message 			*
C*	NEWLIN		LOGICAL		Flag to move to new line	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return 		*
C**									*
C* Log:									*
C* K. Brill/NMC		01/92						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	messg
	LOGICAL		newlin
C------------------------------------------------------------------------
	iret= 0
C
C*	Get length of output string.
C
	CALL ST_LSTR  ( messg, length, ier )
	IF  ( length .ge. 80 )  length = 79
C
C*	Write line with proper carriage control.
C
	IF  ( newlin )  THEN
	    WRITE  ( 6, 5000 )  messg ( 1 : length )
5000	    FORMAT ( ' ', A )
	  ELSE
	    IF ( MTMACH .eq. MTVAX ) THEN
		WRITE ( 6, 5005 ) messg ( 1 : length )
5005		FORMAT ('$', A )
	    ELSE
	    	WRITE  ( 6, 5010 )  messg ( 1 : length )
5010	    	FORMAT ( A, $ )
	    END IF
	END IF
C*
	RETURN
	END
