	SUBROUTINE TM_PROM  ( messg, pagflg, newlin, iret )
C************************************************************************
C* TM_PROM								*
C*									*
C* This subroutine writes a message to the user's terminal followed	*
C* by the phrase 'or type EXIT'.  The phrase '<CR> to page' may also	*
C* be added.  This subroutine does not wait for a user response.	*
C*									*
C* TM_PROM  ( MESSG, PAGFLG, NEWLIN, IRET )				*
C*									*
C* Input parameters:							*
C*	MESSG		CHAR*		Message 			*
C*	PAGFLG		LOGICAL		Flag to add '<CR> to page'	*
C*	NEWLIN		LOGICAL		Flag to move to new line	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return 		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 4/84	Original source code			*
C* M. desJardins/GSFC	 3/88	Cleaned up				*
C* M. desJardins/GSFC	 9/90	Change message for null length string	*
C* K. Brill/NMC		01/92	Call TM_WLIN				*
C************************************************************************
	CHARACTER*(*)	messg
	LOGICAL		pagflg, newlin
C*
	CHARACTER	out*80
C*
	CHARACTER*(*)	strex, strpg
	PARAMETER	( strex = ' or type EXIT:' )
	PARAMETER	( strpg = ', <CR> to page' )
C------------------------------------------------------------------------
	iret= 0
	CALL ST_LSTR  ( messg, length, ierr )
C
C*	Check pagflg to see if page string should be added.
C
	IF  ( pagflg )  THEN
	    IF  ( length .gt. 0 )  THEN
		out = messg ( 1 : length ) // strpg // strex
	      ELSE
		out = 'Enter <CR> to page' // strex
	    END IF
C
C*	    Otherwise, add exit message only.
C
	  ELSE
	    IF  ( length .gt. 0 )  THEN
		out = messg ( 1 : length ) // strex
	      ELSE
		out = 'Type EXIT:'
	    END IF
	END IF
C
C*	Write the output string.
C
	CALL TM_WLIN ( out, newlin, ier )
C*
	RETURN
	END
