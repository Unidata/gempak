	SUBROUTINE TM_INT  ( messg, pagflg, newln, nexp, integ, 
     +			     nint,  iret )
C************************************************************************
C* TM_INT								*
C*									*
C* This subroutine writes a message to the user's terminal followed	*
C* by 'or type EXIT'.  The phrase '<CR> to page' may also be added.	*
C* An array of integers entered by the user is returned.		*
C*									*
C* TM_INT  ( MESSG, PAGFLG, NEWLN, NEXP, INTEG, NINT, IRET )		*
C*									*
C* Input parameters:							*
C*	MESSG		CHAR*		Message				*
C*	PAGFLG		LOGICAL		Flag to add '<CR> to page'	*
C*	NEWLN		LOGICAL		Flag to move to new line	*
C*	NEXP		INTEGER		Maximum number of integers	*
C*									*
C* Output parameters:							*
C*	INTEG (NINT)	INTEGER		Integers entered by user	*
C*	NINT		INTEGER		Number of integers		*
C*	IRET		INTEGER		Return code			*
C*					  3 = too many integers		*
C*					  2 = EXIT entered		*
C*					  1 = <CR> entered		*
C*					  0 = normal return 		*
C*					 -3 = invalid input string	*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/84						*
C* M. Goodman/RDS	 3/85	Corrected error with <cr>		*
C* M. desJardins/GSFC	 3/88	Cleaned up				*
C************************************************************************
	CHARACTER*(*) 	messg, string*80
	LOGICAL 	pagflg, newln
	INTEGER 	integ (1)
C------------------------------------------------------------------------
C*	Get user input.
C
	CALL TM_STR  ( messg, pagflg, newln, string, iret )
	IF  ( iret .gt. 0 )  RETURN
C
C*	Get array of integers.
C
	CALL ST_C2I  ( string, nexp, integ, nint, ier )
	IF  ( ier .lt. 0 )  iret = -3
	IF  ( ier .eq. 1 )  iret = 3
C*
	RETURN
	END
