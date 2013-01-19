	SUBROUTINE TM_REAL (messg, pagflg, newln, nexp, rlnos,
     +                       nreal, iret)
C************************************************************************
C* TM_REAL								*
C*									*
C* This subroutine writes a message to the user's terminal followed	*
C* by 'or type EXIT'.  The phrase '<CR> to page' may also be added.	*
C* An array of real numbers entered by the user is returned.		*
C*									*
C* TM_REAL  ( MESSG, PAGFLG, NEWLN, NEXP, RLNOS, NREAL, IRET )		*
C*									*
C* Input parameters:							*
C*	MESSG		CHAR*		Message				*
C*	PAGFLG		LOGICAL		Flag to add '<CR> to page'	*
C*	NEWLN		LOGICAL		Flag to move to new line	*
C*	NEXP		INTEGER		Maximum number of real numbers	*
C*									*
C* Output parameters:							*
C*	RLNOS (NREAL)	REAL		Real numbers entered		*
C*	NREAL		INTEGER		Number of real numbers		*
C*	IRET		INTEGER		Return code			*
C*					  3 = too many reals		*
C*					  2 = EXIT entered		*
C*					  1 = <CR> entered		*
C*					  0 = normal return		*
C*					 -3 = invalid input string	*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/84						*
C* M. desJardins/GSFC	 5/84	Corrected error flags			*
C* M. Goodman/RDS	 3/85	Corrected error with <cr>		*
C* M. desJardins/GSFC	 3/88	Cleaned up				*
C************************************************************************
	CHARACTER*(*) 	messg, string*80
	LOGICAL 	pagflg, newln
	REAL 		rlnos (1)
C------------------------------------------------------------------------
C*	Get user input.
C
	CALL TM_STR  ( messg, pagflg, newln, string, iret )
	IF  ( iret .gt. 0 )  RETURN
C
C*	Get array of reals.
C
	CALL ST_C2R  ( string, nexp, rlnos, nreal, ier )
	IF  ( ier .lt. 0 )  iret = -3
	IF  ( ier .eq. 1 )  iret =  3
C*
	RETURN
	END
