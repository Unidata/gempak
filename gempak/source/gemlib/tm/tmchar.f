	SUBROUTINE TM_CHAR  ( messg, pagflg, newln, nexp, chrstr, 
     +			      nstr,  iret )
C************************************************************************
C* TM_CHAR								*
C*									*
C* This subroutine writes a message to the user's terminal followed 	*
C* by  'or type EXIT'.  The phrase '<CR> to page' may also be added.	*
C* An array of character strings entered by the user is returned.	*
C*									*
C* TM_CHAR  ( MESSG, PAGFLG, NEWLN, NEXP, CHRSTR, NSTR, IRET )		*
C*									*
C* Input parameters:							*
C*	MESSG		CHAR*		Message				*
C*	PAGFLG		LOGICAL		Flag to add '<CR> to page'	*
C*	NEWLN		LOGICAL	 	Flag to move to new line	*
C*	NEXP		INTEGER	 	Maximum # of strings to return	*
C*									*
C* Output parameters:							*
C*	CHRSTR (NSTR)	CHAR*	 	User input strings		*
C*	NSTR		INTEGER		Number of strings returned	*
C*	IRET		INTEGER		Return code			*
C*					  3 = too many strings		*
C*					  2 = EXIT entered		*
C*					  1 = <CR> entered		*
C*					  0 = normal return 		*
C**									*
C* Log:									*
C* I. Graffman/RDS	5/84						*
C* M. Goodman/RDS	3/85	Corrected error with <CR>		*
C* M. desJardins/GSFC	3/88	Cleaned up				*
C* S. Schotz/GSC	7/90	Call ST_CLST instead of ST_C2C		*
C************************************************************************
	CHARACTER*(*) 	messg, chrstr (1), string*80
	LOGICAL 	pagflg, newln
C------------------------------------------------------------------------
C*	Get user input.
C
	CALL TM_STR  ( messg, pagflg, newln, string, iret )
	IF  ( iret .gt. 0 )  RETURN
C
C*	Break input string into array.
C
	CALL ST_CLST  ( string, ' ', ' ', nexp, chrstr, nstr, ier )
	IF  ( ier .eq. 1 )  iret = 3
C*
	RETURN
	END
