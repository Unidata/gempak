	SUBROUTINE TM_STR  ( messg, pagflg, newln, string, iret )
C************************************************************************
C* TM_STR								*
C*									*
C* This subroutine writes a message to the user's terminal followed	*
C* by 'or type EXIT'.  The phrase '<CR> to page' may also be added.	*
C* The string entered by the user is returned.				*
C*									*
C* TM_STR  ( MESSG, PAGFLG, NEWLN, STRING, IRET )			*
C*									*
C* Input parameters:							*
C*	MESSG		CHAR*		Message				*
C*	PAGFLG		LOGICAL		Flag to add '<CR> to page'	*
C*	NEWLN		LOGICAL		Flag to move to new line	*
C*									*
C* Output parameters:							*
C*	STRING		CHAR*		String entered			*
C*	IRET		INTEGER		Return code			*
C*					  2 = EXIT entered		*
C*					  1 = <CR> entered		*
C*					  0 = normal return 		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/84						*
C* M. desJardins/GSFC	 3/88	Cleaned up				*
C************************************************************************
	CHARACTER*(*)	messg, string
	LOGICAL 	pagflg, newln
C------------------------------------------------------------------------
C*	Prompt the user for input.
C
	CALL TM_PROM  ( messg, pagflg, newln, iret )
C
C*	Get string from user.
C
	CALL TM_RCHR  ( string, iret )
C*
	RETURN
	END
