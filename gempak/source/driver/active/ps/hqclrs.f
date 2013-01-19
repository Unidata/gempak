	SUBROUTINE HQCLRS  ( ibank, ncolr, iret )
C************************************************************************
C* HQCLRS - PS								*
C* 									*
C* This subroutine gets the number of colors in a color bank.		*
C* 									*
C* HQCLRS  ( IBANK, NCOLR, IRET )					*
C* 									*
C* Input parameters:							*
C*	IBANK		INTEGER		Color bank			*
C*									*
C* Output parameters:							*
C*	NCOLR		INTEGER		Number of colors		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 1/97						*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		icolrs (256)
C*-----------------------------------------------------------------------
	CALL PQCLRS ( ibank, ncolr, icolrs, iret )
C*
	RETURN
	END
