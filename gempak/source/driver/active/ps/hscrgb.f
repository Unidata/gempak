	SUBROUTINE HSCRGB  ( icolr, ired, igreen, iblue, iret )
C************************************************************************
C* HSCRGB - PS								*
C*									*
C* This subroutine defines the color components of a color by		*
C* specifying the values of red, green, and blue.  The color components	*
C* must be in the range 0 - 255.					*
C*									*
C* HSCRGB  ( ICOLR, IRED, IGREEN, IBLUE, IRET )				*
C*									*
C* Input parameters:							*
C* 	ICOLR		INTEGER		Color number			*
C*	IRED		INTEGER		Red color component		*
C*	IGREEN		INTEGER		Green color component		*
C*	IBLUE		INTEGER		Blue color component		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* M. desJardins/NMC	12/91	Add reset flag for color components	*
C* G. Krueger/EAI	11/95	Changed RGB range from 0-1 to 0-255.	*
C* S. Jacobs/NCEP	 4/96	Modified to call C routine		*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
C-----------------------------------------------------------------------
	iret = 0
C
	CALL PSCRGB ( 0, icolr, ired, igreen, iblue, iret )
C*
	RETURN
	END
