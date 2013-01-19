	SUBROUTINE HSBRGB  ( icbank, icolr, ired, igreen, iblue, iret )
C************************************************************************
C* HSBRGB - PS								*
C*									*
C* This subroutine defines the color components of a color by		*
C* specifying the values of red, green, and blue.  The color components	*
C* must be in the range 0 - 255.					*
C*									*
C* HSBRGB  ( ICBANK, ICOLR, IRED, IGREEN, IBLUE, IRET )			*
C*									*
C* Input parameters:							*
C* 	ICBANK		INTEGER		Color bank ID 			*
C* 	ICOLR		INTEGER		Color number			*
C*	IRED		INTEGER		Red color component 		*
C*	IGREEN		INTEGER		Green color component		*
C*	IBLUE		INTEGER		Blue color component		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 1/97						*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
C------------------------------------------------------------------------
C*	Set the color components.
C
	CALL PSCRGB ( icbank, icolr, ired, igreen, iblue, iret )
C*
	RETURN
	END
