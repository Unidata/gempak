	SUBROUTINE HSCRGB  ( icolr, ired, igreen, iblue, iret )
C************************************************************************
C* HSCRGB - GN								*
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
C* G. Krueger/EAI	11/95	Changed RGB range from 0-1 to 0-255.	*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'ERROR.PRM'
C-----------------------------------------------------------------------
	iret = NORMAL
C*
	RETURN
	END
