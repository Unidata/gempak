	SUBROUTINE HSBRGB  ( icbank, icolr, ired, igreen, iblue, iret )
C************************************************************************
C* HSBRGB - GN								*
C* 									*
C* This subroutine defines the color components of a color by		*
C* specifying the values of red, green, and blue.  The color components	*
C* must be in the range 0 - 255.					*
C*									*
C* HSBRGB  ( ICBANK, ICOLR, IRED, IGREEN, IBLUE, IRET )			*
C* 									*
C* Input parameters:							*
C*	ICBANK		INTEGER		Color bank ID			*
C*	ICOLR		INTEGER		Color number			*
C*	IRED		INTEGER		Red color component		*
C*	IGREEN		INTEGER		Green color component		*
C*	IBLUE		INTEGER		Blue color component		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* C. Lin/EAI	 	5/95	After HSCOLB ()				*
C* G. Krueger/EAI	12/95	Changed RGB range from 0-1 to 0-255.	*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* T. Piper/SAIC	1/02	Set iret				*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
C*
	iret = NORMAL
C*
	RETURN
	END
