	SUBROUTINE GSCRGB ( icolr, ired, igreen, iblue, iret )
C************************************************************************
C* GSCRGB								*
C*									*
C* This subroutine defines the color components of a color by		*
C* specifying the values of red, green, and blue.  The color components	*
C* must be in the range 0 - 255.					*
C*									*
C* GSCRGB  ( ICOLR, IRED, IGREEN, IBLUE, IRET )				*
C*									*
C* Input parameters:							*
C* 	ICOLR		INTEGER		Color number			*
C*	IRED		INTEGER		Red color component		*
C*	IGREEN		INTEGER		Green color component		*
C*	IBLUE		INTEGER		Blue color component		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 8/85						*
C* M. desJardins/GSFC	 5/88						*
C* G. Krueger/EAI	11/95	Changed RGB range from 0-1 to 0-255.	*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
C-----------------------------------------------------------------------
	CALL DSCRGB  ( icolr, ired, igreen, iblue, iret )
C*
	RETURN
	END
