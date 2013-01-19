	SUBROUTINE HSCOLB  ( icbank, icolr, iret )
C************************************************************************
C* HSCOLB - PS								*
C* 									*
C* This subroutine sets the color in a color bank.			*
C* 									*
C* HSCOLB  ( ICBANK, ICOLR, IRET )					*
C* 									*
C* Input parameters:							*
C*	ICBANK		INTEGER		Color bank			*
C*	ICOLR		INTEGER		Color number			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 2/98	Copied from HSCOLR			*
C************************************************************************
C------------------------------------------------------------------------
C*	Set the current color.
C
	CALL PSCOLR ( icbank, icolr, iret )
C*
	RETURN
	END
