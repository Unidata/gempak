	SUBROUTINE HSCOLB  ( icbank, icolr, iret )
C************************************************************************
C* HSCOLB - XW								*
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
C* J. Cowie/COMET	 5/95	After HSCOLR ()				*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
C------------------------------------------------------------------------
C*	Set this as the current color.
C
	CALL XSCOLR ( icbank, icolr, iret )
C*
	RETURN
	END
