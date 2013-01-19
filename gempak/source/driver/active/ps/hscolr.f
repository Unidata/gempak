	SUBROUTINE HSCOLR  ( icolr, iret )
C************************************************************************
C* HSCOLR - PS								*
C* 									*
C* This subroutine sets the color on a graphics device.			*
C* 									*
C* HSCOLR  ( ICOLR, IRET )						*
C* 									*
C* Input parameters:							*
C*	ICOLR		INTEGER		Color number			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Nielsen/SUNYA	 3/91	Add colors				*
C* M. desJardins/NMC	12/91	Use symbolic command; check for NNCOLR	*
C* G. Krueger/EAI	12/95	Changed RGB range from 0-1 to 0-255.	*
C* E. Wehner/EAi	3/96	Modified to call the C version		*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* S. Jacobs/NCEP	 2/98	Changed call sequence for PSCOLR	*
C************************************************************************
C------------------------------------------------------------------------
C*	Set the current color.
C
	icbank = 0
	CALL PSCOLR ( icbank, icolr, iret )
C*
	RETURN
	END
