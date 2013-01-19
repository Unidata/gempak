	SUBROUTINE HSCOLB  ( icbank, icolr, iret )
C************************************************************************
C* HSCOLB - XWP								*
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
C* S. Jacobs/NCEP	10/96	Added checks for sub-devices		*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* C. Lin/EAI            2/98   Added call to PSCOLR        		*
C************************************************************************
	INCLUDE         'DEVCHR.CMN'
	INCLUDE         'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check the requested device.
C*	Set this as the current color.
C
	IF  ( ( ddev .eq. 'XW' ) .or. ( ddev .eq. 'XWP' ) )  THEN
	    CALL XSCOLR ( icbank, icolr, iret )
          ELSE IF  ( ddev .eq. 'PS' )  THEN
            CALL PSCOLR ( icbank, icolr, iret )
	END IF
C*
	RETURN
	END
