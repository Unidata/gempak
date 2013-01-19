	SUBROUTINE DCLPNL  ( xllf, yllf, xurf, yurf, iret )
C************************************************************************
C* DCLPNL								*
C* 									*
C* This subroutine will clear a particular sub-region of the screen.	*
C* The sub-region is specified using fractions of the available area on	*
C* the plot device.  							*
C*									*
C* DCLPNL  ( XLLF, YLLF, XURF, YURF, IRET )				*
C*									*
C* Input parameters:							*
C*	XLLF		REAL		Lower left x  ( device coord. )	*
C*	YLLF		REAL		Lower left y  ( device coord. )	*
C*	XURF		REAL		Upper right x ( device coord. )	*
C*	YURF		REAL		Upper right y ( device coord. )	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* K. Tyle/GSC		 2/97						*
C************************************************************************
	iret = 0
C
C*      Call driver to clear panel.
C
	CALL HCLPNL ( xllf, yllf, xurf, yurf, iret )
C*
	RETURN
	END
