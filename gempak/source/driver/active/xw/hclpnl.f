	SUBROUTINE HCLPNL  ( xllf, yllf, xurf, yurf, iret )
C************************************************************************
C* HCLPNL - XW								*
C* 									*
C* This subroutine will clear a particular sub-region of the screen.	*
C*								 	*
C* HCLPNL  ( XLLF, YLLF, XURF, YURF, IRET )				*
C*									*
C* Input parameters:							*
C*	XLLF		REAL		Lower left  x fraction		*
C*	YLLF		REAL		Lower left  y fraction		*
C*	XURF		REAL		Upper right x fraction		*
C*	YURF		REAL		Upper right y fraction		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* K. Tyle/GSC		 2/97						*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	iret = 0
	ixll = xllf
	ixur = xurf
	iyll = yllf
	iyur = yurf
C
C*	Clear the panel.
C
	CALL XCLPNL ( ixll, iyll, ixur, iyur, iret )
C*
	RETURN
	END
