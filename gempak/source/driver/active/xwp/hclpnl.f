	SUBROUTINE HCLPNL  ( xllf, yllf, xurf, yurf, iret )
C************************************************************************
C* HCLPNL - XWP								*
C* 									*
C* This subroutine will clear a particular sub-region of the screen.	*
C*									*
C* HCLPNL  ( XLLF, YLLF, XURF, YURF, IRET )				*
C*									*
C* Input parameters:							*
C*	XLLF		REAL		Lower left  x ( device coord. )	*
C*	YLLF		REAL		Lower left  y ( device coord. )	*
C*	XURF		REAL		Upper right x ( device coord. )	*
C*	YURF		REAL		Upper right y ( device coord. )	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* K. Tyle/GSC		 2/97						*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
	iret = 0
	IF ( ddev .eq. 'XW' .or. ddev .eq. 'XWP' ) THEN
C
C*	    Convert coordinates to integers and clear the screen.
C
	    ixll = xllf
	    iyll = yllf
	    ixur = xurf
	    iyur = yurf
	    CALL XCLPNL ( ixll, iyll, ixur, iyur, iret )
	END IF
C*
	RETURN
	END
