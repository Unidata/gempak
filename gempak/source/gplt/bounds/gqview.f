	SUBROUTINE GQVIEW  ( xllf, yllf, xurf, yurf, iret )	
C************************************************************************
C* GQVIEW								*
C* 									*
C* This subroutine returns the current view region boundaries.	 	*
C*									*
C* GQVIEW  ( XLLF, YLLF, XURF, YURF, IRET )				*
C*									*
C* Output parameters:							*
C*	XLLF		REAL		Lower left x fraction		*
C*	YLLF		REAL		Lower left y fraction		*
C*	XURF		REAL		Upper right x fraction		*
C*	YURF		REAL		Upper right y fraction		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Vilardo/RDS	10/84	GEMPLT Version 3.0			*
C* M. desJardins/GSFC	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
C------------------------------------------------------------------------
C*	Get view region boundary values from common.
C
	xllf = xbndlf
	yllf = ybndbf
	xurf = xbndrf
	yurf = ybndtf
	iret = NORMAL
C*
	RETURN
	END
