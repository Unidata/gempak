	SUBROUTINE HOPEN  ( iret )
C************************************************************************
C* HOPEN - GN								*
C*									*
C* This subroutine opens a plot file for the device.  This subroutine	*
C* will be called only by other H subroutines and is used for devices	*
C* which send plotting commands to a plot file.				*
C*									*
C* HOPEN  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C*
	RETURN
	END
