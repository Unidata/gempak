	SUBROUTINE HOPEN  ( iret )
C************************************************************************
C* HOPEN - RBK								*
C*									*
C* This subroutine opens a plot file for the device.			*
C*									*
C* HOPEN  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* A. Hardy/GSC		9/98		Modified from utf's HOPEN       *
C************************************************************************
C------------------------------------------------------------------------
	CALL AOPEN ( iret )
C*
	RETURN
	END
