	SUBROUTINE HOPEN  ( iret )
C************************************************************************
C* HOPEN - TIFF								*
C*									*
C* This subroutine opens a plot file for the device.			*
C*									*
C* HOPEN  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	12/98						*
C************************************************************************
C------------------------------------------------------------------------
	CALL TSOPEN ( iret )
C*
	RETURN
	END
