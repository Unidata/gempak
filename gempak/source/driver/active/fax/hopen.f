	SUBROUTINE HOPEN  ( iret )
C************************************************************************
C* HOPEN - FAX								*
C*									*
C* This subroutine opens a plot file for the device.			*
C*									*
C* HOPEN  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* E. Wehner/EAi	7/96	Adopted to call Raster C routines	*
C* S. Maxwell/GSC       6/97    Documentation changes                   *
C* S. Jacobs/NCEP	 7/97	Renamed RSOPEN to ROPEN			*
C************************************************************************
C------------------------------------------------------------------------
	CALL ROPEN ( iret )
C*
	RETURN
	END
