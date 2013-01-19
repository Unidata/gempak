	SUBROUTINE HOPEN  ( iret )
C************************************************************************
C* HOPEN - PA								*
C*									*
C* This subroutine opens a plot file for the device.			*
C*									*
C* HOPEN  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins	12/90						*
C* J. Nielsen/SUNYA	 3/91	Modify scaling to fit on page		*
C* D. Burks/CSU		 4/91	Set line cap to give rounded edges	*
C* A. Chang/EAI		 2/94	Modify to call C routine		*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
C------------------------------------------------------------------------
	CALL PSOPEN ( iret )
C*
	RETURN
	END
