	SUBROUTINE HSICMN ( iret )
C************************************************************************
C* HSICMN - PS								*
C*									*
C* This subroutine sets image common information 			*
C*									*
C* HSICMN (IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 1/97						*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'IMGDEF.CMN'
C------------------------------------------------------------------------
C*	Pass the first variable of the common block
C
	CALL PSICMN ( imftyp, iret )
C
	RETURN
	END
