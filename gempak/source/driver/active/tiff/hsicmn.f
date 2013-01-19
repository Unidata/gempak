	SUBROUTINE HSICMN ( iret )
C************************************************************************
C* HSICMN - TIFF							*
C*									*
C* This subroutine sets image common information 			*
C*									*
C* HSICMN (IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 9/00						*
C************************************************************************
	INCLUDE		'IMGDEF.CMN'
C------------------------------------------------------------------------
C*	Pass the first variable of the common block
C
	CALL TSICMN ( imftyp, iret )
C
	RETURN
	END
