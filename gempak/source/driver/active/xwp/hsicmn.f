	SUBROUTINE HSICMN ( iret )
C************************************************************************
C* HSICMN - XWP								*
C*									*
C* This subroutine sets image common information 			*
C*									*
C* HSICMN ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Cowie/COMET	 3/95						*
C* S. Jacobs/NCEP	10/96	Added checks for sub-devices		*
C* S. Jacobs/NCEP	 1/97	Added PSICMN				*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE         'DEVCHR.CMN'
	INCLUDE         'ERROR.PRM'
	INCLUDE		'IMGDEF.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check the requested device.
C*	Pass the first variable of the common block.
C
	IF  ( ( ddev .eq. 'XW' ) .or. ( ddev .eq. 'XWP' ) )  THEN
	    CALL XSICMN ( imftyp, iret )
	  ELSE IF  ( ddev .eq. 'PS' )  THEN
	    CALL PSICMN ( imftyp, iret )
	END IF
C*
	RETURN
	END
