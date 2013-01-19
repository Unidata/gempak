	SUBROUTINE HSLTYP  ( iltyp, iret )
C************************************************************************
C* HSLTYP - PS								*
C* 									*
C* This subroutine sets the hardware line type.				*
C*									*
C* HSLTYP  ( ILTYP, IRET )						*
C*									*
C* Input parameters:							*
C* 	ILTYP		INTEGER		 Line type			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/90						*
C* S. Jacobs/NCEP	 4/96	Changed to call PSLTYP			*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Set the hardware line type.
C
	CALL PSLTYP ( iltyp, lpat, lpscal, iret )
C*
	RETURN
	END
