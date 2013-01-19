	SUBROUTINE DQLPAT  ( ilpat, iret )
C************************************************************************
C* DQLPAT								*
C* 									*
C* This subroutine returns the current software line pattern for the 	*
C* current line type. 							*
C*									*
C* DQLPAT  ( ILPAT, IRET )						*
C*									*
C* Output parameters:							*
C*	ILPAT( 8 )	INTEGER		Line pattern values		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Vilardo/RDS	11/84	GEMPLT Version 3.0                      *
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/89	Documentation				*
C* S. Schotz/GSC	 8/90	Removed line type number as input	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DVWNDW.CMN'
C*
	INTEGER		ilpat (*)
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Return active pattern after descaling.
C
	DO  i = 1, 8
	    ilpat (i) = nint (actpat (i) / lpscal)
	END DO
C*
	RETURN
	END
