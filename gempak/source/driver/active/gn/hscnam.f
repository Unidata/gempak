	SUBROUTINE HSCNAM  ( icolr, color, iret )
C************************************************************************
C* HSCNAM - GN								*
C*									*
C* This subroutine defines the color by color name.  Color name can be	*
C* Gempak color name, abbreviation, and X color name.			*
C*									*
C* HSCNAM  ( ICOLR, COLOR, IRET )					*
C*									*
C* Input parameters:							*
C* 	ICOLR		INTEGER		Color number			*
C*	COLOR		CHAR*		GEMPAK color name		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* L. Williams/EAI	 3/96						*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'ERROR.PRM'
C*
        CHARACTER*(*)   color
C-----------------------------------------------------------------------
	iret = NORMAL 
C*
	RETURN
	END
