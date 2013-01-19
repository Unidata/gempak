	SUBROUTINE HSCNAM  ( icolr, color, iret )
C************************************************************************
C* HSCNAM - XWP								*
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
C* S. Jacobs/NCEP	10/96	Added checks for sub-devices		*
C* S. Jacobs/NCEP	11/96	Added PSCNAM				*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE         'DEVCHR.CMN'
	INCLUDE         'ERROR.PRM'
C*
        CHARACTER*(*)   color
C-----------------------------------------------------------------------
	iret = NORMAL
C
C*	Check the requested device.
C*	Set the color name.
C
	IF  ( ( ddev .eq. 'XW' ) .or. ( ddev .eq. 'XWP' ) )  THEN
	    CALL ST_LSTR ( color, ilen, iret )
	    CALL XSCNAM ( 0, icolr, color, ilen, iret )
	  ELSE IF  ( ddev .eq. 'PS' )  THEN
	    CALL ST_LSTR ( color, ilen, iret )
	    CALL PSCNAM ( icolr, color, ilen, iret )
	END IF
C*
	RETURN
	END
