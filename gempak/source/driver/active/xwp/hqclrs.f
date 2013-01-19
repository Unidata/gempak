	SUBROUTINE HQCLRS  ( ibank, ncolr, iret )
C************************************************************************
C* HQCLRS - XWP								*
C* 									*
C* This subroutine gets the number of colors in a color bank.		*
C* 									*
C* HQCLRS  ( IBANK, NCOLR, IRET )					*
C* 									*
C* Input parameters:							*
C*	IBANK		INTEGER		Color bank			*
C*									*
C* Output parameters:							*
C*	NCOLR		INTEGER		Number of colors		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Cowie/COMET	11/95						*
C* S. Jacobs/NCEP	10/96	Added checks for sub-devices		*
C* S. Jacobs/NCEP	 1/97	Added PQCLRS				*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		icolrs (256)
C*-----------------------------------------------------------------------
	iret = NORMAL
C
C*	Check the requested device.
C
	IF  ( ( ddev .eq. 'XW' ) .or. ( ddev .eq. 'XWP' ) )  THEN
	    CALL XQCLRS ( ibank, ncolr, icolrs, iret )
	  ELSE IF  ( ddev .eq. 'PS' )  THEN
	    CALL PQCLRS ( ibank, ncolr, icolrs, iret )
	END IF
C*
	RETURN
	END
