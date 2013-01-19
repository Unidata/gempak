	SUBROUTINE GQDARR  ( szdarw, szdarh, idarwd, idartp, iret )
C************************************************************************
C* GQDARR 								*
C* 									*
C* This subroutine returns the current directional arrow size, arrow  	*
C* head size, line width and wind arrow type.  				*
C*									*
C* GQDARR  ( SZDARW, SZDARH, IDARWD, IDARTP, IRET )			*
C*									*
C* Output parameters:							*
C* 	SZDARW		REAL	 	Wind arrow size multiplier	*
C*	SZDARH		REAL		Wind arrow head size multiplier	*
C*	IDARWD		INTEGER		Wind arrow line width 		*
C*	IDARTP		INTEGER		Wind arrow type			*
C*					  1 = plot arrow for calm wind	*
C*					  2 = don't plot for calm wind	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Durham/GSC	03/98						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVSET.CMN'
C------------------------------------------------------------------------
C*	If device has not been set, return an error.
C
	IF  ( ddev .eq. ' ' )  THEN
	    szdarw = 0.
	    iret   = NDVICE
C*
	  ELSE
C
C*	    Retrieve values from /DEVSET/.
C
	    szdarw = swdasz
	    szdarh = sdahsz
	    idarwd = ldarwd
	    idartp = ldartp
	    iret   = NORMAL
	END IF
C*
	RETURN
	END
