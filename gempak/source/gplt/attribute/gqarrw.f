	SUBROUTINE GQARRW  ( szarrw, szarrh, iarwid, iartyp, iret )
C************************************************************************
C* GQARRW 								*
C* 									*
C* This subroutine returns the current wind arrow size, arrow head 	*
C* size, line width and wind arrow type.  				*
C*									*
C* NOTE THAT THE CALLING SEQUENCE OF THIS SUBROUTINE HAS CHANGED FROM	*
C* PREVIOUS VERSIONS OF GEMPAK.						*
C* 									*
C* GQARRW  ( SZARRW, SZARRH, IARWID, IARTYP, IRET )			*
C*									*
C* Output parameters:							*
C* 	SZARRW		REAL	 	Wind arrow size multiplier	*
C*	SZARRH		REAL		Wind arrow head size multiplier	*
C*	IARWID		INTEGER		Wind arrow line width 		*
C*	IARTYP		INTEGER		Wind arrow type			*
C*					  1 = plot arrow for calm wind	*
C*					  2 = don't plot for calm wind	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* S. Schotz/GSC	 1/90	Added wind arrow width and type		*
C* S. Schotz/GSC	 8/90	Added arrow head size			*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVSET.CMN'
C------------------------------------------------------------------------
C*	If device has not been set, return an error.
C
	IF  ( ddev .eq. ' ' )  THEN
	    szarrw = 0.
	    iret   = NDVICE
C*
	  ELSE
C
C*	    Retrieve values from /DEVSET/.
C
	    szarrw = swasz
	    szarrh = swahsz
	    iarwid = larwid
	    iartyp = lartyp
	    iret   = NORMAL
	END IF
C*
	RETURN
	END
