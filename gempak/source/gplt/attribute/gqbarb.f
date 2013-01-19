	SUBROUTINE GQBARB  ( szbarb, ibrwid, ibrtyp, iret )
C************************************************************************
C* GQBARB								*
C*									*
C* This subroutine returns the current wind barb size, line width, 	*
C* and barb type.  							*
C*									*
C* NOTE THAT THE CALLING SEQUENCE OF THIS SUBROUTINE HAS CHANGED FROM	*
C* PREVIOUS VERSIONS OF GEMPAK.						*
C*									*
C* GQBARB  ( SZBARB, IBRWID, IBRTYP, IRET )				*
C*									*
C* Output parameters:							*
C*	SZBARB		REAL		Wind barb size multiplier	*
C*	IBRWID		INTEGER		Wind barb line width 		*
C*	IBRTYP		INTEGER		Wind barb type 			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* S. Schotz/GSC	 1/90	Added wind barb width and type		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
C* 	If device has not been set, return an error.
C
	IF  ( ddev .eq. ' ' )  THEN
	    szbarb = 0
	    iret   = NDVICE
	  ELSE
C
C*	    Retrieve values from /DEVSET/.
C
	    szbarb = swbsz
	    ibrwid = lbrwid
	    ibrtyp = lbrtyp
	    iret   = NORMAL
	END IF
C*
	RETURN
	END
