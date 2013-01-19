	SUBROUTINE GQLINE  ( iltyp, ilthw, iwidth, iwhw, iret )
C************************************************************************
C* GQLINE								*
C*									*
C* This subroutine returns the current line attributes including the 	*
C* line type number, the software/hardware line type flag, the line 	*
C* width size multiplier and the software/hardware line width flag.  	*
C*									*
C* GQLINE  ( ILTYP, ILTHW, IWIDTH, IWHW, IRET )				*
C*									*
C* Output parameters:							*
C*	ILTYP		INTEGER		Line type number		*
C*					  0 = no change			*
C*	ILTHW		INTEGER		Line type flag			*
C*					  1 = software			*
C*					  2 = hardware			*
C*	IWIDTH		INTEGER		Line width			*
C*	IWHW		INTEGER		Line width flag			*
C*					  1 = software			*
C*					  2 = hardware			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	5/85	GEMPLT Version 3.1			*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
C* 	IF device has not been set, return an error.
C
	IF  ( ddev .eq. ' ' )  THEN
	    iltyp  = 0
	    ilthw  = 0
	    iwidth = 0
	    iwhw   = 0
	    iret   = NDVICE
	  ELSE
C
C*	    Retrieve values from /DEVSET/.
C
	    iltyp  = lltyp
	    ilthw  = llthw
	    iwidth = llwid
	    iwhw   = llwhw
	    iret   = NORMAL
	END IF
C*
	RETURN
	END
