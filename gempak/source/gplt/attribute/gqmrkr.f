	SUBROUTINE GQMRKR  ( imark, imkhw, szmark, imkwid, iret )
C************************************************************************
C* GQMRKR								*
C*									*
C* This subroutine returns the current marker attributes including 	*
C* the marker number, the hardware/software flag and the marker 	*
C* size and line width.							*
C*									*
C* NOTE THAT THE CALLING SEQUENCE OF THIS SUBOUTINE HAS CHANGED FROM	*
C* PREVIOUS VERSIONS OF GEMPAK.						*
C*									*
C* GQMRKR  ( IMARK, IMKHW, SZMARK, IMKWID, IRET )			*
C*									*
C* Output parameters:							*
C*	IMARK		INTEGER		Marker number			*
C*	IMKHW		INTEGER		Hardware/software flag		*
C*					  1 = software			*
C*					  2 = hardware			*
C*	SZMARK		REAL		Marker size multiplier		*
C*	IMKWID		INTEGER		Marker width multiplier		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* S. Schotz/GSC	 1/90	Added marker width			*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
C* 	If device has not been set, return an error.
C
	IF  ( ddev .eq. ' ' )  THEN
	    imark  = 0
	    imkhw  = 0
	    szmark = 0
	    imkwid = 0
	    iret   = NDVICE
	  ELSE
C
C*	    Retrieve values from /DEVSET/.
C
	    imark  = lmark
	    imkhw  = lmkhw
	    szmark = smksz
	    imkwid = lmkwid
	    iret   = NORMAL
	END IF
C*
	RETURN
	END
