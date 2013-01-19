	SUBROUTINE HSMRKR  ( imark, szmark, iret )
C************************************************************************
C* HSMRKR - GN								*
C* 									*
C* This subroutine sets the marker number and size for hardware		*
C* markers.  For some devices, the marker number and size information	*
C* must be saved in a common area to be retrieved when the markers	*
C* are drawn.  This subroutine will be called only when hardware	*
C* markers are available.						*
C*									*
C* HSMRKR  ( IMARK, SZMARK, IRET )					*
C* 									*
C* Input parameters:							*
C* 	IMARK		INTEGER		Marker number			*
C*	SZMARK		REAL		Marker size multiplier		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C*
	RETURN
	END
