	SUBROUTINE LC_SBND  ( rltln, iret )
C************************************************************************
C* LC_SBND								*
C*									*
C* This subroutine sets the latitude/longitude bounds of a geographic	*
C* area.  Once this subroutine has been called, the subroutine LC_INBN	*
C* may be called to check whether a latitude/longitude location is	*
C* within the specified range.						*
C*									*
C* LC_SBND ( RLTLN, IRET )						*
C*									*
C* Input parameters:							*
C*	RLTLN (4)	REAL		Lower left, upper right lat/lon	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return 		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	6/84						*
C************************************************************************
	REAL  		rltln (4)
	INCLUDE 	'lcbnd.cmn'
C-----------------------------------------------------------------
	rllbnd (1) = rltln(1)
	rllbnd (2) = rltln(2)
	rllbnd (3) = rltln(3)
	rllbnd (4) = rltln(4)
C
	iret = 0
C*
	RETURN
	END
