	SUBROUTINE GQMFIL  ( mapnam, iret )
C************************************************************************
C* GQMFIL								*
C*									*
C* This subroutine returns the current map file name to be used by	*
C* GDRMAP to draw a map.						*
C*									*
C* GQMFIL  ( MAPNAM, IRET )						*
C*									*
C* Output parameters:							*
C*	MAPNAM 		CHAR*   	Map file name 			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 3/85	GEMPLT Version 3.0			*
C* M. Goodman/RDS	 7/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* M. desJardins/GSFC	 3/89	Eliminate the attempts to build the	*
C*				map file name since this was never	*
C*				really implemented			*
C* M. desJardins/GSFC	 4/90	Eliminate check for map mode		*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE 	'XYDEF.CMN'
C*
	CHARACTER*(*) 	mapnam 
C------------------------------------------------------------------------
	iret   =  NORMAL
C
C*	Return the map file name from the common area.
C
	mapnam = mpfil
C*
	RETURN
	END
