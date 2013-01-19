	SUBROUTINE HLOGO ( x, y, size, iclmod, ilogo, iret )
C************************************************************************
C* HLOGO - GN								*
C*									*
C* This subroutine draws the specified emblem.				*
C*									*
C* HLOGO ( X, Y, SIZE, ICLMOD, ILOGO, IRET )				*
C*									*
C* Input parameters:							*
C*	X		REAL		X coordinate in device units	*
C*	Y		REAL		Y coordinate in device units	*
C*	SIZE		REAL		Emblem size			*
C*	ICLMOD		INTEGER		Emblem color mode		*
C*					    '1' - monochrome		*
C*					    '2' - color			*
C*	ILOGO		INTEGER		Emblem ID			*
C*					    1 = NOAA			*
C*					    2 = NWS			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Linda/GSC		 7/97	Original				*
C* A. Hardy/GSC		 5/00   Added color mode parameter		*
C* S. Jacobs/NCEP	 4/01	Added ilogo to the calling sequence	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C*
C------------------------------------------------------------------------
	iret = NORMAL
C*
	RETURN
	END
