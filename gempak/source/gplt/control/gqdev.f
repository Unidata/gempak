	SUBROUTINE GQDEV  ( device, iunit, iatyp, iret )
C************************************************************************
C* GQDEV								*
C* 									*
C* This subroutine returns the current plot device identifier, unit	*
C* number and access type. If no device is set, a blank is returned. 	*
C* DEVICE has traditionally been a 2 character name, but may now	*
C* contain up to 12 characters.						*
C*									*
C* GQDEV  ( DEVICE, IUNIT, IATYP, IRET )				*
C*									*
C* Output parameters:							*
C* 	DEVICE		CHAR*		Plot device name		*
C* 	IUNIT		INTEGER		Not used			*
C*	IATYP		INTEGER 	Device access type		*
C*					   1 = direct access		*
C*					   2 = sequential access	*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Chatters/RDS	 8/84	GEMPLT Version 3.0			*
C* M. desJardins/GSFC	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/88	Documentation				*
C************************************************************************
	CHARACTER*(*)	device
C*
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Get device, unit number and access type from table (COMMON).
C
	device = ddev
	iunit  = niunit
	iatyp  = ndtyp
C*
	RETURN
	END
