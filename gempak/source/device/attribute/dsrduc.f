	SUBROUTINE DSRDUC ( filter, rfilt, iret )
C************************************************************************
C* DSRDUC								*
C*									*
C* This subroutine sets the filter factor for the point reduction	*
C* scheme.								*
C*									*
C* DSRDUC ( FILTER, RFILT, IRET )					*
C*									*
C* Input parameters:							*
C*	FILTER		REAL		Filter factor for pnt reduction	*
C*									*
C* Output parameters:							*
C*	RFILT		REAL		Filter factor for pnt reduction	*
C*	IRET		INTEGER		Return code			*
C**									*
C* S. Jacobs/NCEP	 5/99						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C*
	IF  ( filter .ge. 0. )  trfilt = filter
C
	rfilt = trfilt
C*
	RETURN
	END
