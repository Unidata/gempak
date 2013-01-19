	SUBROUTINE UPDGDI  ( iret )
C************************************************************************
C* UPDGDI								*
C*									*
C* This subroutine updates the common areas for scaling grid linear	*
C* intermediate coordinates to grid coordinates.  This subroutine	*
C* is used for map coordinates.						*
C*									*
C* UPDGDI  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/88	Made into separate subroutine		*
C* M. Linda/GSC		 9/97	Changed a key word in the prologue	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
C------------------------------------------------------------------------
	IF  ( ( gpxrm .eq. gpxlm ) .or. ( gpytm .eq. gpybm ) )  THEN
	    iret  = NIPBND
	    mgset = .false.
	    RETURN
	  ELSE
	    iret = NORMAL
	END IF
C
C*	Compute scaling from grid to linear intermediate coordinates.
C
	aglx1  =  ( gxbndr - gxbndl ) / ( gpxrm - gpxlm )
	aglx0  =  gxbndl - gpxlm * aglx1
	agly1  =  ( gybndt - gybndb ) / ( gpytm - gpybm )
	agly0  =  gybndb - gpybm * agly1
C*
	RETURN
	END
