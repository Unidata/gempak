	SUBROUTINE GSTICK  ( itktyp, sztick, iret )
C************************************************************************
C* GSTICK								*
C* 									*
C* This subroutine sets the tick attributes including the tick type	*
C* and size.								*
C* 									*
C* GSTICK  ( ITKTYP, SZTICK, IRET )					*
C*									*
C* Input parameters:							*
C*	ITKTYP		INTEGER		Tick type			*
C*					  0 = no change 		*
C*					  1 = major tick outside	*
C*					  2 = major tick inside		*
C*					  3 = minor tick outside	*
C*					  4 = minor tick inside		*
C*					  5 = major tick out and inside	*
C*					  6 = minor tick out and inside	*
C*	SZTICK		REAL		Tick size multiplier		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/89	GEMPAK 5				*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVREQ.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	This information is only used in GPLT so it can just be set.
C
	IF  ( ( itktyp .ge. 1 ) .and. ( itktyp .le. 6 ) )  THEN
	    ktktyp = itktyp
	END IF
	IF  ( sztick .gt. 0. )  THEN
	    rticsz = sztick
	END IF
C*
	RETURN
	END
