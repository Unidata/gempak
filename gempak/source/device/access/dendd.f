	SUBROUTINE DENDD ( ieop, iret )
C************************************************************************
C* DENDD								*
C*									*
C* This subroutine must be the last subroutine called by any program 	*
C* that uses GEMPLT.  If the device process is to be stopped, any open	*
C* files will be closed.						*
C*									*
C* DENDD  ( IEOP, IRET )						*
C*									*
C* Input parameters:							*
C*	IEOP		INTEGER		End plotting flag		*
C*					  0 = retain subprocess 	*
C*					  1 = stop subprocess		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:                                                                 *
C* M. desJardins/GSFC	 4/85	GEMPLT 3.1				*
C* M. desJardins/GSFC	 5/88	Documentation				*
C* S. Jacobs/NCEP	 4/96	Update call to DCLOSP			*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Make sure that files are all closed if device driver is exiting.
C
	IF  ( ieop .eq. 1 )  CALL DCLOSP ( nwin, ier )
	CALL HENDD  ( ieop, iret )
C*
	RETURN
	END
