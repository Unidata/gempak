	SUBROUTINE DEPLOT  ( iret )
C************************************************************************
C* DEPLOT								*
C* 									*
C* This subroutine is called when a plot is complete.			*
C* 									*
C* DEPLOT ( IRET )							*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log									*
C* M. desJardins/GSFC	 5/88						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Call driver to end plot.
C
	CALL HEPLOT  ( iret )
C*
	RETURN
	END
