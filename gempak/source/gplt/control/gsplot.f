	SUBROUTINE GSPLOT  ( iret )
C************************************************************************
C* GSPLOT								*
C* 									*
C* This subroutine is called when a plot sequence is started. 		*
C* 									*
C* GSPLOT  ( IRET )							*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log									*
C* M. desJardins/GSFC	 5/88						*
C* A. Chane/EAI	 	12/93						*
C* M. Linda/GSC		 2/97	Removed PLOTBF.CMN			*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
	IF  ( ddev .ne. ' ' )  THEN
C
C*	    Now, start plotting.
C
	    CALL DSPLOT  ( iret )
	END IF
C*
	RETURN
	END
