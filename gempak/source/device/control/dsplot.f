	SUBROUTINE DSPLOT  ( iret )
C************************************************************************
C* DSPLOT								*
C* 									*
C* This subroutine is called when a plot is started.			*
C* 									*
C* DSPLOT  ( IRET )							*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/88						*
C* A. Chang/EAI	 	12/93						*
C* S. Jacobs/NMC	 6/94		Added call to HSPLOT		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Call driver to start plot.
C
	CALL HSPLOT ( iret )
C*
	RETURN
	END
