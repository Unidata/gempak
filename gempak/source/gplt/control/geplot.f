	SUBROUTINE GEPLOT  ( iret )
C************************************************************************
C* GEPLOT								*
C* 									*
C* This subroutine is called when a plot sequence is completed. 	*
C* Internal plot buffers will be flushed by this subroutine and the	*
C* terminal will be put in user mode.					*
C* 									*
C* GEPLOT  ( IRET )							*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log									*
C* M. desJardins/GSFC	 5/88						*
C* M. Linda/GSC		 2/97	Removed GFLUSH and PLOTBF.CMN		*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
	IF  ( ddev .ne. ' ' )  THEN
C
	    CALL DEPLOT  ( iret )
	END IF
C*
	RETURN
	END
