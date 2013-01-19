      SUBROUTINE GCLEAR  ( iret )
C************************************************************************
C* GCLEAR								*
C* 									*
C* This subroutine clears the current device.  On a direct-access 	*
C* device, the screen is erased.  On a continuous-paper plotter, the 	*
C* paper is advanced to the next page.  On a single-page plotter, the 	*
C* paper is unloaded and another sheet can be entered. 			*
C* 									*
C* GCLEAR  ( IRET )							*
C* 									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/88	Documentation				*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into the buffer.
C
	isend (1) = 2
	isend (2) = FCLEAR
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL)  iret = ierr
C
	RETURN
	END
