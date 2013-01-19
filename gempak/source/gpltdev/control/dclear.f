      SUBROUTINE DCLEAR  ( iret )
C************************************************************************
C* DCLEAR								*
C* 									*
C* This subroutine clears the current device.  On a direct access 	*
C* device, DCLEAR erases the screen.  On a continous paper plotter, 	*
C* DCLEAR will advance  to the next page.  On a single page plotter, 	*
C* DCLEAR will unload the paper so another sheet can be loaded. 	*
C* 									*
C* DCLEAR  ( IRET )							*
C* 									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/88	Documentation				*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into the buffer.
C
	isend (1) = 2
	isend (2) = CCLEAR
C
	CALL GPUT ( isend, 2, iret )
	IF ( iret .ne. NORMAL ) RETURN
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
C
	RETURN
	END
