      SUBROUTINE DROAM  ( ityp, x, y, iret )
C************************************************************************
C* DROAM								*
C* 									*
C* This subroutine roams the current window to the specified position   *
C* in any coordinate system except 'S'. The base point of the roam can  *
C* be upper left of the screen or the center of the screen.             *
C* 									*
C* DROAM  ( ITYP, X, Y, IRET )						*
C* 									*
C* Input parameters:                                                    *
C*      ITYP            INTEGER         The base point of roam          *
C*                                        0 = upper left screen corner  *
C*                                        1 = center of the screen      *
C*                                        2 = delta_x, delta_y      	*
C*      X          REAL         Upper left window x coordinate (device) *
C*      Y          REAL         Upper left window y coordinate (device) *
C*                                                                      *
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* C. Lin/EAI	 	6/97						*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C
	INTEGER		isend (3)
	REAL		rsend (2)
C------------------------------------------------------------------------
C*	Load input parameters into the buffer.
C
	isend (1) = 5
	isend (2) = CROAM
	isend (3) = ityp
	CALL GPUT ( isend, 3, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	rsend (1) = x
	rsend (2) = y
C
	CALL GPUTR ( rsend, 2, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
C
	RETURN
	END
