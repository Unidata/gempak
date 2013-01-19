	SUBROUTINE GCLPNL  ( xllf, yllf, xurf, yurf, iret )
C************************************************************************
C* GCLPNL								*
C* 									*
C* This subroutine clears a sub-region of the screen.  The sub-region   *
C* is specified as a fraction of the available area on the plot device. *
C* The point (0., 0.) is the lower left corner of the device; (1., 1.)  *
C* is the upper right corner of the device.		        	*
C* For example:								*
C*									*
C*		call gclpnl ( .5, .5, 1., 1., iret )			*
C*									*
C* clears the upper right quadrant of the device.	                *
C*									*
C* Note that the fractions describing the view region are not equal 	*
C* to N coordinate values, except for square devices.			*
C*									*
C* GCLPNL  ( XLLF, YLLF, XURF, YURF, IRET )				*
C*									*
C* Input parameters:							*
C*	XLLF		REAL		Lower left x fraction		*
C*	YLLF		REAL		Lower left y fraction		*
C*	XURF		REAL		Upper right x fraction		*
C*	YURF		REAL		Upper right y fraction		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* K. Tyle/GSC		 2/97	Based on GSVIEW				*
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C
	INTEGER		isend (2)
	REAL		rsend (4)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 6
	isend (2) = FCLPNL
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	rsend (1) = xllf
	rsend (2) = yllf
	rsend (3) = xurf
	rsend (4) = yurf
	CALL GPUTR  ( rsend , 4, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
C*
	RETURN
	END
