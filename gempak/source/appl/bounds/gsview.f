	SUBROUTINE GSVIEW  ( xllf, yllf, xurf, yurf, iret )
C************************************************************************
C* GSVIEW								*
C* 									*
C* This subroutine sets the boundaries of the view region to be used	*
C* to display the plot.  The view region is specified using fractions	*
C* of the available area on the plot device.  The point (0., 0.) is 	*
C* the lower left corner of the device; (1., 1.) is the upper right	*
C* corner of the device.  For example:					*
C*									*
C*		call gsview ( .5, .5, 1., 1., iret )			*
C*									*
C* displays plots in the upper right quadrant of the device.	        *
C*									*
C* Note that the fractions describing the view region are not equal 	*
C* to N coordinate values, except for square devices.			*
C*									*
C* GSVIEW  ( XLLF, YLLF, XURF, YURF, IRET )				*
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
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
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
	isend (2) = FSVIEW
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
