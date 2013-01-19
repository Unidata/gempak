	SUBROUTINE DCLPNL  ( xllf, yllf, xurf, yurf, iret )
C************************************************************************
C* DCLPNL								*
C* 									*
C* This subroutine will clear a particular sub-region of the screen.	*
C* The sub-region is specified using fractions of the available area on	*
C* the plot device.  							*
C*									*
C* DCLPNL  ( XLLF, YLLF, XURF, YURF, IRET )				*
C*									*
C* Input parameters:							*
C*	XLLF		REAL		Lower left x  ( device coord. )	*
C*	YLLF		REAL		Lower left y  ( device coord. )	*
C*	XURF		REAL		Upper right x ( device coord. )	*
C*	YURF		REAL		Upper right y ( device coord. )	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* K. Tyle/GSC		 2/97						*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (2)
	REAL		rsend (4)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 6
	isend (2) = CCLPNL
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
