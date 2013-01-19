      SUBROUTINE DSROAM  ( irmflg, ipwdth, iphght, iret )
C************************************************************************
C* DSROAM								*
C* 									*
C* This subroutine sets up the roaming on current device.  		*
C* 									*
C* DSROAM  ( IRMFLG, IPWDTH, IPHGHT, IRET )				*
C* 									*
C* Input parameters:                                                    *
C*      IRMFLG          INTEGER         Roam flag                       *
C*      IPWDTH          INTEGER         Pixmap(device) width            *
C*      IPHGHT          INTEGER         Pixmap(device) height           *
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
	INTEGER		isend (5)
C------------------------------------------------------------------------
C*	Load input parameters into the buffer.
C
	isend (1) = 5
	isend (2) = CSROAM
	isend (3) = irmflg
	isend (4) = ipwdth
	isend (5) = iphght
C
	CALL GPUT ( isend, 5, iret )
	IF ( iret .ne. NORMAL ) RETURN
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
C
	RETURN
	END
