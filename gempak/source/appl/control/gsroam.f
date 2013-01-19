	SUBROUTINE GSROAM  ( irmflg, ipwdth, iphght, iret )
C************************************************************************
C* GSROAM								*
C* 									*
C* This subroutine sets the roam attributes, including roam flag,       *
C* pixmap width and height.            					*
C*									*
C* GSROAM  ( IRMFLG, IPWDTH, IPHGHT, IRET )				*
C*									*
C* Input parameters:							*
C* 	IRMFLG		INTEGER		Roam flag 			*
C*					  0 = turn roam off             *
C*					  1 = turn roam on              *
C* 	IPWDTH		INTEGER		Pixmap (device) width 		*
C* 	IPHGHT		INTEGER		Pixmap (device) height 		*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* C. Lin/EAI	 	6/97						*
C* A. Hardy/GSC         6/98		Cleaned up prolog               *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (5)
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 5
	isend (2) = FSROAM
C
	isend (3) = irmflg
	isend (4) = ipwdth
	isend (5) = iphght
C
	CALL GPUT  ( isend, 5, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
        CALL GGET  ( iret, 1, ierr )
        IF  ( ierr .ne. NORMAL )  iret = ierr
C*
	RETURN
	END
