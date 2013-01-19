	SUBROUTINE DQDATT ( iunit, filnam, itype, xsize, ysize,
     +			    ncurwn, iret )
C************************************************************************
C* DQDATT								*
C* 									*
C* This subroutine queries the device attributes.			*
C*									*
C* DQDATT ( IUNIT, FILNAM, ITYPE, XSIZE, YSIZE, NCURWN, IRET )		*
C*									*
C* Output parameters:							*
C*	IUNIT		INTEGER		Type of output device		*
C*					  For XW:			*
C*					    1 = GEMPAK window		*
C*					    2 = Motif window		*
C*	FILNAM		CHAR*		Name of output window or file	*
C*	ITYPE		INTEGER		Device type (color,bw,etc)	*
C*	XSIZE		REAL		X size in inches or pixels	*
C*	YSIZE		REAL		Y size in inches or pixels	*
C*	NCURWN		INTEGER		Current window number		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 5/96	Copied from DSDATT			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*)	filnam
C
	CHARACTER	fil*72
	INTEGER		isend (2)
	INTEGER		irecv (18)
	REAL		rrecv (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend ( 1) = 2
	isend ( 2) = CQDATT
C
	CALL GPUT ( isend, 2, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Get output parameters.
C
	CALL GGET ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  THEN
	    iret = ier
	    RETURN
	END IF
C
	CALL GGET ( iunit, 1, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GGET ( irecv, 18, iret )
	IF ( iret .ne. NORMAL ) RETURN
	CALL ST_ITOS ( irecv, 18, nc, fil, ier )
	filnam = fil
C
	CALL GGET ( itype, 1, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GGETR ( rrecv, 2, iret )
	IF ( iret .ne. NORMAL ) RETURN
	xsize = rrecv (1)
	ysize = rrecv (2)
C
	CALL GGET ( ncurwn, 1, ier )
	IF ( ier .ne. NORMAL ) iret = ier
C*
	RETURN
	END
