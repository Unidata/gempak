	SUBROUTINE GQDATT  ( iunit, filnam, itype, xsize, ysize,
     +			     ncurwn, iret )
C************************************************************************
C* GQDATT								*
C* 									*
C* This subroutine queries the device attributes.			*
C*									*
C* GQDATT  ( IUNIT, FILNAM, ITYPE, XSIZE, YSIZE, NCURWN, IRET )		*
C*									*
C* Output parameters:							*
C*	IUNIT		INTEGER		Type of output device		*
C*					  For XW:			*
C*					    1 = GEMPAK window		*
C*					    2 = Motif window		*
C*	FILNAM		CHAR*		Name of output window or file	*
C*	ITYPE		INTEGER		Device type (color,bw,etc) 	*
C*      XSIZE           REAL            X size in inches or pixels 	*
C*	YSIZE		REAL	        Y sixe in inches or pixels 	*
C*	NCURWN		INTEGER	        Current window number		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 5/96	Copied from GSDATT			*
C* M. Linda/GSC		 9/97	Corrected right border of prologue	*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*)	filnam
C*
	CHARACTER	fil*72
	INTEGER		isend (2)
	INTEGER		irecv (18)
	REAL		rrecv (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend ( 1) = 2
	isend ( 2) = FQDATT
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get output parameters.
C
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  THEN
	    iret = ierr
	    RETURN
	END IF
C
	CALL GGET  ( iunit, 1, ierr )
	IF  ( ierr .ne. NORMAL )  THEN
	    iret = ierr
	    RETURN
	END IF
C
	CALL GGET  ( irecv, 18, ierr )
	IF  ( ierr .ne. NORMAL )  THEN
	    iret = ierr
	    RETURN
	END IF
	CALL ST_ITOS ( irecv, 18, nc, fil, ier )
	filnam = fil
C
	CALL GGET  ( itype, 1, ierr )
	IF  ( ierr .ne. NORMAL )  THEN
	    iret = ierr
	    RETURN
	END IF
C
	CALL GGETR ( rrecv, 2, ierr )
	IF  ( ierr .ne. NORMAL )  THEN
	    iret = ierr
	    RETURN
	END IF
	xsize = rrecv (1)
	ysize = rrecv (2)
C
	CALL GGET  ( ncurwn, 1, ierr )
	IF  ( ierr .ne. NORMAL )  THEN
	    iret = ierr
	    RETURN
	END IF
C*
	RETURN
	END
