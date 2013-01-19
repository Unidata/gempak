	SUBROUTINE GQMMGN  ( xl, yb, xr, yt, iret )
C************************************************************************
C* GQMMGN								*
C*									*
C* This subroutine returns the current margin sizes used in the map     *
C* mode of the the map/graph coordinate system.  The value returned  	*
C* is that originally specified (fraction of view region or multiples 	*
C* of text size); subsequent changes in view region or text size do 	*
C* not affect the value.						*
C*									*
C* GQMMGN  ( XL, YB, XR, YT, IRET )					*
C*									*
C* Output parameters:							*
C*	XL		REAL		Left margin size		*
C*	YB		REAL		Bottom margin size		*
C*	XR		REAL		Right margin size		*
C*	YT		REAL		Top margin size			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/84	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (2)
	REAL		rrcv (4)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FQMMGN
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL)  RETURN
C
C*	Get output parameters.
C
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  THEN
	    iret = ierr
	    RETURN
	END IF
C
	CALL GGETR  ( rrcv, 4, ierr )
	IF  ( ierr .eq. NORMAL )  THEN
	    xl = rrcv (1)
	    yb = rrcv (2)
	    xr = rrcv (3)
	    yt = rrcv (4)
	  ELSE
	    iret = ierr
	END IF
C*
	RETURN
	END
