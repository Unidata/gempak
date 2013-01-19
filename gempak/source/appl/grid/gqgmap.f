	SUBROUTINE GQGMAP  ( proj, kx, ky, dlatll, dlonll, dlatur,
     +			     dlonur, iret )
C************************************************************************
C* GQGMAP								*
C*									*
C* This subroutine returns the current definition for a grid which 	*
C* is evenly spaced in a simple map projection defined by GSGMAP.	*
C* 									*
C* GQGMAP  ( PROJ, KX, KY, DLATLL, DLONLL, DLATUR, DLONUR, IRET )	*
C*									*
C* Output parameters:							*
C*	PROJ		CHAR*		Map projection name		*
C*	KX		INTEGER		Number of x grid points		*
C*	KY		INTEGER		Number of y grid points		*
C*	DLATLL		REAL		Lower left latitude		*
C*	DLONLL		REAL		Lower left longitude		*
C*	DLATUR		REAL		Upper right latitude		*
C*	DLONUR		REAL		Upper right longitude		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Changed calling sequence to have kx,ky	*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* M. Linda/GSC		 9/97	Changed a key word in the prologue	*
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
C
	CHARACTER*(*) 	proj
C*
	INTEGER		isend (2)
	REAL		rcv (4)
C------------------------------------------------------------------------
C*	Load input parameters into buffer then write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FQGMAP
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get the return code.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  THEN
	    iret = ier
	    RETURN
	END IF
C
C*	Get the map projection and then convert it to characters.
C
	CALL GGET  ( iprj, 1, ier )
	IF  ( ier .ne. NORMAL )  THEN
	    iret = ier
	    RETURN
	  ELSE
	    CALL ST_ITOS  ( iprj, 1, nc, proj, ier )
	END IF
C
C*      Get the rest of the output parameters.
C
	CALL GGET  ( isend, 2, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = ier
	    RETURN
	  ELSE
	    kx = isend (1)
	    ky = isend (2)
	END IF
C*
	CALL GGETR  ( rcv, 4, ier ) 
	IF  ( ier .ne. NORMAL )  THEN
	    iret = ier
	    RETURN
	  ELSE
	    dlatll = rcv (1)
	    dlonll = rcv (2)
	    dlatur = rcv (3)
	    dlonur = rcv (4)
	END IF
C*
	RETURN
	END
