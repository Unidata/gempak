	SUBROUTINE GQSYSZ  ( rxszmk, ryszmk, rxsztx, rysztx,
     +			     rxszwb, ryszwb, iret )
C************************************************************************
C* GQSYSZ								*
C*									*
C* This subroutine returns the current size of text, markers and wind   *
C* barbs in terms of normalized device coordinates.		        *
C*									*
C* GQSYSZ  ( RXSZMK, RYSZMK, RXSZTX, RYSZTX, RXSZWB, RYSZWB, IRET )	*
C*									*
C* Output parameters:							*
C*	RXSZMK		REAL		Width of markers		*
C*	RYSZMK		REAL		Height of markers		*
C*	RXSZTX		REAL		Width of text characters	*
C*	RYSZTX		REAL		Height of text characters	*
C*	RXSZWB		REAL		Length of wind barbs if		*
C*					oriented along x axis		*
C*	RYSZWB		REAL		Length of wind barbs if		*
C*					oriented along y axis		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* T. Piper/GSC		 5/98	Corected typo in prolog                 *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
C*
	INTEGER 	isend (2)
	REAL 		rrcv (6)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FQSYSZ
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
	CALL GGETR  ( rrcv, 6, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
	rxszmk = rrcv (1)
	ryszmk = rrcv (2)
	rxsztx = rrcv (3)
	rysztx = rrcv (4)
	ryszwb = rrcv (5)
	rxszwb = rrcv (6)
C*
	RETURN
	END
