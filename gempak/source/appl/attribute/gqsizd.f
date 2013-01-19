	SUBROUTINE GQSIZD  ( szmk, sztx, szwb, szws, szab, szah,
     +			     iret )
C************************************************************************
C* GQSIZD								*
C*									*
C* This subroutine returns the size of markers, text, wind barbs,	*
C* weather symbols, arrow bodies, and arrow heads in device coordinates.*
C* The values returned are for size=1.		      			*
C*									*
C* GQSIZD  ( SZMK, SZTX, SZWB, SZWS, SZAB, SZAH, IRET )			*
C*									*
C* Output parameters:							*
C*	SZMK		REAL		Size of markers 		*
C*	SZTX		REAL		Size of text characters 	*
C*	SZWB		REAL		Size of wind barbs 		*
C*	SZWS		REAL		Size of weather symbols 	*
C*	SZAB		REAL		Size of arrow bodies 		*
C*	SZAH		REAL		Size of arrow heads 		*
C*	IRET		INTEGER		Return code 			*
C**									*
C* Log:									*
C* I. Durham/GSC	12/97		Created GQSIZD			*
C* A. Hardy/GSC          6/98           Cleaned up prolog               *
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
	isend (2) = FQSIZD
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
	szmk  = rrcv (1)
	sztx  = rrcv (2)
	szwb  = rrcv (3)
	szws  = rrcv (4)
	szab  = rrcv (5)
	szah  = rrcv (6)
C*
	RETURN
	END
