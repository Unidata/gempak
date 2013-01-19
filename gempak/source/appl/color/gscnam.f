	SUBROUTINE GSCNAM  ( icolr, color, iret )
C************************************************************************
C* GSCNAM								*
C*									*
C* This subroutine defines the color corresponding to a color number	*
C* by specifying a color name.  The recognized colors are stored in     *
C* coltbl.tbl.								*
C*									*
C* GSCNAM  ( ICOLR, COLOR, IRET )					*
C*									*
C* Input parameters:							*
C* 	ICOLR		INTEGER		Color number			*
C*	COLOR		CHAR*		Color name			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	 	Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 8/85						*
C* M. desJardins/GSFC	 5/88	Documentation				*
C* I. Graffman/RDS	 6/88	Clean up				*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* D. Keiser/GSC	12/95	Removed reference to GEMTBL in comments	*
C* L. Williams/EAI	 3/96	Increase color name to 40 charaters	*
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
C*
	CHARACTER*(*)	color
C*
	INTEGER 	isend (24)
	CHARACTER	cbuff*40
C-----------------------------------------------------------------------
C*	Move color name.  Always send 40 character string.
C
	cbuff     = color	                
	nchar     = 40
C
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 14 
	isend (2) = FSCNAM
	isend (3) = icolr
	isend (4) = nchar
	CALL ST_STOI  ( cbuff, nchar, nv, isend (5), iret )
	CALL GPUT   ( isend, 14, iret )
C
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Get the return code.
C
	CALL GGET ( iret, 1, ier )
	IF ( ier .ne. NORMAL ) iret = ier
C
	RETURN
	END
