	SUBROUTINE DSCNAM  ( icolr, color, iret )
C************************************************************************
C* DSCNAM								*
C*									*
C* This subroutine defines the color corresponding to a color 		*
C* number by specifying a color name.  Maximum characters for		*
C* the color name = 40.  The colors are saved in coltbl.tbl.		*
C*									*
C* DSCNAM  ( ICOLR, COLOR, IRET )					*
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
C* D. Keiser/GSC	12/95	Removed reference to GEMTBL in comments	*
C* L. Williams/EAI	 3/96	Increased color name to 40 characters	*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (13)
	CHARACTER*(*)	color
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 13
	isend (2) = CSCNAM
	isend (3) = icolr
	CALL ST_STOI  ( color, 40, iwd, isend(4), iret )
	CALL GPUT ( isend, 13, iret )
C
	IF ( iret .eq. NORMAL ) THEN
C
C*	    Get the return code.
C
	    CALL GGET ( iret, 1, ier )
	    IF ( ier .ne. NORMAL ) iret = ier
	END IF
C*
	RETURN
	END
