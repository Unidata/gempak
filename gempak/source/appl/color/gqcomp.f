	SUBROUTINE GQCOMP ( icolr, color, ired, igreen, iblue, xname,
     +			    iret )
C************************************************************************
C* GQCOMP								*
C*									*
C* This subroutine returns the red, green, and blue components of a	*
C* color.  The color components are defined in the range 0 - 255.  If	*
C* the color was defined by name, the GEMPAK color name is also		*
C* returned.  The X Window System color name is returned if it is	*
C* available.								*
C*									*
C* GQCOMP  ( ICOLR, COLOR, IRED, IGREEN, IBLUE, XNAME, IRET )		*
C*									*
C* Input parameters:							*
C* 	ICOLR		INTEGER		Color number			*
C*									*
C* Output parameters:							*
C*	COLOR		CHAR*		GEMPAK color name		*
C*	IRED		INTEGER		Red color component		*
C*	IGREEN		INTEGER		Green color component		*
C*	IBLUE		INTEGER		Blue color component		*
C*	XNAME		CHAR*		X Window System color name	*
C*	IRET		INTEGER 	Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 8/85						*
C* M. desJardins/GSFC	 5/88	Documentation				*
C* I. Graffman/RDS	 6/88	Clean up				*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* G. Krueger/EAI	11/95	Removed HLS;Added XNAME;Mod. RGB range	*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (3), ircv (20)
	CHARACTER*(*) 	color, xname
C-----------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 3
	isend (2) = FQCOMP
	isend (3) = icolr
C
	CALL GPUT ( isend, 3, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Get the return code.
C
	CALL GGET ( iret, 1, ier )
	IF ( ier .ne. NORMAL ) THEN
	    iret = ier
	    RETURN
	END IF
C
 	CALL GGET ( ircv, 20, ier )
	IF ( ier .ne. NORMAL ) THEN
	    iret = ier
	    RETURN
	END IF
C
	CALL ST_ITOS  ( ircv, 20, nc, color, ier )
C
	IF ( ier .ne. NORMAL ) THEN
	    iret = ier
	    RETURN
	  ELSE
C
	    CALL GGET ( ircv, 3, ier )
	    ired   = ircv (1)
	    igreen = ircv (2)
	    iblue  = ircv (3)
	    IF ( ier .ne. NORMAL ) THEN
	       iret = ier
	    END IF
	END IF
C
 	CALL GGET ( ircv, 20, ier )
	IF ( ier .ne. NORMAL ) THEN
	    iret = ier
	    RETURN
	END IF
C
	CALL ST_ITOS  ( ircv, 20, nc, xname, ier )
C*
	RETURN
	END
