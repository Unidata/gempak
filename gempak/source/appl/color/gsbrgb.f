	SUBROUTINE GSBRGB ( icbank, ncolr, icolrs, ireds, igrns, iblus,
     +			    iret )
C************************************************************************
C* GSBRGB								*
C*									*
C* This subroutine defines the color components of a set of colors 	*
C* by specifying the values of red, green, and blue for each color.	*
C* The color components must be in the range 0 - 255.  The set of	*
C* colors are identified by their color bank number and the color       *
C* indices in that bank. 						*
C*									*
C* GSBRGB ( ICBANK, NCOLR, ICOLRS, IREDS, IGRNS, IBLUS, IRET )		*
C*									*
C* Input parameters:							*
C* 	ICBANK		INTEGER		Color bank number		*
C*					  0 = graphics                  *
C*					  1 = satellite                 *
C*					  2 = radar                     *
C* 	NCOLR		INTEGER		Number of colors 		*
C* 	ICOLRS (NCOLR)	INTEGER		Array of color indices 		*
C*	IREDS  (NCOLR)	INTEGER		Array of red color components	*
C*	IGRNS  (NCOLR)	INTEGER		Array of green color components	*
C*	IBLUS  (NCOLR)	INTEGER		Array of blue color components	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	 	Return code			*
C**									*
C* Log:									*
C* C. Lin/EAi		 6/95	From GSCRGB()				*
C* G. Krueger/EAI	11/95	Changed RGB range from 0-1 to 0-255.	*
C* M. Linda/GSC		 3/96	Added check for GPLT buffer overflow	*
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		icolrs(*), ireds(*), igrns(*), iblus(*)
C
	INTEGER 	isend (4)
C-----------------------------------------------------------------------
C
C*	Check if GPLT buffer will overflow.
C
	isnd = 2 + ( 2 + ( 4 * ncolr ) )
	ircv = 1 + ( 1 )
	IF ( ( isnd + ircv ) .gt. IGBSIZ ) THEN
	    iret = NOBUFF
	    RETURN
	END IF
C
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = isnd
	isend (2) = FSBRGB
	isend (3) = icbank
	isend (4) = ncolr
C
	CALL GPUT ( isend, 4, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUT ( icolrs, ncolr, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUT ( ireds, ncolr, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUT ( igrns, ncolr, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUT ( iblus, ncolr, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Get output parameters.
C
	CALL GGET ( iret, 1, ier )
	IF ( ier .ne. NORMAL ) iret = ier
C*
	RETURN
	END
