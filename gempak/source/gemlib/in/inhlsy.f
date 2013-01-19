        SUBROUTINE IN_HLSY ( hlsym, symtxt, valtxt, labloc, iret )
C************************************************************************
C* IN_HLSY								*
C*									*
C* This subroutine scans the user input for HLSYM.  The entry for	*
C* HLSYM is in the form:						*
C*									*
C*	size;size / position / font;font / width;width / hwsw;hwsw	*
C*									*
C* The size, font, width, and hardware/software flag (hwsw) entries	*
C* are for the symbol or string used to mark an extremum and the	*
C* string used to print the value, in that order. These entries are	*
C* separated into the two output strings which must be fed into 	*
C* IN_TEXT for final processing and attribute setting.			*
C*									*
C* The position is a single entry specifying where the value string	*
C* is to be plotted beneath the marking symbol or string.  There are	*
C* three positions as shown below:					*
C*									*
C*				H					*
C*			    1   2   3					*
C*									*
C* The default is 2.							*
C*									*
C*									*
C* IN_HLSY  ( HLSYM, SYMTXT, VALTXT, LABLOC, IRET )			*
C*									*
C* Input parameters:							*
C*	HLSYM		CHAR*		User input			*
C*									*
C* Output parameters:							*
C*	SYMTXT		CHAR*		TEXT output for marker string	*
C*	VALTXT		CHAR*		TEXT output for value string	*
C*	LABLOC		INTEGER		Label location			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC		01/93						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	CHARACTER* (*)	hlsym, symtxt, valtxt
C*
	CHARACTER	strngs(5)*32, part(2)*12
C------------------------------------------------------------------------
	iret  = 0
	symtxt = ' '
	valtxt = ' '
C
C*	Split the input into five substrings.
C
	CALL ST_CLST ( hlsym, '/', ' ', 5, strngs, num, ier )
C
C*	Separate substrings 1, 3-5 into two pieces and build the
C*	the IN_TEXT strings for symbols and values.
C
	DO i = 1, 5
	    IF ( i .ne. 2 ) THEN
C
C*		This is a dual input:
C
		CALL ST_CLST ( strngs (i), ';', ' ', 2, part,
     +			       num, ier )
		IF ( part (2) .eq. ' ' ) part (2) = part (1)
		IF ( i .eq. 1 ) THEN
		    symtxt = part (1)
		    valtxt = part (2)
		ELSE
		    CALL ST_LSTR ( symtxt, lensym, ier )
		    CALL ST_LSTR ( valtxt, lenval, ier )
		    symtxt = symtxt(:lensym) // '/' // part (1)
		    valtxt = valtxt(:lenval) // '/' // part (2)
		END IF
	    ELSE
C
C*		This is a single integer input.
C
		CALL ST_NUMB ( strngs (2), iv, ier )
		IF ( iv .ne. 1 .and. iv .ne. 3 ) iv = 2
		labloc = iv
	    END IF
	END DO
C*
	RETURN
	END
