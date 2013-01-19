	SUBROUTINE RS_NUMD  ( line, numb, defalt, nout, iret )
C************************************************************************
C* RS_NUMD								*
C*									*
C* This subroutine reads a character substring (which must contain 	*
C* nothing but digits and slashes) and returns the integer		*
C* represented by that substring.  If the string contains nothing but	*
C* slashes, the subroutine returns DEFALT.  If embedded slashes are 	*
C* encountered, they are treated as zeroes.				*
C*									*
C* RS_NUMD  ( LINE, NUMB, DEFALT, NOUT, IRET )				*
C*									*
C* Input parameters:							*
C*	LINE		CHAR*		Substring			*
C*	NUMB		INTEGER		Number of characters to read	*
C*	DEFALT		INTEGER		Default value for numb		*
C*									*
C* Output parameters:							*
C*	NOUT		INTEGER		Returned integer		*
C*	IRET		INTEGER		Return status			*
C*					  0 = Successful completion	*
C*					 -1 = Unexpected chars found	*
C*					  1 = Needed to use defalt	*
C**									*
C* Log:									*
C* J. Nielsen/MIT	10/86						*
C* J. Nielsen/MIT	 2/89						*
C* F. Cuq/UCLA		 8/90	Integer*2 -> integer*4			*
C* J. Nielsen/TAMU	 2/92	Gempacized				*
C* S. Jacobs/NMC	 9/95	Fixed format I to I1 			*
C* T. Lee/GSC		 9/97	Fixed typo, DEFAULT -> DEFALT		*
C************************************************************************
	CHARACTER*(*)	line
	INTEGER		numb, defalt					
C*
	INTEGER		nout, iret
C*
	INTEGER		tento, defcnt
C------------------------------------------------------------------------`
	defcnt = 0
	nout = 0
	tento = 1
C
C*	Work backwards from last digit
C
	DO  j = numb, 1, -1
	    IF  ( line ( j:j) .ne. '/' )  THEN
C
C*		Read digit, add to nout
C
		READ ( line ( j:j ), '(I1)', ERR=1000 ) idigit
		nout = nout + idigit*tento
	    ELSE
C
C*		Found slash; do not increment NOUT
C
		defcnt = defcnt + 1
	    ENDIF
C
	    tento = tento * 10
	END DO
C
C*	See if we have found nothing but slashes
C
	IF  ( defcnt .eq. numb )  THEN
	    nout = defalt
	    iret = 1
	ELSE
C
C*	Normal successful completion
C
	    iret = 0
	ENDIF
	RETURN
C
C*	Here if found a non-integer, non-slash in substring
C
1000	nout = defalt
	iret = -1
	RETURN
	END

