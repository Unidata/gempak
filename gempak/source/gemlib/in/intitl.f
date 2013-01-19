	SUBROUTINE IN_TITL ( title, idlin, icttl, linttl, ttlstr, iret )
C************************************************************************
C* IN_TITL								*
C*									*
C* This subroutine converts the input for the TITLE variable into a	*
C* title color, title line and title string.  The inputs for TITLE are	*
C* separated by slashes.						*
C*									*
C* IN_TITL  ( TITLE, IDLIN, ICTTL, LINTTL, TTLSTR, IRET )		*
C*									*
C* Input parameters:							*
C*	TITLE		CHAR*		TITLE input			*
C*	IDLIN		INTEGER		Default line			*
C*									*
C* Output parameters:							*
C*	ICTTL		INTEGER		Title color			*
C*	LINTTL		INTEGER		Title line			*
C*	TTLSTR		CHAR*		Title string			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/88						*
C* G. Huffman/GSC	 1/89	Color .lt. 0 reset to 0			*
C* S. Schotz/GSC	10/90	Added call IN_COLOR			*
C* M. desJardins/NMC	10/94	Cleaned up; allow old syntax with ~,^	*
C* T. Piper/GSC		 7/01	Added GEMPRM.PRM to define IMISSD	*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'

	CHARACTER*(*) 	title, ttlstr
C*
	INTEGER		iarr (2)
	CHARACTER	color*48
	CHARACTER	ttltmp*132
C------------------------------------------------------------------------
	iret = 0
C
C*	Get first and second occurrences of '/' which correspond to
C*	the end of the color and line portions of the title string.
C
	CALL ST_NOCC  ( title, '/', 1, ip1, ier )
	CALL ST_NOCC  ( title, '/', 2, ip2, ier )
	IF  ( ip1 .eq. 0 )  THEN
            color = title
          ELSE IF  ( ip1 .eq. 1 )  THEN
            color = ' '
	  ELSE 
            color = title ( :ip1-1 )
        END IF
	IF  ( color .eq. ' ' )  THEN
            icttl = 0
          ELSE
            CALL IN_COLR ( color, 1, icttl, ier )
        END IF
C
C*	Get the title line.
C
	CALL ST_ILST  ( title, '/', IMISSD, 2, iarr, nt, ier )
	linttl = iarr (2)
	IF  ( linttl .eq. IMISSD )  linttl = idlin
C
C*	Check for title.
C
	IF  ( ip2 .gt. 0 )  THEN
	    ttlstr = title ( ip2 + 1 : )
C
C*	    Check for old syntax with ^ and ~.
C
	    ipos = INDEX  ( ttlstr, '^' )
	    IF (( ttlstr (1:1) .eq. '~' ) .and. ( ipos .gt. 0 )) THEN
		ttltmp = ttlstr ( 2: )
		ipos = ipos - 1
		DO WHILE ( ipos .gt. 0 )
		    ttltmp ( ipos:ipos ) = '~'
		    ipos = INDEX ( ttltmp, '^' )
		END DO
		ttlstr = ttltmp
	    END IF
	  ELSE
	    ttlstr = ' '
	END IF
C*
	RETURN
	END
