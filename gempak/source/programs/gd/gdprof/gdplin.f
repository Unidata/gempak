	SUBROUTINE GDPLIN  ( thtaln, thteln, mixrln,
     +			     ithtal, ithtel, imixrl, iret )
C************************************************************************
C* GDPLIN								*
C*									*
C* This subroutine finds the characteristics of the lines to be		*
C* drawn.								*
C*									*
C* GDPLIN  ( THTALN, THTELN, MIXRLN, ITHTAL, ITHTEL, IMIXRL, IRET )	*
C*									*
C* Input parameters:							*
C*	THTALN		CHAR*		Theta line input		*
C*	THTELN		CHAR*		Theta-e line input		*
C*	MIXRLN		CHAR*		Mixing ratio line input		*
C*									*
C* Output parameters:							*
C*	ITHTAL (3)	INTEGER		Theta color/type/width		*
C*	ITHTEL (3)	INTEGER		Theta-e color/type/width	*
C*	IMIXRL (3)	INTEGER		Mixr color/type/width		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/GSC          5/90   Created from SNPLIN			*
C* S. Schotz/GSC	10/90	Call IN_COLR for color part of input	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	thtaln, thteln, mixrln
	INTEGER		ithtal (*), ithtel (*), imixrl (*)
C*
	CHARACTER	color*28
C----------------------------------------------------------------------
	iret = 0
C
C*	Extract color portion of string
C
	CALL ST_NOCC  ( thtaln, '/', 1, ip, ier )
	IF  ( (ier .ne. 0 ) .or. ( ip .eq. 0 ) )  THEN
            color = thtaln
	ELSE IF ( ip .eq. 1 ) THEN
            color = ' '
	ELSE
	    color = thtaln ( :ip-1)
        END IF
	IF  ( color .eq. ' ' )  THEN 
            icolor  = 0
        ELSE
C
C*	    Decode color string
C
	    CALL IN_COLR ( color, 1, icolor, ier )
	END IF
C*
	CALL ST_ILST  ( thtaln, '/', IMISSD, 6, ithtal, n, ier )
	ithtal (1) = icolor
	IF  ( ithtal (2) .eq. IMISSD )  ithtal (2) = 1
	IF  ( ithtal (3) .eq. IMISSD )  ithtal (3) = 1
	IF  ( ithtal (4) .eq. IMISSD .or. n .ne. 6 )  ithtal (4) = 213
	IF  ( ithtal (4) .eq. IMISSD .or. n .ne. 6 )  ithtal (5) = 463
	IF  ( ithtal (4) .eq. IMISSD .or. n .ne. 6 )  ithtal (6) = 20
C*
C
C*	Extract color portion of string
C
	CALL ST_NOCC  ( thteln, '/', 1, ip, ier )
	IF  ( (ier .ne. 0 ) .or. ( ip .eq. 0 ) )  THEN
            color = thteln
	ELSE IF ( ip .eq. 1 ) THEN
            color = ' '
	ELSE
	    color = thteln ( :ip-1)
        END IF
	IF  ( color .eq. ' ' )  THEN 
            icolor = 0
        ELSE
C
C*	    Decode color string
C
	    CALL IN_COLR ( color, 1, icolor, ier )
	END IF
	CALL ST_ILST  ( thteln, '/', IMISSD, 6, ithtel, n, ier )
	ithtel (1) = icolor
	IF  ( ithtel (2) .eq. IMISSD )  ithtel (2) = 1
	IF  ( ithtel (3) .eq. IMISSD )  ithtel (3) = 1
	IF  ( ithtel (4) .eq. IMISSD .or. n .ne. 6 )  ithtel (4) = 223
	IF  ( ithtel (4) .eq. IMISSD .or. n .ne. 6 )  ithtel (5) = 403
	IF  ( ithtel (4) .eq. IMISSD .or. n .ne. 6 )  ithtel (6) = 20
C*
C
C*	Extract color portion of string
C
	CALL ST_NOCC  ( mixrln, '/', 1, ip, ier )
	IF  ( (ier .ne. 0 ) .or. ( ip .eq. 0 ) )  THEN
            color = mixrln
	ELSE IF ( ip .eq. 1 ) THEN
            color = ' '
	ELSE
	    color = mixrln ( :ip-1)
        END IF
	IF  ( color .eq. ' ' )  THEN 
            icolor = 0
        ELSE
C
C*	    Decode color string
C
	    CALL IN_COLR ( color, 1, icolor, ier )
	END IF
	CALL ST_ILST  ( mixrln, '/', IMISSD, 6, imixrl, n, ier )
	imixrl (1) = icolor
	IF  ( imixrl (2) .eq. IMISSD )  imixrl (2) = 1
	IF  ( imixrl (3) .eq. IMISSD )  imixrl (3) = 1
	IF  ( imixrl (4) .eq. IMISSD .or. n .ne. 6 )  imixrl (4) = 0
	IF  ( imixrl (4) .eq. IMISSD .or. n .ne. 6 )  imixrl (5) = 0
	IF  ( imixrl (4) .eq. IMISSD .or. n .ne. 6 )  imixrl (6) = 0
C*
	RETURN
	END
