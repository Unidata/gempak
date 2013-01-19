	SUBROUTINE SNHPLT  ( line, mkcolr, u, v,  npts,   utag, vtag,
     +			     tag,  ntag,   lvert, iret )
C************************************************************************
C* SNHPLT								*
C*									*
C* This subroutine plots the hodograph for SNHODO.			*
C*									*
C* SNHPLT  ( LINE,   MKCOLR, U, V, NPTS, UTAG, VTAG, TAG, NTAG, LVERT,	*
C*           IRET )							*
C*									*
C* Input parameters:							*
C*	LINE		CHAR*		Line color, type, width		*
C*	MKCOLR		INTEGER		Marker color			*
C*	U    (NPTS)	REAL		U for line			*
C*	V    (NPTS)	REAL		V for line			*
C*	NPTS		INTEGER		Number of line points		*
C*	UTAG (NTAG)	REAL		U for tag			*
C*	VTAG (NTAG)	REAL		V for tag			*
C*	TAG  (NTAG)	REAL		Vertical coordinate tags	*
C*	NTAG		INTEGER		Number of tags			*
C*	LVERT		INTEGER		Vertical coordinate type	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* G. Huffman/USRA	 8/89	Adapted from SNPPLT			*
C* S. Schotz/GSC	 7/90	Remove motion parameter			*
C* S. Schotz/GSC	 8/90	Fix marker color, plot meters not KM	*
C* S. Schotz/GSC	10/90	Allow five characters for height text	*
C* K. Brill/NMC		07/91	Cleaned up				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	line
	REAL		u (*), v (*), utag (*), vtag (*), tag (*)
C*
	CHARACTER	ctag*8
	INTEGER		larr (3)
C----------------------------------------------------------------------
	iret  = 0
C
C*	Decode the line color, type, width.
C
	CALL ST_ILST  ( line, '/', 1, 3, larr, n, ier )
C
C*	Check that the color is non-zero.
C
	IF  ( larr (1) .eq. 0 )  RETURN
C
C*	Save current line characteristics, draw line, draw markers.
C
	CALL GQLINE  ( i1, i2, i3, i4, ier )
	CALL GSCOLR  ( larr (1), ier )
	CALL GSLINE  ( larr (2), 0, larr (3), 0, ier )
	CALL GLINE   ( 'M', npts, u, v, ier )
C
	IF  ( mkcolr .gt. 0 )  THEN
            CALL GSCOLR ( mkcolr, ier )
            CALL GMARK  ( 'M', ntag, utag, vtag, ier )
            CALL GSCOLR ( larr (1), ier )
        END IF
C
C*	Reset the line type.
C
	CALL GSLINE  ( i1, 0, i3, 0, ier )
C
C*	Convert each coordinate value to character (in whole numbers).  
C
	IF  ( lvert .eq. 3 )  THEN
	    DO  i = 1, ntag
		CALL ST_RLCH  (  tag (i), 0, ctag, ier )
		CALL ST_LSTR  ( ctag, len, ier )
		IF  ( ctag ( len:len ) .eq. '.' )  THEN
		    ctag ( len:len ) = ' '
		    len = len - 1
		END IF
		CALL GTEXT  ( 'M', utag (i), vtag (i), ctag, 0., 
     +			      ((-2) * len),    0,        ier )
	    END DO
	  ELSE
	    DO  i = 1, ntag
		CALL ST_RLCH  ( tag (i),             0, ctag, ier )
		CALL ST_LSTR  ( ctag, len, ier )
		IF  ( ctag ( len:len ) .eq. '.' )  THEN
		    ctag ( len:len ) = ' '
		    len = len - 1
		END IF
		CALL GTEXT  ( 'M', utag (i), vtag (i), ctag, 0., 
     +			      ((-2) * len),    0,        ier )
	    END DO
	END IF
C*
	RETURN
	END
