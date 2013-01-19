	SUBROUTINE ST_LDSP  ( string, outstr, ncout, iret )
C************************************************************************
C* ST_LDSP								*
C*									*
C* This subroutine deletes the leading spaces and tabs in a string.	*
C* The input and output strings may be the same variable.		*
C*									*
C* ST_LDSP  ( STRING, OUTSTR, NCOUT, IRET )				*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String				*
C*									*
C* Output parameters:							*
C*	OUTSTR		CHAR*		Output string			*
C*	NCOUT		INTEGER		Number of characters output	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 2/84	Use new GEMPAK routines			*
C* M. desJardins/GSFC	11/84	Fixed					*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* L. Sager/NCEP         2/96   Increased size of stbuf to 160          *
C* D. Kidwell/NCEP       6/97   Referenced actual length of input string*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	string, outstr
C*
	CHARACTER	stbuf*160, c*1
C*-------------------------------------------------------------------------
	lenin = LEN ( string )
	lenst = MIN ( lenin, 160 )
	stbuf = string
	iret  = 0
C
C*	Get length of string.
C
	CALL ST_LSTR  ( stbuf (:lenst), lens, iret )
C
C*	If length is non-zero, find first non space.
C
	IF  ( lens .eq. 0 )  THEN
	    ncout  = 0
	    outstr = ' '
	  ELSE
	    jp = 1
	    c  = stbuf ( jp:jp )
C
	    DO WHILE  ( ( ( c .eq. CHSPAC ) .or. ( c .eq. CHTAB ) .or.
     +			  ( c .eq. CHNULL ) ) .and. ( jp .le. lens ) )
		jp = jp + 1
		IF  ( jp .le. lens )  c = stbuf ( jp:jp )
	    ENDDO
C
C*	    Compute length and fill output string.
C
	    ncout = lens - jp + 1
	    IF  ( ncout .gt. 0 )  THEN
		outstr = stbuf ( jp : lens )
	      ELSE
		outstr = ' '
	    END IF
	ENDIF
C*  
	RETURN
	END
