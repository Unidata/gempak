        SUBROUTINE GPLBND( bndtyp, filcol, filsiz, filpat, filter, minp,
     +                     lincol, lintyp, linwid,
     +                     symtyp, symcol, symnum, symsiz, symwid, iret)
C************************************************************************
C* GPLBND                                                               *
C*                                                                      *
C* This subroutine processes a bound area.                              *
C*                                                                      *
C* GPLBND  ( BNDTYP, FILCOL, FILSIZ, FILPAT, FILTER, MINP,		*
C*           LINCOL, LINTYP, LINWID,					*
C*           SYMTYP, SYMCOL, SYMNUM, SYMSIZ, SYMWID, IRET)		*
C*                                                                      *
C* Input parameters:                                                    *
C*      BNDTYP          CHAR*           Bounds type                     *
C*      FILCOL          INTEGER         Fill color			*
C*      FILSIZ          REAL            Fill pattern size		*
C*      FILPAT          INTEGER         Fill pattern			*
C*      FILTER          REAL            Filter factor-reduce pts        *
C*      MINPTS          INTEGER         Min number of points            *
C*      LINCOL          INTEGER         Outline line color		*
C*      LINTYP          INTEGER         Outline line type		*
C*      LINWID          INTEGER         Outline line width		*
C*      SYMTYP		CHAR*		Symbol type ("MARK" or "WTHR")	*
C*      SYMCOL		INTEGER		Symbol color			*
C*      SYMNUM		INTEGER		Symbol number			*
C*      SYMSIZ          REAL            Symbol size			*
C*      SYMWID		INTEGER		Symbol width			*
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C**                                                                     *
C* Log:                                                                 *
C* D.W.Plummer/NCEP      9/02   From appl/utility/gflbnd.f		*
C* D.W.Plummer/NCEP      9/02   Upgrade to perform outline and sym plot *
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
	INCLUDE         'GEMPRM.PRM'
	INTEGER 	isend (5), ichar(128)
C*
	CHARACTER*(*)   bndtyp, symtyp
	INTEGER		filcol, filpat, minp, lincol, lintyp, linwid
	INTEGER		symcol, symnum, symwid
	REAL		filsiz, filter, symsiz
C------------------------------------------------------------------------
	iret = NORMAL
C
C*      Find the length of the bndtyp string in characters and words.
C
        CALL ST_LSTR ( bndtyp, lencbt, iret )
        IF ( lencbt .eq.   0 ) RETURN
        IF ( lencbt .gt. 400 ) lencbt = 400
        lenwbt = ( lencbt - 1 ) / 4 + 1
C
C*      Find the length of the symtyp string in characters and words.
C
        CALL ST_LSTR ( symtyp, lencst, iret )
        IF ( lencst .gt. 400 ) lencst = 400
        lenwst = ( lencst - 1 ) / 4 + 1
C
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = lenwbt + 1 + lenwst + 1 + 14 
	isend (2) = FFLBND 
C
	CALL GPUT ( isend, 2, iret )
	IF( iret .ne. NORMAL ) RETURN
C
C*      Convert the character string to an integer array and send it.
C
        DO ich = 1, lencbt
           IF ( bndtyp (ich:ich) .eq. CHLF )  bndtyp (ich:ich) = CHCR
        END DO

        CALL ST_STOI ( bndtyp, lencbt, nv, ichar, iret )
	isend (1) = lenwbt
	CALL GPUT ( isend, 1, iret )
        CALL GPUT ( ichar, lenwbt, iret )
        IF ( iret .ne. NORMAL ) RETURN
C
C*      Send the fill color.
C
        isend (1) = filcol 
        CALL GPUT ( isend, 1, iret )
        IF ( iret .ne. NORMAL ) RETURN
C
C*      Send the fill size.
C
        CALL GPUTR ( filsiz, 1, iret )
C
C*      Send the fill pattern.
C
        isend (1) = filpat 
        CALL GPUT ( isend, 1, iret )
        IF ( iret .ne. NORMAL ) RETURN
C
C*      Send the filter.
C
        CALL GPUTR ( filter, 1, iret )
        IF ( iret .ne. NORMAL ) RETURN
C
        isend (1) = minp 
        isend (2) = lincol 
        isend (3) = lintyp 
        isend (4) = linwid 
C
        CALL GPUT ( isend, 4, iret )
        IF ( iret .ne. NORMAL ) RETURN
C
C*      Convert the character string to an integer array and send it.
C
        DO ich = 1, lencst
           IF ( symtyp (ich:ich) .eq. CHLF )  symtyp (ich:ich) = CHCR
        END DO

        CALL ST_STOI ( symtyp, lencst, nv, ichar, iret )
	isend (1) = lenwst
	CALL GPUT ( isend, 1, iret )
        CALL GPUT ( ichar, lenwst, iret )
        IF ( iret .ne. NORMAL ) RETURN
C
        isend (1) = symcol
        isend (2) = symnum
C
        CALL GPUT ( isend, 2, iret )
        IF ( iret .ne. NORMAL ) RETURN
C
C*      Send the symsiz.
C
        CALL GPUTR ( symsiz, 1, iret )
        IF ( iret .ne. NORMAL ) RETURN
C
        isend (1) = symwid
C
        CALL GPUT ( isend, 1, iret )
        IF ( iret .ne. NORMAL ) RETURN
C
C*	Get output parameters.
C
	CALL GGET ( iret, 1, ier )
	IF ( ier .ne. NORMAL ) iret = ier
C
	RETURN
	END
