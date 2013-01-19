	SUBROUTINE GPLBND( bndtyp, filcol, filsiz, filtyp, filter, minp,
     +                     lincol, lintyp, linwid,
     +                     symtyp, symcol, symnum, symsiz, symwid, iret)
C************************************************************************
C* GPLBND								*
C* 									*
C* This subroutine processes a bound area.                              *
C* 									*
C* GPLBND  ( BNDTYP, FILCOL, FILSIZ, FILPAT, FILTER, MINP,              *
C*           LINCOL, LINTYP, LINWID,                                    *
C*           SYMTYP, SYMCOL, SYMNUM, SYMSIZ, SYMWID, IRET)              *
C*									*
C* Input parameters:                                                    *
C*      BNDTYP          CHAR*           Bounds type                     *
C*      FILCOL          INTEGER         Fill color                      *
C*      FILSIZ          REAL            Fill pattern size               *
C*      FILTYP          INTEGER         Fill pattern type               *
C*      FILTER          REAL            Filter factor-reduce pts        *
C*      MINPTS          INTEGER         Min number of points            *
C*      LINCOL          INTEGER         Outline line color              *
C*      LINTYP          INTEGER         Outline line type               *
C*      LINWID          INTEGER         Outline line width              *
C*      SYMTYP          CHAR*           Symbol type ("MARK" or "WTHR")  *
C*      SYMCOL          INTEGER         Symbol color                    *
C*      SYMNUM          INTEGER         Symbol number                   *
C*      SYMSIZ          REAL            Symbol size                     *
C*      SYMWID          INTEGER         Symbol width                    *
C*                                                                      *
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**                                                                     *
C* Log:									*
C* M. Li/GSC		04/01						*
C* D.W.Plummer/NCEP	 4/01	Moved processing of background		*
C* D.W.Plummer/NCEP	 1/02	Removed ST_LCUC for tags; add CLO_BSTAG	*
C* D.W.Plummer/NCEP	 9/02	From gplt/utility/gflbnd.f ...		*
C* D.W.Plummer/NCEP      9/02	Upgrade to perform outline and sym plot	*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'XYDEF.CMN'
	INCLUDE 	'DEVCHR.CMN'
C*
        CHARACTER*(*)   bndtyp, symtyp
        INTEGER         filcol, filtyp, minp, lincol, lintyp, linwid
        INTEGER         symcol, symnum, symwid
        REAL            filsiz, filter, symsiz
C
	CHARACTER	type*80
C*
	REAL		xlat (LLMXPT), ylon (LLMXPT)
	REAL            xx (4), yy (4)
	CHARACTER*32	btyp, btag, carr(2), symtuc
	CHARACTER*2	bttmp
C-----------------------------------------------------------------------
	iret   = NORMAL 
C 
C*	If bound type is "bg", fill all area and return.
C
	CALL ST_LCUC ( bndtyp(:2), bttmp, ier )
	IF ( bttmp .eq. 'BG' ) THEN
C
	    CALL GQBND  ( 'P', xllf, yllf, xurf, yurf, ier )
	    xx(1) = xllf
	    xx(2) = xurf
	    xx(3) = xurf
	    xx(4) = xllf
	    yy(1) = yllf
	    yy(2) = yllf
	    yy(3) = yurf
	    yy(4) = yurf
	    npts = 4
C
	    CALL GQCOLR ( icolor, ier )
	    CALL GSCOLR ( filcol, ier )
	    CALL GFILL('P', npts, xx, yy, ier)
	    CALL GSCOLR ( icolor, ier )
C 
	    RETURN
C
	END IF
C
C*      Query the boundaries in map coordinates.
C*
        CALL GQBND ( 'M', rlatmn, rlonmn, dlatmx, dlonmx, iret )
        IF  ( iret .ne. NORMAL )  RETURN
C
C*	Initialize the CLO data structure.
C
	CALL CLO_INIT(iret)
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Set the bounds type for searching.
C
	type = bndtyp
	CALL ST_CLST ( type, '|', ' ', 2, carr, num, ier )
	CALL ST_LSTR ( carr(1), lens, ier )
	btyp = carr(1)(1:lens) // CHNULL 
 	CALL ST_LCUC ( btyp, btyp, ier )
	CALL CLO_BSTYPE( btyp, iret )
	IF  ( iret .ne. NORMAL )  RETURN
	CALL ST_RMBL ( carr(2), carr(2), lentag, ier )
	IF ( lentag .gt. 0 )  THEN
	    btag = carr(2)(1:lentag) // CHNULL 
	    CALL CLO_BSTAG( btag, iret )
	    IF  ( iret .ne. NORMAL )  RETURN
	END IF
C
C*	Set the geographical limits for searching.
C
	CALL CLO_BSAREA(rlatmn, rlonmn, dlatmx, dlonmx, iret)
	IF  ( iret .ne. NORMAL )  RETURN
C
C*      Save some information.
C
	CALL GQCOLR ( icolor, ier )
	CALL GQLINE ( iltyp, ilthw, iwidth, iwhw, ier )
	CALL GSLINE ( lintyp, ilthw, linwid, iwhw, ier )
	CALL GQFILL ( szfil, iftyp, ier )
	CALL GSFILL ( filsiz, filtyp, ier )
C
C*	Loop over all parts to the bounds area; first fill (if 
C*      requested), then outline (if requested).
C
	IF ( filcol .gt. 0 .or. lincol .gt. 0 ) THEN
C
	  ier = 0
	  mxpts = LLMXPT
	  DO WHILE ( ier .eq. 0 )
C 
    	    CALL CLO_BGNEXT(minp, mxpts, filter, npts, xlat, ylon, ier)
C
	    IF ( ier .eq. 0 ) THEN 
C
	      IF ( filcol .gt. 0 ) THEN 
C
C*	        Fill area.
C
	        CALL GSCOLR ( filcol, ier )
C
	        CALL GFILL('M', npts, xlat, ylon, ier) 
C
	      END IF
C
	      IF ( lincol .gt. 0 ) THEN
C
C*	        Outline area.
C
	        CALL GSCOLR ( lincol, ier )
C
	        IF ( npts .lt. LLMXPT )  THEN
	  	  npts = npts + 1
		  xlat(npts) = xlat(1)
		  ylon(npts) = ylon(1)
	        END IF
C
                CALL GLINE('M', npts, xlat, ylon, ier)
C
              END IF
C
	    END IF

	  END DO
C
	END IF
C
	IF ( symcol .gt. 0 )  THEN
C
	  CALL CLO_BSTYPE( btyp, iret )
	  IF  ( iret .ne. NORMAL )  RETURN
C
	  IF ( lentag .gt. 0 )  THEN
	    btag = carr(2)(1:lentag) // CHNULL 
	    CALL CLO_BSTAG( btag, iret )
	    IF  ( iret .ne. NORMAL )  RETURN
	  END IF
C
	  ier = 0
	  DO WHILE ( ier .eq. 0 ) 
C
C*	    Plot either marker or weather symbol at area 
C*          centroid (if requested).
C
	    CALL CLO_BGCENT ( clat, clon, ier )
C
	    IF ( ier .eq. 0 )  THEN
C
	      CALL GSCOLR ( symcol, ier )
	      CALL ST_LCUC ( symtyp, symtuc, ier )
C
	      IF ( symtuc .eq. 'MARK' )  THEN
C
	        CALL GQMRKR ( imark, imkhw, szmark, imkwid, ier )
	        CALL GSMRKR ( symnum, imkhw, symsiz, symwid, ier )
C
	        CALL GMARK ( 'M', 1, clat, clon, ier )
C
	        CALL GSMRKR ( imark, imkhw, szmark, imkwid, ier )
C
	        ELSE IF ( symtuc .eq. 'WTHR' )  THEN
C
	        CALL GQWTHR ( szwthr, iwtwid, ier )
	        CALL GSWTHR ( symsiz, symwid, ier )
C
	        symn = symnum
	        CALL GWTHR ( 'M', 1, symn, clat, clon, 0, 0, ier )
C
	        CALL GSWTHR ( szwthr, iwtwid, ier )
C
	      END IF
C
	    END IF
C
	  END DO
C
	END IF
C
C*      Reset some information.
C
	CALL GSCOLR ( icolor, ier )
	CALL GSLINE ( iltyp, ilthw, iwidth, iwhw, ier )
	CALL GSFILL ( szfil, iftyp, ier )
C*
	RETURN
	END
