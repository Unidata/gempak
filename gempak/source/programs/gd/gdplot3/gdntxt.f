	SUBROUTINE GDNTXT ( ictyp, line, loci, info, itxtp, iret )
C************************************************************************
C* GDNTXT								*
C*									*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	12/91						*
C* T. Lee/SAIC		01/06	Copied from GPNTXT; Minor bug fix	*
C* C. Bailey/HPC	10/06	Changed call in IN_LINE			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	line, loci, info
C*
	CHARACTER	sys*1, text(2)*256, nchar(3)*256, ctext*256, 
     +			ctmp*256, offset*256
	REAL		txpt(2), xx(4), yy(4)
	REAL		smooth, linfltr
	INTEGER		iarr(2), iarr2(2)
	LOGICAL		scflag
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Get plot bounds in Normal coordinates.
C
	CALL GQBND ( 'N', xleft, ybot, xrght, ytop, ier )
C
C*	Parse the fill input.
C
	CALL IN_LINE ( line, 1, 1, icolor, itype, iwidth, ilabel, 
     +                 smooth, linfltr, scflag, ier )
C
	IF  ( icolor .ne. 0 )  THEN
C
C*	    Set coordinate system to be used.
C
	    ipipe = INDEX ( loci, '|' )
	    IF ( ipipe .gt. 0 ) THEN
 		iend = ipipe - 1
		offset = loci(ipipe+1:)
	    ELSE
		CALL ST_LSTR ( loci, iend, ier )
	    END IF
	    sys = 'N'
	    IF  ( loci(1:1) .eq. '@' )  THEN
		sys = 'G'
		loci = loci(2:iend)
	    ELSE IF  ( loci(1:1) .eq. '#' )  THEN
		sys = 'M'
		loci = loci(2:iend)
	    END IF
C
C*	    Get the point(s) for the plot.
C
	    CALL ST_RLST ( loci, ';', RMISSD, 2, txpt, num, ier )
C
C*	    Separate the shape information.
C
	    CALL ST_NOCC ( info, '/', 7, ibegin, ier )
	    IF  ( ibegin .gt. 0 )  THEN
		text(1) = info(:ibegin-1)
		text(2) = info(ibegin+1:)
	      ELSE
		text(1) = info
		text(2) = ' '
	    END IF
C
C*	    Set text attributes.
C
	    CALL IN_TEXT ( text(1), ier )
C
C*	    Get text size.
C
	    CALL GQSYSZ  ( rxszmk, ryszmk, rxsztx, rysztx, rxszwb,
     +			   ryszwb, ier )
C
C*	    Separate the justification and the text.
C
	    ipos = INDEX ( text(2), '/' )
	    IF  ( ipos .gt. 0 )  THEN
		nchar(1) = text(2)(:ipos-1)
		nchar(2) = text(2)(ipos+1:)
		ipos2 = INDEX ( nchar(2), '/' )
		IF  ( ipos2 .gt. 0 )  THEN
		    nchar(3) = nchar(2)(:ipos2-1)
		    nchar(2) = nchar(2)(ipos2+1:)
		END IF
	      ELSE
		nchar(1) = ' '
		nchar(2) = text(2)
		nchar(3) = ' '
	    END IF		
	    CALL ST_LCUC ( nchar(1), nchar(1), ier )
	    CALL ST_CRNM ( nchar(3), rotat, ier )
C
C*	    If using Normal coords, correct for device.
C
	    IF  ( sys .eq. 'N' )  THEN
		txpt(1) = (xrght-xleft) * txpt(1)
		txpt(2) = (ytop -ybot ) * txpt(2)
	    ENDIF
C
	    CALL ST_LSTR ( nchar(2), itlen, ier )
C
C*	    Set the offsets from the justification and the text length.
C
	    IF  ( nchar(1) .eq. 'C' )  THEN
		xlft = -0.5
		xrgt =  0.5
		ijust = 0
		irjust = 2
	    ELSE IF  ( nchar(1) .eq. 'R' )  THEN
		xlft = -1
		xrgt =  0
		ijust = 1
		irjust = 3
	    ELSE
		xlft = 0
		xrgt = 1
		ijust = -1
		irjust = 1
	    END IF
	    iyoff  = 0
C
C*	    Set justification for regular text.
C
	    IF ( itxtp .eq. 1 ) THEN
		CALL GSTEXT  ( 0, 0, 0., 0, 0, 0, irjust, ier )
	    END IF
C
	    CALL GTRANS ( sys, 'N', 1, txpt(1), txpt(2), xcnt,
     +			  ycnt, ier )
	    IF  ( ictyp .eq. 2 .and. itype .ne. 0 )  THEN
C
C*		If the label box color exists, then find the corner
C*		points in order to draw a box around the text.
C
		d1 = ABS (xlft)
		d2 = ABS (xrgt)
		d3 = 0.5
		xx(1) = xcnt - ( d1 * (itlen * rxsztx) + 
     +				      ( .45 * rxsztx ) )
		yy(1) = ycnt - ( d3 * ( rysztx ) + ( .25 * rysztx ) )
		xx(2) = xcnt - ( d1 * (itlen * rxsztx) +
     +				      ( .45 * rxsztx ) )
		yy(2) = ycnt + ( d3 * ( rysztx ) + ( .25 * rysztx ) )
		xx(3) = xcnt + ( d2 * (itlen * rxsztx) +
     +				      ( .05 * rxsztx ) )
		yy(3) = ycnt + ( d3 * ( rysztx ) + ( .25 * rysztx ) )
		xx(4) = xcnt + ( d2 * (itlen * rxsztx) +
     +				      ( .05 * rxsztx ) )
		yy(4) = ycnt - ( d3 * ( rysztx ) + ( .25 * rysztx ) )
C
C*		Set attributes.
C
		CALL GSCOLR ( itype, ier )
C
C*		Plot label box.
C
		CALL GFILL ( 'N', 4, xx, yy, ier )
C
	    END IF
	    IF  ( nchar(1) .eq. 'R' )  THEN
		ixoff = (-2) * itlen
	    ELSE IF  ( nchar(1) .eq. 'L' )  THEN
		ixoff = 0
	    ELSE 
		ixoff = -(itlen)
	    END IF
	    ixoff = 0
C
C*	    Set attributes.
C
	    CALL GSCOLR ( icolor, ier )
C
C*	    Plot text.
C
	    IF ( itxtp .eq. 1 ) THEN
              CALL GQTEXT  ( ihtxfn, ihtxhw, shztext, ihtxwid,
     +                       ihbrdr, ihrrotn, ihjust, iret )
              IF ( ihrrotn .eq. 2 ) THEN
                CALL GTRANS('N', 'M', 1, xcnt, ycnt, xm, ym, ier )
	        CALL GTEXT ( 'M', xm, ym, nchar(2), rotat, ixoff,
     +			   iyoff, ier )
              ELSE
	        CALL GTEXT ( 'N', xcnt, ycnt, nchar(2), rotat, ixoff,
     +			   iyoff, ier )
              END IF
	    ELSE IF ( itxtp .eq. 2 ) THEN
	      CALL ST_NOCC  ( nchar(2), '/', 2, ipoint, iret )
	      IF ( iret .eq. 0 ) THEN
		IF  ( ipoint .gt. 0 )  THEN
		  CALL ST_ILST ( nchar(2)(:ipoint-1), '/', IMISSD, 2, 
     +				 iarr, num, ier )
		END IF
		IF ( .not. ERMISS(float(iarr(1))) .and. 
     +		     .not. ERMISS(float(iarr(2))) ) THEN
		  ipos = 0
		  ctmp = ' '
		  ipoint = ipoint + 1
		  ibs = INDEX ( nchar(2)(ipoint+ipos:), '\n' )
		  DO WHILE ( ibs .ne. 0 ) 
		      CALL ST_LSTR ( ctmp, ilen, ier )
		      IF ( ilen .le. 0 ) THEN
		        ctmp = 
     +			  nchar(2)(ipoint+ipos:ipoint+ipos+ibs-2) //
     +		          CHCR
		      ELSE
		        ctext = ctmp(:ilen) // 
     +		          nchar(2)(ipoint+ipos:ipoint+ipos+ibs-2) //
     +		          CHCR
			ctmp = ctext
		      END IF
		      ipos = ipos + ibs + 1
		      ibs = INDEX ( nchar(2)(ipoint+ipos:), '\n' )
		  END DO
		  IF ( ipipe .gt. 0 ) THEN
		    CALL ST_ILST ( offset, ';', 0, 2, iarr2, num, ier )
		    ixoff = iarr2(1)
		    iyoff = iarr2(2)
		  ELSE
		    ixoff = 0
		    iyoff = 0
		  END IF
		  CALL ST_LSTR ( nchar(2), ilen, ier )
		  CALL ST_LSTR ( ctmp, ilen1, ier )
		  ctext = ctmp(:ilen1) // nchar(2)(ipoint+ipos:ilen)
                  CALL GQTEXT  ( ihtxfn, ihtxhw, shztext, ihtxwid,
     +                       ihbrdr, ihrrotn, ihjust, iret )
                  IF ( ihrrotn .eq. 2 ) THEN
                    CALL GTRANS('N', 'M', 1, xcnt, ycnt, xm, ym, ier )
	            CALL GTXSY ( 'M', iarr(1), iarr(2), ijust, 
     +				ixoff, iyoff, rotat, xm, ym,
     +				ctext, ier )
                  ELSE
	            CALL GTXSY ( 'N', iarr(1), iarr(2), ijust, 
     +				ixoff, iyoff, rotat, xcnt, ycnt, 
     +				ctext, ier )
                  END IF
		END IF
	      END IF
	    END IF
C
	END IF
C*
	RETURN
	END
