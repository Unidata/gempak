	SUBROUTINE GGMNMX ( iret )
C************************************************************************
C* GGMNMX                                                               *
C*                                                                      *
C* This subroutine determines which closed lines are innermost among	*
C* the set of closed contours. If a contour has been determined to be	*
C* innermost, then a check is performed to see if a minima/maxima	*
C* point is contained within its boundary. In either case, flags are	*
C* set indicating which min/max is contained within a closed contour	*
C* and which contour surrounds each min/max.				*
C*                                                                      *
C* GGMNXM ( IRET )							*
C*                                                                      *
C* Input parameters:                                                    *
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C**                                                                     *
C* Log:                                                                 *
C* W.D.Plummer/NCEP     12/02                                           *
C* W.D.Plummer/NCEP     09/02	Chgs for relative mm w/o clsd cntrs	*
C************************************************************************
C
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'ERROR.PRM'
	INCLUDE		'grphgd.cmn'
C
	INTEGER		one/1/, found
	CHARACTER	sys_G*2
C------------------------------------------------------------------------
	iret = NORMAL
C
	sys_G = 'G'
	CALL ST_NULL ( sys_G, sys_G, lens, ier )
C
C*	For each closed contour line, test all other closed lines with
C*	CGR_INPOLY. If the closed contour contains no other, mark it as
C*	such and test all minima/maxima for inclusion. If included,
C*	mark the min/max with the line number it is included in.
C
	DO  nm = 1, nmm
	  mmline(nm,1) = IMISSD
	  mmline(nm,2) = IMISSD
	END DO
C
	DO  nl = 1, nlines
C
	  inrmst(nl) = IMISSD
C
	  IF ( closed(nl) .eq. CLSD .and. npts(nl) .gt. 1 .and.
     &		value(nl) .ne. RMISSD )  THEN
C
	    np = npts(nl)
	    found = 0
	    nlx = 1
	    DO WHILE ( found .eq. 0 .and. nlx .le. nlines )
C
	      IF ( nlx .ne. nl )  THEN
C
		CALL CGR_INPOLY ( sys_G, one, fi(1,nlx), fj(1,nlx),
     +				  sys_G, np, fi(1,nl), fj(1,nl),
     +				  found, ier )
C
	      END IF
C
	      nlx = nlx + 1
C
	    END DO
C
C*	    Assign NOMMVALU as innermost flag.
C
	    IF ( found .eq. 0 )  inrmst(nl) = NOMMVALU
C
	    nm = 0
	    DO WHILE ( found .eq. 0 .and. nm .lt. nmm )
C
	      nm = nm + 1
	      CALL CGR_INPOLY ( sys_G, one, fimm(nm), fjmm(nm),
     +                          sys_G, np, fi(1,nl), fj(1,nl),
     +                          found, ier )
C
	      IF ( found .eq. 1 )  THEN
C
C*	    	Assign line index as minmax line flag.
C*	    	Assign minmax index as innermost flag.
C
		mmline(nm,1) = nl
		inrmst(nl) = nm
C
	      END IF
C
	    END DO
C
	  END IF
C
	END DO
C
	DO  nm = 1, nmm
C
	    ii = NINT ( fimm(nm) )
	    jj = NINT ( fjmm(nm) )
C
	    DO  ndir = 1, 8
C
	        CALL GCIIJJ ( ii, jj, kky, ndir, ij, iret )
C
		CALL GINDX1 ( ii, jj, ij, ndir, ffi, ffj, lin, iret )
C
		mmline(nm,ndir) = lin
C
	    END DO
C
	END DO
C
	RETURN
C
	END
