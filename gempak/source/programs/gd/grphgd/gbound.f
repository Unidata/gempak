	SUBROUTINE GBOUND ( imn, imx, jmn, jmx,
     +			    grid, hist, kx, ky, iret )
C************************************************************************
C* GBOUND                                                               *
C*                                                                      *
C* This subroutine performs the GEMPAK bounds search for the 		*
C* graph-to-grid algorithm. The bounds polygons are saved in the 	*
C* graph-to-grid arrays as closed contour lines with smoothing IMISSD	*
C* and contour value RMISSD. Grid points within the bounds areas are	*
C* set to RMISSD and the corresponding history value is set to BOUNDED.	*
C*                                                                      *
C* Contour lines may be used to simulate boundaries. Closed lines with	*
C* the label "-9999" will mask all points inside their area; lines with	*
C* "9999" will mask all points outside their area.			*
C*                                                                      *
C* GBOUND ( IMN, IMX, JMN, JMX, GRID, HIST, KX, KY, IRET )		*
C*                                                                      *
C* Input parameters:                                                    *
C*      IMN		INTEGER		Lower I range of GRID		*
C*      IMX		INTEGER		Upper I range of GRID		*
C*      JMN		INTEGER		Lower J range of GRID		*
C*      JMX		INTEGER		Upper J range of GRID		*
C*      KX		INTEGER		Number of grid points in X	*
C*      KY		INTEGER		Number of grid points in Y	*
C*                                                                      *
C* Input and Output parameters:                                         *
C*      HIST (KX,KY)	REAL		Grid to keep history		*
C*                                                                      *
C* Output parameters:                                                   *
C*      GRID (KX,KY)	REAL		Grid to calculate point values	*
C*      IRET            INTEGER         Return code                     *
C**                                                                     *
C* Log:                                                                 *
C* W.D.Plummer/NCEP     12/02                                           *
C* W.D.Plummer/NCEP     08/03	Treat missing-value lines as bounds	*
C* W.D.Plummer/NCEP     12/03	Bug fix for BOUNDS processing		*
C* D.W.Plummer/NCEP     02/04	Bug fix for BOUNDS w/ > MAXPPL points	*
C************************************************************************
C
        INCLUDE         'GEMPRM.PRM'
	INCLUDE		'grphgd.cmn'
        INCLUDE         'ERROR.PRM'
C
	REAL		grid(kx,ky), hist(kx,ky)
C
	INTEGER		inout(1200), qinout
	REAL		fiq(1200), fjq(1200)
	REAL            xlat (LLMXPT), ylon (LLMXPT)
	CHARACTER       sys_G*2, sys_M*2, sys_N*2
	CHARACTER*32    btyp, btag, carr(3), barr(10)
C
	LOGICAL		proces, done
C-----------------------------------------------------------------------
	iret = NORMAL
	qinout = GG_IN
	sys_G = 'G'
	CALL ST_NULL ( sys_G, sys_G, lens, ier )
	sys_M = 'M'
	CALL ST_NULL ( sys_M, sys_M, lens, ier )
	sys_N = 'N'
	CALL ST_NULL ( sys_N, sys_N, lens, ier )
C
	CALL CLO_INIT ( iret )
C
C*      Set the geographical limits for searching.
C
        CALL GQBND ( sys_M, rlatmn, rlonmn, dlatmx, dlonmx, iret )
        CALL CLO_BSAREA(rlatmn, rlonmn, dlatmx, dlonmx, iret)
C
	nprow = imx - imn + 1
C
C*      Loop over bounds specifications, setting the bounds type 
C*      (and tag) for searching.
C
        CALL ST_CLST ( bnds, '+', ' ', 10, barr, numb, ier )
C
	DO  nb = 1, numb
C
	  proces = .true.
C
          CALL ST_CLST ( barr(nb), '|', ' ', 3, carr, num, ier )
          CALL ST_LSTR ( carr(1), lens, ier )
          btyp = carr(1)(1:lens) // CHNULL
          CALL ST_LCUC ( btyp, btyp, ier )
C
	  CALL CLO_BSTYPE( btyp, iret )
          IF  ( iret .ne. NORMAL )  proces = .false.
C
	  IF ( proces )  THEN
C
            CALL ST_LSTR ( carr(2), lentag, ier )
            IF ( lentag .gt. 0 )  THEN
              btag = carr(2)(1:lentag) // CHNULL
              CALL CLO_BSTAG( btag, iret )
              IF  ( iret .ne. NORMAL )  proces = .false.
            END IF
C
          END IF
C
	  IF ( proces )  THEN
C
C*	    Check third parm for TRUE or otherwise.
C*	    This controls whether to assign RMISSD to the grid (and
C*	    BOUNDED to the history grid) to gridpoints inside (TRUE, default)
C*	    or outside (otherwise) the bound area.
C*	    Logic: Assign BOUNDED to the history grid if the grid 
C*	    point is inside a bounds part. After all bounds parts have been
C*	    checked, apply the TRUE/FALSE logic. Note that this procedure
C*	    will only work when bounds checking is performed before any other
C*	    grid point value assignment.
C
            CALL ST_LSTR ( carr(3), lenlog, ier )
	    IF ( lenlog .gt. 0 )  THEN
              CALL ST_LCUC ( carr(3), carr(3), ier )
	      IF ( carr(3)(1:1) .ne. 'T' )  qinout = GG_OUT
	    END IF
C
	  END IF
C
	  IF ( proces )  THEN
C
C*	   Loop over gridpoint rows.
C
	   minp   = 0
	   mxpts  = LLMXPT
	   filter = 0.0
	   DO  i = imn, imx
	     fiq(i) = i
	   END DO
C
	   ier = 0
	   DO WHILE ( ier .eq. 0 ) 
C
	    CALL CLO_BGNEXT ( minp, mxpts, filter, npoly, 
     +			      xlat, ylon, ier)
C
            IF ( ier .eq. 0 ) THEN
C
	      IF ( ( xlat(1) .ne. xlat(npoly) ) .or.
     +             ( ylon(1) .ne. ylon(npoly) ) )  THEN
	        npoly = npoly + 1
	        xlat(npoly) = xlat(1)
	        ylon(npoly) = ylon(1)
	      END IF
C
	      DO  i = imn, imx
	        fiq(i) = i
	      END DO
C
	      DO  j = jmn, jmx
C
	        DO  i = imn, imx
	          fjq(i) = j
	        END DO
C
	        CALL CGR_INPOLY ( sys_G, nprow, fiq, fjq, 
     +			        sys_M, npoly, xlat, ylon, 
     +			        inout, ier )
C
C*		Flag all gridpoints inside as BOUNDED
C
	        DO  i = imn, imx
	          IF ( inout(i) .eq. GG_IN )  THEN
		    hist(i,j) = BOUNDED
		    grid(i,j) = RMISSD
	          END IF
	        END DO
C
	      END DO
C
	      nps = 1
	      npe = MIN ( (nps+MAXPPL-1), npoly )
	      done = .false.
	      DO WHILE ( .not. done )
		IF ( nlines .eq. MAXLIN )  THEN
		    done = .true.
		    iret = +4
		ELSE
	            nlines = nlines + 1
	            npts(nlines) = npe - nps + 1
	            value(nlines) = RMISSD
	            ismth(nlines) = IMISSD
	            closed(nlines) = CLSD
	            n = 0
	            DO  np = nps, npe
		        n = n + 1
	                flat(n, nlines) = xlat(np)
	                flon(n, nlines) = ylon(np)
	            END DO
		    if ( npe .lt. npoly )  THEN
		        nps = npe
		        npe = MIN ( (nps+MAXPPL-1), npoly )
		    ELSE
		        done = .true.
		    END IF
		END IF
	      END DO
C
	    END IF
C
	  END DO
C
C*	  If user wants all points outside of bounds areas to be missing,
C*	  then reverse the above assigments.
C*        Note that all history gridpoints should be either BOUNDED or INIT.
C
	  IF ( qinout .eq. GG_IN )  THEN
	      DO  i = imn, imx
	          DO  j = jmn, jmx
	              IF ( hist(i,j) .eq. BOUNDED )  THEN
			  hist(i,j) = INIT
		      ELSE IF ( hist(i,j) .eq. INIT )  THEN
			  hist(i,j) = BOUNDED
		      END IF
	          END DO
	      END DO
          END IF
C
	 END IF
C
	END DO
C
	DO  nl = 1, nlines
C
	  IF ( ( value(nl).eq.RMISSD .or. value(nl).eq.-RMISSD ) .and. 
     &		ismth(nl).ne.IMISSD )  THEN
C
	    DO  nn = 1, npts(nl)
	      xlat(nn) = flat(nn,nl)
	      ylon(nn) = flon(nn,nl)
	    END DO
C
C*	    Loop over gridpoint rows.
C
	    DO  i = imn, imx
	      fiq(i) = i
	    END DO
C
	    DO  j = jmn, jmx
C
	      DO  i = imn, imx
	        fjq(i) = j
	      END DO
C
	      CALL CGR_INPOLY ( sys_G, nprow, fiq, fjq, 
     +			        sys_M, npts(nl), xlat, ylon, 
     +			        inout, ier )
C
	      DO  i = imn, imx
	        IF ( (value(nl).eq. RMISSD .and. inout(i).eq.1) .or.
     &		     (value(nl).eq.-RMISSD .and. inout(i).eq.0) )  THEN
		  hist(i,j) = BOUNDED
		  grid(i,j) = RMISSD
	        END IF
	      END DO
C
	    END DO
C
	    IF ( value(nl).eq.-RMISSD )  value(nl) = RMISSD
C
	  END IF
C
	END DO
C
	RETURN
C
	END
