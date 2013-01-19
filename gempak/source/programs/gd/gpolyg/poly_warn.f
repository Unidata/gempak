	SUBROUTINE POLY_WARN ( rlat, rlon, iret )
C************************************************************************
C* POLY_WARN	  							*
C*									*
C* This subroutine gets vertices of warning polygons.  The TPC zone	*
C* along 31N has to have three points in order to produce a seamless	*
C* boundary between TPC and OPC.					*
C*									*
C* POLY_WARN ( RLAT, RLON, IRET )					*
C*									*
C* Input parameters:							*
C*	RLAT(IGXD,IGYD)	REAL		Latitude 			*
C*	RLON(IGXD,IGYD)	REAL		Longitude 			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C*					+4 = Cannot generate fcst zones	*
C*					+5 = Cannot generate raw polys	*
C* Log:									*
C* T. Lee/SAIC		03/08	Created					*
C* T. Lee/SAIC		05/08	Called poly_cvgfw to create poly VGF	*
C************************************************************************
	INCLUDE		'gpolyg.cmn'
	INCLUDE		'ERROR.PRM'
	PARAMETER	( EPS = .001 )
	REAL		rlat (igxd,igyd), rlon (igxd,igyd)
	REAL		xclip(500), yclip(500)
	REAL		rsnake (MAXPTS,MAXGRP,NCATEG)
	REAL		ssnake (MAXPTS,MAXGRP,NCATEG)
	REAL		rout (MAXPTS,MAXGRP,NCATEG)
	REAL		sout (MAXPTS,MAXGRP,NCATEG)
	REAL		xin(MAXPTS), yin(MAXPTS)
	REAL		xout(MAXPTS), yout(MAXPTS)
	INTEGER		iorg(MAXPTS), lintyp(2), filtyp(4)
	LOGICAL		done, rmflg(MAXPTS)
	CHARACTER	opts*128, fname*40, ftype*4, cname*30
	CHARACTER	cmxclp*2, crdist*3, crdpct*4
	DATA		ftype/'.vgf'/
	DATA		lintyp/12,1/, filtyp/2,7,3,0/
C-----------------------------------------------------------------------
	iret = 0
C
C*	Get margins as a rectagular polygon.
C
	CALL POLY_MARGIN ( ier )
C
C*	Add padding next the grid points.
C
	CALL POLY_APAD ( ier )
C
C*	Get the first point of the "snake" at the far left and lowest j.
C
	DO nc = 1, NCATEG
	    DO ng = 1, ngroup ( nc )
		IF ( imargn ( 1, ng, nc ) .gt. 1 )  THEN
		    imargn ( 1, ng, nc ) = imargn ( 1, ng, nc ) - 1
		END IF
C
		IF ( jmargn ( 1, ng, nc ) .gt. 1 )  THEN
		    jmargn ( 1, ng, nc ) = jmargn ( 1, ng, nc ) - 1
		END IF
C
		IF ( imargn ( 2, ng, nc ) .lt. igxd )  THEN
		    imargn ( 2, ng, nc ) = imargn ( 2, ng, nc ) + 1
		END IF
C
		IF ( jmargn ( 2, ng, nc ) .lt. igyd )  THEN
		    jmargn ( 2, ng, nc ) = jmargn ( 2, ng, nc ) + 1
		END IF
	    END DO
C
	    DO ng = 1, ngroup ( nc )
		ist = imargn ( 1, ng, nc )
		jst = jmargn ( 2, ng, nc )
		isnake ( 1, ng, nc )  = ist
		DO np = 1, numpts ( ng, nc )
		    newi = polygi ( np, ng, nc )
		    newj = polygj ( np, ng, nc )
		    IF ( (newi .eq. ist) .and. (newj .le. jst) )  THEN
			jst = newj
			jsnake ( 1, ng, nc ) = jst
		    END IF
		END DO
	    END DO
	END DO
C
C*	Transverse the outermost "padding" points counter-clockwise.
C
	DO nc = 1, NCATEG
	    DO ng = 1, ngroup ( nc )
		kx = imargn ( 2, ng, nc ) - imargn ( 1, ng, nc )
		ky = jmargn ( 2, ng, nc ) - jmargn ( 1, ng, nc )
		CALL POLY_CCWA ( kx, ky, ng, nc, ier )
	    END DO
	END DO
C
C*	Convert to map coordinate for clipping.
C
	CALL ST_LSTR ( gdattm, ngd, ier )
	fname = gdattm ( : ngd ) // '.' // cdxzon // ftype
	CALL ST_NULL ( fname, fname, lens, ier )
	ncount = 0
	DO nc = 1, NCATEG
	    DO ng = 1, ngroup ( nc )
		ncount = ncount + 1 
		DO np = 1, mpoint ( ng, nc )
		    is = isnake (np, ng, nc)
		    js = jsnake (np, ng, nc)
		    rsnake ( np, ng, nc ) = FLOAT ( is )
		    ssnake ( np, ng, nc ) = FLOAT ( js )
		    xin ( np ) = rlat ( is, js )
		    yin ( np ) = rlon ( is, js )
		END DO
C
C*		Create VG files for "raw" polygoins (before clipping
C*		and point reductions).
C
		IF ( map_on )  THEN
		    majcol = 3
		    CALL POLY_CVGFW ( fname, npzon, zzlat, zzlon,
     +				      lintyp ( 2 ), filtyp ( 4 ), 
     +				      majcol, ierr )
		    IF ( ierr .ne. 0 )  THEN
			ierr = +4 
			CALL ER_WMSG ( 'GPOLYG', ierr, ' ', ier )
		    END IF
		END IF
C
		IF ( raw_on )  THEN
		    majcol = 1
		    CALL POLY_CVGFW ( fname, mpoint ( ng,nc ), xin, yin, 
     +				      lintyp ( 1 ), filtyp ( 4 ), 
     +				      majcol, ierr ) 
		    IF ( ierr .ne. 0 )  THEN
			ierr = +5 
			CALL ER_WMSG ( 'GPOLYG', ierr, ' ', ier )
		    END IF
		END IF
C
		CALL GTRANS ( 'G', 'M', mpoint ( ng, nc ), 
     +			      rsnake (1, ng, nc), ssnake (1, ng, nc),
     +			      rout (1, ng, nc), sout (1, ng, nc),
     +			      ier )
C
C*		Remove redundant points around the polygons.
C
		kk = 2
		done = .false.
		mp = mpoint ( ng, nc )
		DO WHILE ( .not. done ) 
		    A = sout ( kk, ng, nc ) - sout ( kk - 1, ng, nc )
		    B = rout ( kk, ng, nc ) - rout ( kk - 1, ng, nc )
		    ang1 = ATAN2 ( A, B )
		    C = sout ( kk + 1, ng, nc ) - sout ( kk, ng, nc )
		    D = rout ( kk + 1, ng, nc ) - rout ( kk, ng, nc )
		    ang2 = ATAN2 ( C, D )
C
		    IF ( ABS ( ang1 - ang2 ) .le. EPS .or.
     +			 ( ABS ( A ) .le. EPS .and. 
     +			   ABS ( B ) .le. EPS ) )  THEN
			mp = mp - 1
			DO  ii = kk, mp
			    rout ( ii, ng, nc ) = rout ( ii+1, ng, nc )
			    sout ( ii, ng, nc ) = sout ( ii+1, ng, nc )
			END DO
			mpoint ( ng, nc ) = mp 
		      ELSE
			kk = kk + 1
		    END  IF
		    IF ( kk .gt. ( mp - 1 ) ) done = .true.
		END DO
	    END DO
	END DO
C
C*	Clip the warning polygon against map bounds.  Map coordinate
C*	is used.
C
	CALL ST_INCH ( maxclp, cmxclp, ier )
	CALL ST_RLCH ( redpct, 0, crdpct, ier )
	CALL ST_RLCH ( redist, 0, crdist, ier )
	opts =	'<alg_choice>2</alg_choice>' //
     +		'<reduce_num>' // cmxclp // '</reduce_num>' //
     +		'<incr_pct>' // crdpct  // '</incr_pct>' //
     +		'<incr_dst>' // crdist // '</incr_dst>'
	CALL ST_NULL ( opts, opts, lens, ier )
	nopoly = 0
	DO nc = 1, NCATEG
	    DO ng = 1, ngroup ( nc )
		nout = mpoint ( ng, nc )
		DO np = 1, nout
		   xout ( np ) = rout ( np, ng, nc ) 
		   yout ( np ) = sout ( np, ng, nc ) 
		END DO
		CALL POLY_CLIPW ( nout, xout, yout, npzon, 
     +				  zzlat, zzlon, ier )
C
		CALL POLY_GET_CONTOUR ( ncntr, ier )
		DO kc = 1, ncntr
		    nopoly = nopoly + 1
		    CALL POLY_GET_VERTEX ( kc, nclip, xclip, yclip, ier)
C
C*		    Convert the clipped vertices from map to device 
C*		    coord and reduce polygon vertices to a desired 
C*		    number.
C
		    IF ( nclip .gt. 8 ) THEN
C
		        CALL GTRANS ( 'M', 'D', nclip, xclip, yclip, 
     +				       xout, yout, ier)
		        DO kk = 1, nclip
		            xin ( kk ) = xout ( kk )
		            yin ( kk ) = yout ( kk )
		            rmflg ( kk ) = .TRUE.
		        END DO
C
C*		        Set remove flags.
C
			CALL POLY_RMFLG ( nclip, xclip, yclip, rmflg,
     +					  ier )
C
		        CALL CGR_REDUCEPTSW ( opts, nclip, xin, yin, 
     +					      rmflg, nout, xout, yout, 
     +					      iorg, ier )
			nclip = nout
C
C*			Convert point back to map coordinates.
C
			CALL GTRANS ( 'D', 'M', nclip, xout, yout, 
     +				      xclip, yclip, ier)
		    END IF
C
C*		    Make a closed polygon and create CAP messages.
C
		    CALL POLY_OPNF ( nopoly, lunp, cname, ier )
C
		    IF ( nclip .gt. 0 ) THEN
			majcol = 1
			IF ( nc .eq. 1 )  THEN
			    CALL poly_cvgfw (fname, nclip, xclip, yclip,
     +				             lintyp (2), filtyp(1), 
     +					     majcol, ier)
			  ELSE IF ( nc .eq. 2 )  THEN
			    CALL poly_cvgfw (fname, nclip, xclip, yclip,
     +				             lintyp (2), filtyp(2), 
     +					     majcol, ier)
			  ELSE IF ( nc .eq. 3 )  THEN
			    CALL poly_cvgfw (fname, nclip, xclip, yclip,
     +				             lintyp (2), filtyp(3), 
     +					     majcol, ier)
			  ELSE IF ( nc .eq. 4 )  THEN
			    CALL poly_cvgfw (fname, nclip, xclip, yclip,
     +				             lintyp (2), filtyp(4), 
     +					     majcol, ier)
			END IF
C
		        IF ( xclip ( 1 ) .ne. xclip ( nclip ) .or.
     +			     yclip ( 1 ) .ne. yclip ( nclip ) ) THEN
			    xclip ( nclip + 1 ) = xclip ( 1 )
			    yclip ( nclip + 1 ) = yclip ( 1 )
			    nclip = nclip + 1
		        END IF
		        CALL POLY_CAP ( lunp, nc, nopoly, nclip, xclip,
     +				        yclip, ier )
		    END IF
		    CALL FL_CLOS ( lunp, ier )
		END DO
	    END DO
	END DO
C
C*	Creat "empty" warning map.
C
	IF ( ncount .eq. 0 )  THEN
	    IF ( map_on )  THEN
		nzon = npzon 
	      ELSE
		nzon = 0
	    END IF
	    majcol = 3
	    CALL POLY_CVGFW ( fname, nzon, zzlat, zzlon, lintyp ( 2 ),
     +			      filtyp ( 4 ), majcol, ierr )
	    IF ( ierr .ne. 0 ) CALL ER_WMSG ( 'GPOLYG', ierr, ' ', ier )
C
	    CALL POLY_OPNF ( nopoly, lunp, cname, ier )
	END IF
C*
	RETURN
	END
