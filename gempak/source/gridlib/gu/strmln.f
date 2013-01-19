	SUBROUTINE STRMLN  ( u, v, kx, ky, flag1, flag2, iminx, jminy, 
     +			     filtst, filtar, ststop, fdispc, fdispl, 
     +			     coslat, gelon, iret )
C************************************************************************
C* STRMLN								*
C*									*
C* This subroutine draws streamlines through a gridded vector field.	*
C* The coordinates of the streamlines will be in "G" coordinates.	*
C*									*
C* This subroutine has been adapted to GEMPAK from the NCAR subroutine	*
C* with the same name.							*
C*									*
C* STRMLN  ( U, V, KX, KY, FLAG1, FLAG2, IMINX, JMINY, FILTST, FILTAR,	*
C*		STSTOP, FDISPC, FDISPL, COSLAT, GELON, IRET )		*
C*									*
C* Input parameters:							*
C*	U  (KX,KY)	REAL		U component of vector		*
C*	V  (KX,KY)	REAL		V component of vector		*
C*	KX		INTEGER		Number of points in x dir	*
C*	KY		INTEGER		Number of points in y dir	*
C*	FLAG1 (KX,KY)	LOGICAL		Flag to start in box		*
C*	FLAG2 (KX,KY)	LOGICAL		Flag to add arrow in box	*
C*	IMINX		INTEGER		X grid coord of lower left	*
C*	JMINY		INTEGER		Y grid coord of lower left	*
C*      FILTST          REAL            Filter to thin strmlines        *
C*      FILTAR          REAL            Filter to thin strmline arrows  *
C*      STSTOP          REAL            Controls stopping of strmline   *
C*                                              near another strmline   *
C*      FDISPC          REAL            Controls stopping of strmline   *
C*                                              when wind speed is small*
C*      FDISPL          REAL            Controls pre-scaling of vectos  *
C*									*
C* Temporary work parameters:                                           *
C*      COSLAT (KX*KY)  REAL            Cosines of Latitudes            *
C*      GELON (KX*KY)   REAL            Longitudes                      *
C*                                                                      *
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Chatters/RDS	 2/83	Modified NCAR subroutine		*
C* M. desJardins/GSFC	 3/89	Standardized FORTRAN and cleaned up	*
C* K. Brill/NMC          9/90   Correction for CED  grids		*
C* D.W.Plummer/NCEP      5/96   Added filter to thin streamlines	*
C* M. Li/GSC		 9/00	flag1(i,j)->flag1(iii, jjj) in line 235	* 
C* T. Piper/SAIC	 1/02	Check for divide by zero in ATAN2 	*
C* T. Lee/SAIC		 3/07	Initialized ICHKB			*
C* S. Gilbert/NCEP       3/07   Added work arrays to call sequence      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		u ( KX, KY ), v ( KX, KY )
	LOGICAL		flag1 ( KX, KY ), flag2 ( KX, KY )
C*
	PARAMETER	( NCHK = 5555 )
	PARAMETER	( MXPTS = 100 )
	COMMON  / STR01 / is, iend, js, jend, iend1, jend1, i, j,
     +			  x, y, delx, dely
C*
	COMMON  / STR03 / iterp, iterc, displ, dispc, cstop, 
     +				fltar
C*
	COMMON  / STR04 / xchk (NCHK), ychk (NCHK), lchk, numchk, 
     +			  ichk, ichkb, nbox, uxsml
C*
	COMMON  / MVDR /  np, drmvx (MXPTS), drmvy (MXPTS), rminx, rminy
C*
	LOGICAL		done, forwrd
C*
	CHARACTER  	proj*4
	REAL            coslat ( * ), gelon (*)
C*
	INCLUDE         'ERMISS.FNC'
C*
	FDLI (Z,Z1,Z2,Z3,DX,DY) = (1.-DX)*((1.-DY)*Z +DY*Z1) +
     +				   DX *((1.-DY)*Z2+DY*Z3)
C*
C------------------------------------------------------------------------
C*	Assignment statements.  These were originally data statements.
C
	iterp = 35
	iterc = -99
C
C* 	- start new code
C
	filter = filtst
C	write(6,*)  "FILTER =", filter
	fltar  = filtar
C	write(6,*)  "FLTAR =", fltar
	cstop = ststop
C	write(6,*)  "CSTOP =", cstop
	dispc = fdispc
C	write(6,*)  "DISPC =", dispc
	displ = fdispl
C	write(6,*)  "DISPL =", displ
C*	- end new code
C 
	np    = 0
	iret  = 0
	rminx = FLOAT ( iminx ) - 1.0
	rminy = FLOAT ( jminy ) - 1.0
C
C*	Load /STR01/ with parameters.
C
	is    = 1
	iend  = kx
	js    = 1
	jend  = ky
	iend1 = iend - 1
	jend1 = jend - 1
C
C*	Initialize values in / STR04 /.
C
	uxsml  = 1. E-36
	numchk = NCHK
C
	lchk = 1
	ichk = 1
	xchk (1) = 0.
	ychk (1) = 0.
	ichkb = 0
	kflag = 0
C*
	kexy = kx * ky
C
C*	Query the grid projection, and, if it is CED, compute the 
C*      cosines of the latitudes.
C
	CALL GQGPRJ ( proj, ang1, ang2, ang3, kkxx, kkyy, dlatl,
     +                dlonl, dlatr, dlonr, ier )
	IF ( proj .eq. 'CED' ) THEN
	    ij = 0
	    DO j = js, jend
                DO i = is, iend
	            ij = ij + 1
	            coslat ( ij ) = FLOAT ( i ) + rminx
	            gelon ( ij ) = FLOAT ( j ) + rminy
	        END DO
            END DO
C
C*	    Transform the points.
C
	    CALL GTRANS ( 'G', 'M', kexy, coslat, gelon,
     +                    coslat, gelon, iret )
	    IF  ( iret .ne. 0 )  THEN
	        RETURN
	    END IF
C
C*	    Compute the cosine of the latitude.
C
	    DO  i = 1, kexy
	        IF  ( .not. ERMISS ( coslat (i) ) )  THEN
		    coslat (i) = COS ( coslat (i) * DTR )
	            IF ( coslat (i) .eq. 0.0 ) coslat (i) = RMISSD
	        END IF
	    END DO
	ELSE
	    DO i = 1, kexy
	      coslat ( i ) = 1.00
	    END DO
	END IF
C
C*	Compute the u- and v- components normalized to DISPL (.33) .
C
	ij = 0
	DO  j = js, jend
	    DO   i = is, iend
	      ij = ij + 1
	      IF ( ERMISS ( coslat (ij) ) ) THEN
	          u (i,j) = RMISSD
	          v (i,j) = RMISSD
	      END IF
	      IF  ( ( .not. ERMISS ( u (i,j) ) ) .and. 
     +		    ( .not. ERMISS ( v (i,j) ) ) )  THEN
		IF  ( ( u (i,j) .eq. 0. ) .and. ( v (i,j) .eq. 0. ) )
     +							THEN
C
C*		In the NCAR STRMLN program, the lowest two bits of
C*		the normalized u and v component arrays were used
C*		as flags.  These values are stored in the logical
C*		arrays, flag1 and flag2.  The reason for checking
C*		for small numbers was for portability.
C
		    u (i,j) = uxsml
		    v (i,j) = 0.
		  ELSE
		    con = displ / SQRT ( u (i,j) **2 + v (i,j)**2 )
		    u (i,j) = con * u (i,j)
		    v (i,j) = con * v (i,j)
		    IF ( u (i,j) .eq. 0.) u (i,j) = con * uxsml
		END IF
C
C*		The flag arrays have the following meaning:
C*		    FLAG1 = .false. ==> streamlines have passed
C*					through this box.
C*		    FLAG2 = .false. ==> box is eligible for an arrow.
C
		   flag1 (i,j) = .false.
		   flag2 (i,j) = .false.
	       ELSE
		   flag1 (i,j) = .true.
		   flag2 (i,j) = .true.
	      END IF
	    END DO
	END DO
C
C*	This code will find the next box for starting a streamline.
C*	Experience has shown that a pleasing picture will be produced 
C*	if new streamlines are started only in grid boxes that 
C*	previously have not had other streamlines pass through them,
C*	as long as a reasonably dense pattern of available boxes is
C*	prescribed.
C
	ij = 0
	nlin = 0
	DO  jjj = js, jend1
	  DO  iii = is, iend1
	    ij = ij + 1
C
C*	    -- start new code
C
	    IF  ( .not. flag1 (iii,jjj) )  THEN
		done = .false.
		loc = 1
		DO WHILE ( ( .not. done ) .and. ( loc .le. lchk ) )
	    	    IF ( ABS ( iii - xchk (loc) ) .lt. filter )  THEN
		    IF ( ABS ( jjj - ychk (loc) ) .lt. filter )  THEN
			done = .true.
			flag1 (iii,jjj) = .true.
	    	    END IF
	    	    END IF
	    	    loc = loc + 1
		END DO
	    END IF
C
C*	    -- end new code
C
	    IF  ( ( .not. flag1 (iii,jjj) ) .and. 
     +		  ( .not. ERMISS ( u (iii,jjj) ) ) .and.
     +		  ( .not. ERMISS ( u (iii,jjj+1) ) ) .and.
     +		  ( .not. ERMISS ( u (iii+1,jjj) ) ) .and.
     +		  ( .not. ERMISS ( u (iii+1,jjj+1))) )  THEN
C
C*		Initialize parameters for starting a streamline.
C*		Turn off the box for future streamlines.
C
		isav   = iii
		jsav   = jjj
		forwrd = .true.  
		flag1 (iii,jjj) = .true.
C
C*		Loop through drawing points.
C
		done = .false.
		DO WHILE  ( .not. done )
		    i = isav
		    j = jsav
		    IF  ( forwrd )  THEN
			plmn1 = +1.
		      ELSE
			plmn1 = -1.
		    END IF
C
C*		    Initiate the drawing sequence; start streamline in 
C*		    center of box.
C
		    nbox = 0
		    iter = 0
		    IF  ( .not. forwrd )  ichkb = ichk + 1
		    IF  ( ichkb .gt. numchk )  ichkb = 1
		    x = FLOAT (i) + 0.5
		    y = FLOAT (j) + 0.5
		    xbase = x
		    ybase = y
C
C*		    Flush the plot buffer and move to the new point.
C
		    IF  ( np .gt. 0 )  CALL GLINE  ( 'G', np, drmvx, 
     +						     drmvy, ier )
		    nlin = nlin + 1
		    np = 1
		    drmvx (np) = x + rminx
		    drmvy (np) = y + rminy
C
C*		    If this is the first point, add arrow.
C
		    IF  ( forwrd .and. ( .not. flag2 (i,j) ) )  THEN
			dx = x - AINT (x)
			dy = y - AINT (y)
C
C*			Use a bilinear interpolation formula.
C
C			delx = FDLI ( u(i,j), u(i,j+1), u(i+1,j),
C     +				      u(i+1,j+1), dx, dy )
C     +                         / coslat (ij)
			delx = (1.-dx)*((1.-dy)*u(i,j) + dy*u(i,j+1)) +
     +                         dx *((1.-dy)*u(i+1,j) + dy*u(i+1,j+1)) 
			delx = delx / coslat (ij)
C			dely = FDLI ( v(i,j), v(i,j+1), v(i+1,j),
C     +				      v(i+1,j+1), dx, dy )
			dely = (1.-dx)*((1.-dy)*v(i,j) + dy*v(i,j+1)) +
     +                         dx *((1.-dy)*v(i+1,j) + dy*v(i+1,j+1)) 
			CALL GSTARR  ( i, j, x, y, delx, dely, flag2, 
     +				       kx, ky, rminx, rminy, ier )
C 
C*		        - start new code
C
		        DO  iar = max(1,iii-nint(2*fltar)), 
     +                      min(iend,iii+nint(2*fltar))
		        DO  jar = max(1,jjj-nint(fltar)), 
     +                      min(jend,jjj+nint(fltar))
			    flag2(iar,jar) = .true.
			END DO
			END DO
C
C*		    - end new code
C
		    END IF
C
C*		    Draw streamlines.
C
		    done = .false.
		    DO WHILE  ( .not. done )
C
C*		        Check to see if streamline has entered new box.
C
			IF  ( ( i .ne. IFIX (x) ) .or. 
     +			      ( j .ne. IFIX (y) ) )  THEN
			    CALL GSTCHK  ( u, v, flag1, flag2, kx, ky, 
     +					   done, ier )
			END IF
C
C*			Calculate the displacement components.
C
			IF  ( .not. done )  THEN
			    dx = x - AINT (x)
			    dy = y - AINT (y)
C
C*			    Use a bilinear interpolation formula.
C
C			    delx = FDLI ( u(i,j), u(i,j+1), u(i+1,j),
C     +					  u(i+1,j+1), dx, dy )
C     +                             / coslat ( ij )
C
			    delx = (1.-dx)*((1.-dy)*u(i,j) + 
     +				    dy*u(i,j+1)) +
     +                              dx *((1.-dy)*u(i+1,j) + 
     +				    dy*u(i+1,j+1)) 
			    delx = delx / coslat (ij)
C			    dely = FDLI ( v(i,j), v(i,j+1), v(i+1,j),
C     +					  v(i+1,j+1), dx, dy )
			    dely = (1.-dx)*((1.-dy)*v(i,j) + 
     +				   dy*v(i,j+1)) +
     +                             dx *((1.-dy)*v(i+1,j) + 
     +			    dy*v(i+1,j+1)) 
C
C*			    Update the position and draw the vector.
C
			    x = x + plmn1 * delx
			    y = y + plmn1 * dely
C
C*			    This code adds the point to the buffer.
C
			    IF  ( np .eq. MXPTS )  THEN
				xx = drmvx (MXPTS)
				yy = drmvy (MXPTS)
				CALL GLINE ('G', np, drmvx, drmvy, ier)
				np = 1
				drmvx (1) = xx
				drmvy (1) = yy
			    END IF
			    np = np + 1
			    drmvx (np) = x + rminx
			    drmvy (np) = y + rminy
			    iter = iter + 1
C
C*			    Check streamlines every ITERP iterations.
C
			    IF  ( MOD ( iter, iterp ) .ne. 0 )  THEN
C
C*				Check the circular lists.
C
				IF  ( ( iterc .ge. 0 ) .and. 
     +				      ( MOD (iter,iterc) .eq. 0 ) )  
     +								THEN
				    CALL GSTLST  ( flag1, kx, ky, done, 
     +						   ier )
				  ELSE
				    done = .false.
				END IF
			      ELSE
				IF ( ( ABS ( x - xbase ) .lt. dispc ) 
     +						.and. 
     +				     ( ABS ( y - ybase ) .lt. dispc ) ) 
     +								THEN
				    done = .true.
				  ELSE
				    xbase = x
				    ybase = y
				END IF
			    END IF
			END IF
C
C			- start new code
C
			IF ( done .and. forwrd )  THEN
			    CALL GSTARR ( i, j, x, y, delx, dely, flag2,
     +				 	  kx, ky, rminx, rminy, ier )
		 	    DO iar = max(1,i-nint(2*fltar)), 
     +				     min(iend,i+nint(2*fltar))
				DO jar = max(1,j-nint(fltar)), 
     +					 min(jend,j+nint(fltar))
				    flag2(iar,jar) = .true.
				END DO
			    END DO
			END IF
C
C*			- end new code
C
		    END DO
C
		    IF  ( forwrd )  THEN
			forwrd = .false.
			done   = .false.
		    END IF
C
		END DO
C
	    END IF
C
	  END DO
C
	END DO
C
C*	Plot any points left in buffer.
C
	IF ( np .gt. 0 ) CALL GLINE  ( 'G', np, drmvx, drmvy, ier )
C
C	write(6,*) "Final count for ichk =", ichk
C*
	RETURN
	END
C------------------------------------------------------------------------
	SUBROUTINE GSTARR  ( i, j, x, y, delx, dely, flag2, kx, ky, 
     +			     rminx, rminy, iret )
C************************************************************************
C* GSTARR								*
C*									*
C* This subroutine draws an arrow head at the current location.		*
C*									*
C* GSTARR ( I, J, X, Y, DELX, DELY, FLAG2, KX, KY, RMINX, RMINY, IRET)	*
C*									*
C* Input parameters:							*
C*	I	INTEGER							*
C*	J	INTEGER							*
C*	X	REAL							*
C*	Y	REAL							*
C*	DELX								*
C*	DELY								*
C*	FLAG2								*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/89	Made into a subroutine from NCAR code	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	LOGICAL		flag2 ( KX, KY )
C------------------------------------------------------------------------
	iret = 0
C*
	IF  ( ( delx .eq. 0. ) .and. ( dely .eq. 0. ) )  RETURN
C
C*	Set the flag in this box.
C
	flag2  ( i, j ) = .true.
C
C*	Translate this point and the last point to N coordinates.
C*	Then compute the "wind" direction in order to draw an arrow.
C
	xs = x + rminx
	ys = y + rminy
	CALL GTRANS  ( 'G', 'N', 1, xs, ys, px, py, ier )
	xold = xs - delx
	yold = ys - dely
	CALL GTRANS ('G', 'N', 1, xold, yold, pxx, pyy, ier )
	ddxx = px - pxx
	ddyy = py - pyy
	IF ( ABS(ddxx) .lt. 0.00001 ) THEN
	    ddd = 0.0
	  ELSE
	    ddd = ATAN2 ( ddyy, ddxx )
	ENDIF
	ddd  = 270. - ddd * RTD
	px   = px 
	py   = py
	CALL GARRW  ( 'P', 1, px, py, 0., ddd, ier )
C*
	RETURN
	END
C------------------------------------------------------------------------
	SUBROUTINE GSTCHK  ( u, v, flag1, flag2, kx, ky, done, iret )
C************************************************************************
C* GSTCHK								*
C*									*
C* This subroutine is called when the streamlines have entered a new	*
C* grid box.  If the new box is at the edge of the grid or contains	*
C* missing data, a flag is set to indicate that the streamline should	*
C* terminate.  The following checks are performed:			*
C*    1)  Has the edge of the grid been reached.			*
C*    2)  Does the new grid box contain missing data.			*
C*    3)  Is the location too close to a previous streamline.		*
C*									*
C* GSTCHK  ( U, V, FLAG1, FLAG2, KX, KY, DONE, IRET )			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/89	Made into a subroutine using NCAR code	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		u ( KX, KY ), v ( KX, KY )
	LOGICAL		flag1 ( KX, KY ), flag2 ( KX, KY )
	LOGICAL		done
C*
	COMMON  / STR01 / is, iend, js, jend, iend1, jend1, i, j,
     +			  x, y, delx, dely
C*
	COMMON  / STR03 / iterp, iterc, displ, dispc, cstop, 
     +				fltar
C*
	PARAMETER	( NCHK = 5555 )
	COMMON  / STR04 / xchk (NCHK), ychk (NCHK), lchk, numchk, 
     +			  ichk, ichkb, nbox, uxsml
C*
	PARAMETER	( MXPTS = 100 )
	COMMON  / MVDR /  np, drmvx (MXPTS), drmvy (MXPTS), rminx, rminy
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	done = .false.
	nbox = nbox + 1
	ii = IFIX (x)
	jj = IFIX (y)
C
C*	Check for the edge of the grid.
C
	IF  ( ( IFIX (x) .lt. is ) .or. ( IFIX (x) .gt. iend1 ) .or.
     +	      ( IFIX (y) .lt. js ) .or. ( IFIX (y) .gt. jend1 ) )  THEN
	    done = .true.
C
C*	    - start new code
C
	    CALL GSTARR  ( i, j, x, y, delx, dely, flag2, kx, ky, 
     +			   rminx, rminy, ier )
	    DO  iar=max(1,i-nint(2*fltar)), min(iend,i+nint(2*fltar))
	        DO  jar=max(1,j-nint(fltar)), min(jend,j+nint(fltar))
		    flag2(iar,jar) = .true.
		END DO
	    END DO
C
C*	    - end new code
C
	    RETURN
C
C*	  Check for missing data.
C
	  ELSE IF  ( ( ERMISS ( u ( ii, jj ) ) ) .or. 
     +		     ( ERMISS ( u ( ii, jj+1 ) ) ) .or.
     +		     ( ERMISS ( u ( ii+1, jj ) ) ) .or. 
     +		     ( ERMISS ( u ( ii+1, jj+1 ) ) ) )  THEN
	    done = .true.
C
C*	    - start new code
C
	    CALL GSTARR  ( i, j, x, y, delx, dely, flag2, kx, ky, 
     +			   rminx, rminy, ier )
	    DO  iar=max(1,i-nint(2*fltar)), min(iend,i+nint(2*fltar))
	        DO  jar=max(1,j-nint(fltar)), min(jend,j+nint(fltar))
	            flag2(iar,jar) = .true.
	        END DO
	    END DO
C
C*	    - end new code
C
	    RETURN
	END IF
C
C*	Draw arrow if there is none in this box.
C
	IF  ( .not. flag2 (i,j) )  THEN
	    CALL GSTARR  ( i, j, x, y, delx, dely, flag2, kx, ky, 
     +			   rminx, rminy, ier )
C
C*	    - start new code
C
	    DO  iar=max(1,i-nint(2*fltar)), min(iend,i+nint(2*fltar))
		DO  jar=max(1,j-nint(fltar)), min(jend,j+nint(fltar))
		    flag2(iar,jar) = .true.
		END DO
	    END DO
C
C*	    - end new code
C
	END IF
C
C*	Check circular lists.
C
	CALL GSTLST  ( flag1, kx, ky, done, ier )
C*
	RETURN
	END
C------------------------------------------------------------------------
	SUBROUTINE GSTLST  ( flag1, kx, ky, done, iret )
C************************************************************************
C* GSTLST								*
C*									*
C* GSTLST  ( FLAG1, KX, KY, DONE, IRET )				*
C**									*
C************************************************************************
	LOGICAL		flag1 ( KX, KY )
	LOGICAL		done
C*
	COMMON  / STR01 / is, iend, js, jend, iend1, jend1, i, j,
     +			  x, y, delx, dely
C*
	COMMON  / STR03 / iterp, iterc, displ, dispc, cstop, 
     +				fltar
C*
	PARAMETER	( NCHK = 5555 )
	COMMON  / STR04 / xchk (NCHK), ychk (NCHK), lchk, numchk, 
     +			  ichk, ichkb, nbox, uxsml
C------------------------------------------------------------------------
C*	Compare crossing to previous crossings.
C
	done = .false.
	iret = 0
	loc  = 1
	DO WHILE  ( ( .not. done ) .and. ( loc .le. lchk ) )
C
	    IF ( ABS ( x - xchk (loc) ) .lt. cstop )  THEN
C
		IF ( ABS ( y - ychk (loc) ) .lt. cstop )  THEN
C
		    done = .true.
		    IF  ( (ichkb .le. ichk) .and. (loc .ge. ichkb) .and.
     +		          (loc   .le. ichk) )  THEN
			done = .false.
		    END IF
		END IF
C
		IF  ( ( ichkb .ge. ichk ) .and. 
     +		      ( (loc .ge. ichkb) .or. (loc .le. ichk) ) ) THEN
		    done = .false.
		END IF
	    END IF
	    loc = loc + 1
	END DO
	IF  ( done )  RETURN
C
C*	Set up new box values.
C
	lchk = MIN0 ( lchk + 1, numchk )
	ichk = ichk + 1
C	IF (ichk .gt. numchk) write(6,*) "Starting over with ichk"
	IF  ( ichk .gt. numchk )  ichk = 1
	xchk (ichk) = X
	ychk (ichk) = y
	i = IFIX (x)
	j = IFIX (y)
C
C*	Set flag for this box.
C
	flag1 (i,j) = .true.
	IF  ( nbox .ge. 5 )  THEN
	    ichkb = ichkb + 1
	    IF  ( ichkb .gt. numchk )  ichkb = 1
	END IF
C*
	RETURN
	END
