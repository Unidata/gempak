	SUBROUTINE GDXGRD ( gridin, nx, nv, ivcord, iyaxis, rlvls,
     +                      ybot, ytop, lvlflg, grdout, qlvls, nvout,
     +			    iret)
C************************************************************************
C* GDXGRD								*
C*									*
C* This subroutine vertically interpolates values in a cross section	*
C* plane to regularly spaced levels in a specified vertical coordinate	*
C* type.								*
C*									*
C* GDXGRD ( GRIDIN, NX, NV, IVCORD, IYAXIS, RLVLS, YBOT, YTOP, LVLFLG,	*
C*          GRDOUT, QLVLS, NVOUT, IRET )				*
C*									*
C* Input parameters:							*
C*	GRIDIN (NX, NV)       REAL	Input cross section array	*
C*      NX		      INTEGER	Number of points in horz	*
C*      NV		      INTEGER	Number of points in vert	*
C*	IVCORD		      INTEGER   Vertical coordinate		*
C*	 			          1 = PRES			*
C*					  2 = THET			*
C*					  3 = HGHT			*
C*      IYAXIS		      INTEGER   Vertical coordinate type:	*
C*					  1 = linear			*
C*                                        2 = logarthmic		*
C*                                        3 = ** RKAPPA			*
C*      RLVLS (NV)	      REAL      Values of vert lvls in GRIDIN	*
C*	YBOT		      REAL	Lower bound for vert coord	*
C*      YTOP		      REAL	Upper bound for vert coord	*
C*	LVLFLG		      LOGICAL	Flag for levels:		*
C*					 .false. = compute QLVLS, NVOUT	*
C*					 .true.  = QLVLS, NVOUT input	*
C*									*
C* Output parameters:							*
C*      GRDOUT (NX, NVOUT)    REAL      Output cross section grid	*
C*	QLVLS (NVOUT)	      REAL	Values of vert lvls in GRDOUT	*
C*      NVOUT		      INTEGER   Number of vert lvls post interp	*
C*	IRET		      INTEGER	Return code			*
C*					  0 = normal return		*
C*					 -7 = invalid vert coord	*
C**									*
C* Log:									*
C* K. F. Brill/GSC       7/89  						*
C* K. Brill/GSC          8/89           Interpolate to exact levels	*
C* K. Brill/GSC          1/90           Set lower/upper lvls for int	*
C* K. Brill/NMC		 1/91		Check for equality between 	*
C*        				input and output levels		*
C* S. Gilbert/NCEP       8/07           Set new limit numgpts		*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
        REAL 		gridin (nx, nv), grdout (*), rlvls (nv)
	REAL		qlvls (*)
	LOGICAL		lvlflg
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C*
	IF ( .not. lvlflg ) THEN
C*
	  rkinv = 1./RKAPPA
C
C*	  Compute the vertical increment for the output grid using the 
C*        minimum vertical differential distance in the transformed
C*        coordinate.
C
	  difmin = 1.e32
	  DO k = 1, nv
            IF ( iyaxis .eq. 1 ) THEN
              qk = rlvls ( k )
            ELSE IF ( iyaxis .eq. 2 ) THEN
              IF ( rlvls ( k ) .gt. 0.0 ) THEN
                qk = ALOG ( rlvls ( k ) )
	      ELSE
                iret = -7
	        CALL ER_WMSG ( 'GDCROSS', -7, ' ', ier )
	        RETURN
	      END IF
	    ELSE IF ( iyaxis .eq. 3 ) THEN
	      IF ( rlvls ( k ) .gt. 0.0 ) THEN
                qk = rlvls ( k ) ** RKAPPA
              ELSE
                iret = -7
	        CALL ER_WMSG ( 'GDCROSS', -7, ' ', ier )
	        RETURN
	      END IF
	    ELSE
                iret = -7
	        CALL ER_WMSG ( 'GCDROS', -7, ' ', ier )
	        RETURN
	    END IF
	    IF ( k .gt. 1 ) THEN
              dif = ABS ( qk - qkm1 )
              IF ( dif .lt. difmin ) difmin = dif
	    END IF
	    IF ( k .eq. 1 ) q1 = qk
	    IF ( k .eq. nv ) qnv = qk
 	    qkm1 = qk
          END DO
C
C*        Compute the bounds in output coordinate system.
C
            IF ( iyaxis .eq. 1 ) THEN
              q1 = ybot
	      q2 = ytop
            ELSE IF ( iyaxis .eq. 2 ) THEN
              IF ( ybot .gt. 0.0 .and. ytop .gt. 0.0 ) THEN
                q1 = ALOG ( ybot )
	        q2 = ALOG ( ytop )
	      ELSE
                iret = -7
	        CALL ER_WMSG ( 'GDCROSS', -7, ' ', ier )
	        RETURN
	      END IF
	    ELSE IF ( iyaxis .eq. 3 ) THEN
	      IF ( ybot .gt. 0.0 .and. ytop .gt. 0.0 ) THEN
                q1 = ybot ** RKAPPA
	        q2 = ytop ** RKAPPA
              ELSE
                iret = -7
	        CALL ER_WMSG ( 'GCDROS', -7, ' ', ier )
	        RETURN
	      END IF
            ELSE
              iret = -7
              CALL ER_WMSG ( 'GCDROS', -7, ' ', ier )
	      RETURN
	    END IF
C
C*        Determine the number of levels to which to interpolate.
C
	  rnolv = ABS ( q1 - q2 ) / difmin
	  nvout = INT ( rnolv + .5 ) + 1
	  nvchk = nvout * nx
	  numgpts = LLMXLV * MXXPTS
	  IF ( nvchk .gt. numgpts ) nvout = numgpts / nx
	  IF ( nvout .gt. LLMXLV ) nvout = LLMXLV
C*
 	  nvm1 = nvout - 1
	  dz = ( q2 - q1 ) / FLOAT ( nvm1 )
C
C*	  Create array of output levels.
C
	  z = q1
	  DO k = 2, nvm1
	    z = z + dz
	    IF ( iyaxis .eq. 1 ) qlvls ( k ) = z
            IF ( iyaxis .eq. 2 ) qlvls ( k ) = EXP ( z )
            IF ( iyaxis .eq. 3 ) qlvls ( k ) = z ** rkinv
	  END DO	
C
C*	  Set lower and upper most levels for the interpolation.
C
	  qlvls ( 1 ) = rlvls ( 1 )
	  qlvls ( nvout ) = rlvls ( nv )
C
	END IF
C
C* 	Load output grid with missing values.
C
        istp = nvout * nx
	DO i = 1, istp
	  grdout ( i ) = RMISSD
	END DO
C
C*	Do the interpolation to the regularly spaced grid.
C
	DO  ko = 2, nv
	  DO kn = 1, nvout
	    indx = nx * ( kn - 1 )
	    q = qlvls ( kn )
C
C*	    Compute interpolation weights.
C
	    IF ( q .eq. rlvls ( ko - 1 ) ) THEN
 		wt = 0.
	    ELSE IF ( q .eq. rlvls ( ko ) ) THEN
  		wt = 1.0
	    ELSE IF
     +	       ( ( q .gt. rlvls ( ko - 1 ) .and. q .lt. rlvls ( ko ) )
     +                           .or.
     +           ( q .lt. rlvls ( ko - 1 ) .and. q .gt. rlvls ( ko ) ) )
     +                           THEN
	        IF ( ivcord .eq. 1 ) THEN
	            wt = ALOG ( q / rlvls ( ko - 1 ) ) /
     +			 ALOG ( rlvls ( ko ) / rlvls ( ko - 1 ) )
	        ELSE
	            wt = ( q - rlvls ( ko - 1 ) ) / 
     +                        ( rlvls ( ko ) - rlvls ( ko - 1 ) )
	        END IF
	    ELSE
		wt = -9999.
	    END IF
C
C*	    If the weight is positive, apply it.
C
	    IF ( wt .ge. 0.0 ) THEN
	        wb = 1.000 - wt
	        DO i = 1, nx
	            indx = indx + 1
	            IF ( ERMISS (gridin ( i, ko ) ) .OR.
     +	                ERMISS (gridin ( i, ko-1 ) ) ) THEN
                        grdout ( indx ) = RMISSD
		    ELSE
     	                grdout ( indx ) = gridin ( i, ko ) * wt +
     +                            gridin ( i, ko-1 ) * wb
	            END IF
	        END DO
	    END IF
	  END DO
	END DO
C*
	RETURN
	END
