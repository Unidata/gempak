	SUBROUTINE GDXSCV ( w, ponth, wlvls, nx, nv, rlen, ivcord,
     +		   	    iyaxis, ystrt, ystop, aratio, ws, iret )
C************************************************************************
C* GDXSCV								*
C*									*
C* This subroutine scales the vertical component of the cross section	*
C* vector velocity.  The output vector can be the same as the input	*
C* vector.								*
C*									*
C* GDXSCV  ( W, PONTH, WLVLS, NX, NV, RLEN, IVCORD, IYAXIS, YSTRT,	*
C*           YSTOP, ARATIO, WS, IRET )					*
C*									*
C* Input parameters:							*
C*	W     (NX,NV)	REAL		Input array of vert comps	*
C*	PONTH (NX,NV)	REAL		P on theta (ivcord = 2)		*
C*  	WLVLS (NV)	REAL		Vertical levels			*
C*	NX		INTEGER		Number of points in horz	*
C*	NV		INTEGER		Number of points in vert	*
C*	RLEN		REAL		Length of xsectn ( m )		*
C*	IVCORD		INTEGER		Vertical coordinate		*
C*					  1 = PRES			*
C*					  2 = THET			*
C*   					  3 = HGHT			*
C*	IYAXIS		INTEGER		Vertical coordinate type:	*
C*					  1 = linear			*
C*					  2 = logarthmic		*
C*					  3 = ** RKAPPA			*
C*	YSTRT		REAL		Vertical coordinate lower bnd	*
C*	YSTOP		REAL		Vertical coordinate upper bnd	*
C*	ARATIO 		REAL		Aspect ratio of plot		*
C*					  0 = compute aspect ratio	*
C*									*
C* Output parameters:							*
C*	WS    (NX,NV)	REAL		Output scaled vert comps	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = invalid vert coord	*
C*					 -7 = invalid vert axis type 	*
C**									*
C* Log:									*
C* K. Brill/GSC		 7/89  						*
C* M. desJardins/GSFC	 5/90	Cleaned up; declared PONTH		*
C************************************************************************
	INCLUDE 'GEMPRM.PRM'
C*
        REAL 		w (nx,*), ws (nx,*), wlvls (nv), ponth (nx,*)
C*
	REAL		rlvls (LLMXLV)
C*
	INCLUDE 'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	If input aspect ratio is 0, it is computed.
C
	ratio = aratio
	IF  ( ratio .lt. .001 )  THEN
	    CALL GQBND  ( 'P', xl, yb, xr, yt, ire )
	    IF  ( ire .ne. 0 )  THEN
		ratio = 1.0
	      ELSE
		ratio = ABS ( ( yt - yb ) / ( xr - xl ) )
	    END IF
	END IF
C
C*	Compute aspect ratio of plot times length.
C
	al = ratio * rlen
C
C*	Convert vertical coordinate to assumed standard units 
C*	corresponding to vertical velocity.  The assumptions are:
C*        1.  When IVCORD = 1, the p coordinate is in mb and the 
C*	      vertical motion is dp/dt (omega) in millibars/second.
C*        2.  When IVCORD = 2, the theta coordinate is in K and the
C*	      vertical motion is dp/dt (omega) in millibars/second.
C*	  3.  When IVCORD = 3, the height coordinate is in meters
C*            and the vertical motion is in cm/s.
C
	IF ( ivcord .eq. 1 ) THEN
	    zstrt = ystrt 
	    zstop = ystop 
	    DO  k = 1, nv
		rlvls ( k ) = wlvls ( k )
	    END DO
	  ELSE IF  ( ivcord .eq. 2 )  THEN
	    zstrt = ystrt
	    zstop = ystop
	    DO  k = 1, nv
		rlvls ( k ) = wlvls ( k )
	    END DO
	  ELSE IF ( ivcord .eq. 3 ) THEN
	    zstrt = ystrt * 100.0
	    zstop = ystop * 100.0
	    DO  k = 1, nv
		rlvls ( k ) = wlvls ( k ) * 100.0
	    END DO
	  ELSE
	    iret = -6
	    RETURN
	END IF
C
C*	Compute constant part of scale factor.
C
	IF  ( iyaxis .eq. 1 )  THEN
	    conf = al / ( zstop - zstrt )
	  ELSE IF  ( iyaxis .eq. 2 )  THEN
	    IF  ( ( zstop .le. 0.0 ) .or. ( zstrt .le. 0.0 ) )  THEN
		iret = -7
		RETURN
	    END IF
	    conf = al / ALOG ( zstop / zstrt )
	  ELSE IF ( iyaxis .eq. 3 ) THEN
	    IF  ( ( zstop .le. 0.0 ) .or. ( zstrt .le. 0.0 ) )  THEN
		iret = -7
		RETURN
	    END IF
	    rkpm1 = RKAPPA - 1.
	    conf = al / ( zstop ** RKAPPA - zstrt ** RKAPPA )
	  ELSE
		iret = -7
		RETURN
	END IF
C
C*	Do the scaling at all the levels.
C
	DO k = 1, nv
	    IF ( iyaxis .eq. 1 ) THEN
		scale = conf
	      ELSE IF ( iyaxis .eq. 2 ) THEN
		scale = conf / rlvls ( k )
	      ELSE IF ( iyaxis .eq. 3 ) THEN
		scale = conf * RKAPPA * rlvls ( k ) ** rkpm1
	    END IF
C*
	    DO  i = 1, nx
		IF  ( ivcord .eq. 2 )  THEN
		    IF ( ERMISS ( ponth ( i, k ) ) .or.
     +			 ERMISS ( w ( i, k ) ) )  THEN
			ws ( i, k ) = RMISSD
		      ELSE
			scale = (-scale) * RKAPPA * 
     +				 rlvls ( k ) / ponth ( i, k )
			ws ( i, k ) = scale * w ( i, k )
		    END IF
		  ELSE IF ( ERMISS ( w ( i, k ) ) ) THEN
		    ws ( i, k ) = RMISSD
		  ELSE
		    ws ( i, k ) = scale * w ( i, k )
		END IF
	    END DO
	END DO
C*
	RETURN
	END
