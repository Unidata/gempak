	SUBROUTINE GDTXEV ( rlvlsi, nlvlsi, icord, rlvlso, nlvlso,
     +			    iret )
C************************************************************************
C* GDTXEV								*
C*									*
C* This subroutine gets evenly spaced levels for interpolation		*
C*									*
C* GDTXEV ( RLVLSI, NLVLSI, ICORD, RLVLSO, NLVLSO, IRET )		*
C*									*
C* Input parameters:							*
C*	RLVLSI (NLVLSI)	REAL		input levels			*
C*	NLVLSI		INTEGER		# of input levels		*
C*	ICORD		INTEGER		coordinate type:		*
C*					  1 = linear			*
C*					  2 = log			*
C*					  3 = **RKAPPA			*
C*									*
C* Output parameters:							*
C*	RLVLSO (NLVLSO)	REAL		output levels			*
C*	NLVLSO		INTEGER		# of output levels		*
C*	IRET		INTEGER		return code			*
C*					  0 = normal return		*
C*					 -4 = invalid vert coord	*
C**									*
C* Log:									*
C* K. F. Brill/GSC       7/89  						*
C* K. Brill/GSC          8/89           Interpolate to exact levels	*
C* K. Brill/GSC          1/90           Set lower/upper lvls for int	*
C* K. Brill/NMC		 1/91		Check for equality between 	*
C*        				input and output levels		*
C* T.W.Barker		 9/91		Created from gdtxgd		*
C* K. Brill/NMC		11/92		Fix calls to ER_WMSG		*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
        REAL 		rlvlsi(*),rlvlso(*)
C------------------------------------------------------------------------
	iret = 0
C*
C*
	rkinv = 1. / RKAPPA
C
C*	  Compute the vertical increment for the output grid using the 
C*        minimum vertical differential distance in the transformed
C*        coordinate.
C
	difmin = 1.e32
	DO k = 1, nlvlsi
	    IF ( icord .eq. 1 ) THEN
		qk = rlvlsi ( k )
	    ELSE IF ( icord .eq. 2 ) THEN
		IF ( rlvlsi ( k ) .gt. 0.0 ) THEN
		    qk = ALOG ( rlvlsi ( k ) )
		ELSE
		    iret = -7
		    CALL ER_WMSG ( 'GDTHGT', -4, ' ', ier )
		    RETURN
		END IF
	    ELSE IF ( icord .eq. 3 ) THEN
		IF ( rlvlsi ( k ) .gt. 0.0 ) THEN
		    qk = rlvlsi ( k ) ** RKAPPA
		ELSE
		    iret = -7
		    CALL ER_WMSG ( 'GDTHGT', -4, ' ', ier )
		    RETURN
		END IF
	    ELSE
		iret = -7
		CALL ER_WMSG ( 'GDTHGT', -4, ' ', ier )
		RETURN
	    END IF
	    IF ( k .gt. 1 ) THEN
		dif = ABS ( qk - qkm1 )
		IF ( dif .lt. difmin ) difmin = dif
	    END IF
	    qkm1 = qk
	END DO
C
C*      Compute the bounds in output coordinate system.
C
	ybot = rlvlsi(1)
	ytop = rlvlsi(nlvlsi)
        IF ( icord .eq. 1 ) THEN
	    q1 = ybot
	    q2 = ytop
        ELSE IF ( icord .eq. 2 ) THEN
	    IF ( ybot .gt. 0.0 .and. ytop .gt. 0.0 ) THEN
		q1 = ALOG ( ybot )
		q2 = ALOG ( ytop )
	    ELSE
		iret = -7
		CALL ER_WMSG ( 'GDTHGT', -4, ' ', ier )
		RETURN
	    END IF
	ELSE IF ( icord .eq. 3 ) THEN
	    IF ( ybot .gt. 0.0 .and. ytop .gt. 0.0 ) THEN
		q1 = ybot ** RKAPPA
		q2 = ytop ** RKAPPA
	    ELSE
		iret = -7
		CALL ER_WMSG ( 'GDTHGT', -4, ' ', ier )
		RETURN
	    END IF
        ELSE
	    iret = -7
	    CALL ER_WMSG ( 'GDTHGT', -4, ' ', ier )
	    RETURN
	END IF

C*      Determine the number of levels to which to interpolate.
C
	rnolv = ABS ( q1 - q2 ) / difmin
	nvout = INT ( rnolv + .5 ) + 1
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
	   IF ( icord .eq. 1 ) rlvlso ( k ) = z
           IF ( icord .eq. 2 ) rlvlso ( k ) = EXP ( z )
           IF ( icord .eq. 3 ) rlvlso ( k ) = z ** rkinv
	END DO	
C
C*	Set lower and upper most levels for the interpolation.
C
	rlvlso ( 1 ) = rlvlsi ( 1 )
	rlvlso ( nvout ) = rlvlsi ( nlvlsi )
	nlvlso = nvout
C
C*
	RETURN
	END
