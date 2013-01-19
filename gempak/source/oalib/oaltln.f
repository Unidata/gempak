	SUBROUTINE OA_LTLN  ( kex, key, iextnd, gelat, gelon, coslat, 
     +			      iret )
C************************************************************************
C* OA_LTLN								*
C*									*
C* This subroutine computes the latitude and longitude at each grid 	*
C* point in the extend grid area.  The grid coordinate system must	*
C* be defined in GEMPLT before this subroutine is called.		*
C*									*
C* OA_LTLN  ( KEX, KEY, IEXTND, GELAT, GELON, COSLAT, IRET )		*
C*									*
C* Input parameters:							*
C*	KEX		INTEGER		# of x points in extend grid	*
C*	KEY		INTEGER		# of y points in extend grid	*
C*	IEXTND (4)	INTEGER		# of points to extend grid	*
C*									*
C* Output parameters:							*
C*	GELAT (KEX,KEY)	REAL		Latitudes in degrees		*
C*	GELON (KEX,KEY)	REAL		Longitudes in degrees		*
C*	COSLAT(KEX,KEY)	REAL		Cosine of latitude 		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = grid projection error	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/88	From GR_LTLN				*
C* K. Brill/NMC		06/91	Pass out cosine instead of cosine 	*
C*				squared of latitude			*
C* K. Brill/NMC         01/92   Replace GERROR with ER_WMSG             *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		gelat (*), gelon (*), coslat (*)
	INTEGER		iextnd (*)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Get grid values in grid coordinates.
C
	kxstrt = 1   - iextnd (1)
	kxstop = kex - iextnd (1)
	kystrt = 1   - iextnd (2)
	kystop = key - iextnd (2)
C
C*	Fill x, y arrays with grid point values. 
C
	k = 0
	DO  i = kystrt, kystop
	    fi =  FLOAT (i)
	    DO  j = kxstrt, kxstop
	        k = k + 1
	        gelon (k) = fi
	    END DO
	END DO
C
	DO  j = kxstrt, kxstop
	    fj = FLOAT (j)
	    k  = j + iextnd (1)
	    DO  i = kystrt, kystop
	        gelat (k) = fj
	        k = k + kex
	    END DO
	END DO
C
C*	Transform the points.
C
	kexy = kex * key
	CALL GTRANS ( 'G', 'M', kexy, gelat, gelon, gelat, gelon, iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
	    iret = -2
	    RETURN
	END IF
C
C*	Compute the square of the cosine of the latitude.
C
	DO  i = 1, kexy
	    IF  ( ERMISS ( gelat (i) ) )  THEN
		coslat (i) = 0.
	      ELSE
		coslat (i) = COS ( gelat (i) * DTR ) 
	    END IF
	END DO
C*
	RETURN
	END
