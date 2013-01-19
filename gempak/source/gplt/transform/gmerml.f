 	SUBROUTINE GMERML ( mproj, np, xm, ym, anglr1, xl, yl, iret ) 
C************************************************************************
C* GMERML								*
C* 									*
C* This subroutine converts a point from map latitude/longitude		*
C* to intermediate coordinates for the general Mercator case.		*
C* See cylindrical class projection for the usual Mercator map.		*
C* 									*
C* GMERML  ( MPROJ, NP, XM, YM, ANGLR1, XL, YL, IRET )			*
C*									*
C* Input parameters:							*
C*	MPROJ		INTEGER		Projection type ( not used )	*
C*	NP		INTEGER		Number of points		*
C* 	XM(*)		REAL		Latitudes			*
C* 	YM(*)		REAL		Longitudes			*
C*	ANGLR1		REAL		Center longitude		*
C* 									*
C* Output parameters:							*
C* 	XL(*) 		REAL		X coordinates			*
C* 	YL(*) 		REAL		Y coordinates			*
C*	IRET		INTEGER		Return code			*
C** 									*
C* Log:									*
C* B. Doty/RDS		 4/87	GEMPLT 4				*
C* M. desJardins/GSFC	 1/88	Fixed missing data			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		xm (*), ym (*), xl (*), yl (*)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = NORMAL
C*
	DO  i = 1, np
	    IF  ( ( ERMISS ( xm (i) ) ) .or. ( ERMISS ( ym (i) ) ) )  
     +        THEN
		xl (i) = RMISSD
		yl (i) = RMISSD
	      ELSE
		xmr = xm (i) * DTR
		ymr = ym (i) * DTR
		angdif = ymr - anglr1
		rrr1 = TAN ( xmr ) 
		rrr2 = COS ( angdif ) 
		IF  ( ( rrr1 .eq. 0.0 ) .and. (rrr2 .eq. 0.0) )  THEN 
		    xl (i) = RMISSD
		    yl (i) = RMISSD
		  ELSE
		    b  = COS ( xmr ) * SIN ( angdif )
		    b1 = ( 1.0 + b ) / ( 1.0 - b )
		    xl (i) = 0.5 * LOG ( b1 )
		    yl (i) = ATAN2 ( rrr1 , rrr2 )
		END IF 
	    END IF
	END DO
C*
	RETURN
	END
