	SUBROUTINE GSTRMLF  ( kx, ky, u, v, iminx, jminy, imaxx, jmaxy,
     +			     misflg, filtst, filtar, ststop,
     +			     dispc, displ, ux, vy, flag1, flag2,
     +                       coslat, gelon, iret )
C************************************************************************
C* GSTRMLF								*
C* 									*
C* This subroutine draws streamlines through a gridded vector		*
C* field.  Streamlines will be computed for a subgrid with the		*
C* lower left corner at (IMINX,JMINY) and the upper right corner	*
C* at (IMAXX,JMAXY).  The maximum number of points in the subgrid	*
C* is LLMXGD.  A grid coordinate transformation and a map / graph	*
C* coordinate transformation must be defined before GSTRMLF is		*
C* called.  The current line and arrow attributes apply to the		*
C* streamlines.  This subroutine is an adaptation of the NCAR		*
C* streamline subroutine STRMLN.					*
C*									*
C* GSTRMLF  ( KX, KY, U, V, IMINX, JMINY, IMAXX, JMAXY, MISFLG, FILTST,	*
C*	     FILTAR, STSTOP, DISPC, DISPL, UX, VY, FLAG1, FLAG2,        *
C*           COSLAT, GELON, IRET )					*
C* 									*
C* Input parameters:							*
C* 	KX		INTEGER		Number of x grid points 	*
C* 	KY		INTEGER		Number of y grid points 	*
C* 	U (KX,KY)	REAL		U - component grid		*
C* 	V (KX,KY)	REAL		V - component grid		*
C*	IMINX		INTEGER		First x point of subgrid	*
C* 	JMINY		INTEGER		First y point of subgrid	*
C*	IMAXX		INTEGER		Last  x point of subgrid	*
C*	JMAXY		INTEGER		Last  y point of subgrid	*
C* 	MISFLG 		LOGICAL		Interpolate missing data flag	*
C*      FILTST          REAL            Filter to thin strmlines        *
C*      FILTAR          REAL            Filter to thin strmline arrows  *
C*      STSTOP          REAL            Controls stopping of strmline   *
C*                                              near another strmline   *
C*      DISPC           REAL            Controls stopping of strmline   *
C*                                              when wind speed is small*
C*      DISPL           REAL            Controls pre-scaling of vectos  *
C* 									*
C* Temporary work parameters:						*
C*      U  (KX*KY)      REAL            U component of vector           *
C*      V  (KX*KY)      REAL            V component of vector           *
C*      FLAG1 (KX*KY)   LOGICAL         Flag to start in box            *
C*      FLAG2 (KX*KY)   LOGICAL         Flag to add arrow in box        *
C*      COSLAT (KX*KY)  REAL            Cosines of Latitudes		*
C*      GELON (KX*KY)   REAL            Longitudes			*
C* 									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Vilardo/RDS	12/84	GEMPLT Version 3.0			*
C* I. Graffman/RDS  	 7/85  	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 3/89	Changed to standard FORTRAN		*
C* S. Schotz/GSC	 5/90	Set arrow type to 1 to ensure arrow	*
C*				for streamlines				*
C* S. Schotz/GSC	 8/90	Update call to DSARRW			*
C* J. Whistler/SSAI	 6/91	Set all internal grids to size LLMXGD	*
C* S. Jacobs/EAI	 6/92	Update call to DSARRW			*
C* D.W.Plummer/NCEP	 5/96	Added 5 new parms to control strmlines	*
C* S. Gilbert/NCEP	 3/07	Removed calls to DSARRW			*
C* S. Gilbert/NCEP	 3/07	Added work arrays to call sequence      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE 	'ERROR.PRM'
C*
	REAL		u ( KX, KY ), v ( KX, KY )
	LOGICAL		misflg
C*
	REAL		ux (*), vy (*), coslat(*), gelon(*)
	LOGICAL		flag1 (*), flag2 (*)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Define the subgrid to be used by the contouring program.
C*	Interpolate missing data points is MISFLG is true.
C
	ix1 = iminx
	iy1 = jminy
	ix2 = imaxx
	iy2 = jmaxy
	IF  ( ix1 .le. 0 ) ix1 = 1
	IF  ( iy1 .le. 0 ) iy1 = 1
	IF  ( ( ix2 .eq. 0 ) .or. ( ix2 .gt. kx ) ) ix2 = kx
	IF  ( ( iy2 .eq. 0 ) .or. ( iy2 .gt. ky ) ) iy2 = ky
C
C*	Test for valid subgrid specifications. 
C
	nx = ix2 - ix1 + 1
	ny = iy2 - iy1 + 1
C
C*	Move data. Interpolate if necessary.
C
	knt = 0
	DO  j = iy1, iy2
	    DO  i = ix1, ix2
	      knt = knt + 1
C*
	      IF ( .not. misflg .or. .not. ERMISS (u( i, j ))) THEN
	        ux ( knt ) = u ( i, j ) 
	        vy ( knt ) = v ( i, j ) 
C*
	        ELSE IF ( ( i .gt. 1 ) .and. ( i .lt. kx ) .and.
     +			  ( .not. ERMISS (u( i-1, j ))) .and.
     +			  ( .not. ERMISS (u( i+1, j ))) )  THEN
		    ux ( knt ) = 0.5 * ( u(i+1, j) + u(i-1, j) )
		    vy ( knt ) = 0.5 * ( v(i+1, j) + v(i-1, j) )
C*
		ELSE IF ( ( j .gt. 1 ) .and. ( j .lt. ky ) .and.
     +			  ( .not. ERMISS (u( i, j-1 ))) .and.
     +			  ( .not. ERMISS (u( i, j+1 ))) )  THEN
		    ux ( knt ) = 0.5 * ( u(i, j+1 ) + u(i, j-1) )
		    vy ( knt ) = 0.5 * ( v(i, j+1 ) + v(i, j-1) )
C*
		ELSE IF ( ( i .gt. 2 ) .and. 
     +                    ( .not. ERMISS (u( i-2, j ))) .and.
     +			  ( .not. ERMISS (u( i-1, j ))) )  THEN
		    ux ( knt ) = 2.0 * u( i-1, j ) - u( i-2, j )
		    vy ( knt ) = 2.0 * v( i-1, j ) - v( i-2, j )
C*
		ELSE IF ( ( i .lt. kx -1 ) .and.
     +			  ( .not. ERMISS (u( i+1, j ))) .and.
     +			  ( .not. ERMISS (u( i+2, j ))) ) THEN
		    ux ( knt ) = 2.0 * u( i+1, j ) - u( i+2, j )
		    vy ( knt ) = 2.0 * v( i+1, j ) - v( i+2, j )
C*
		ELSE IF ( ( j .gt. 2 ) .and. 
     +		    	  ( .not. ERMISS (u( i, j-2 ))) .and.
     +			  ( .not. ERMISS (u( i, j-1 ))) ) THEN
		    ux ( knt ) = 2.0 * u( i, j-1 ) - u( i, j-2 )
		    vy ( knt ) = 2.0 * v( i, j-1 ) - v( i, j-2 )
C*
		ELSE IF ( ( j .lt. ky -1 ) .and.
     +			  ( .not. ERMISS (u( i, j+2 ))) .and.
     +			  ( .not. ERMISS (u( i, j+1 ))) ) THEN
		    ux ( knt ) = 2. * u( i, j+1 ) - u( i, j+2 )
		    vy ( knt ) = 2. * v( i, j+1 ) - v( i, j+2 )
C*
		ELSE
		    ux ( knt ) = RMISSD
		    vy ( knt ) = RMISSD
	      END IF
	    END DO
	END DO   
C
C*	Streamline routine.
C
	CALL STRMLN  (ux, vy, nx, ny, flag1, flag2, iminx, jminy, 
     +			filtst, filtar, ststop, dispc, displ, 
     +                  coslat, gelon, iret)
C
C*
	RETURN 
	END
