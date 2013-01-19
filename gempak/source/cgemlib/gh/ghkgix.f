	SUBROUTINE GH_KGIX ( xbmin, xbmax, ybmin, ybmax, bxlab, bylab, 
     +			     nprev, xprev, yprev, bad, iret )
C************************************************************************
C* GH_KGIX								*
C*									*
C* This subroutine checks whether a label overlaps a previously plotted *
C* line.                                                                *
C*									*
C* GH_KGIX ( XBMIN, XBMAX, YBMIN, YBMAX, BXLAB, BYLAB, NPREV, XPREV,    *
C*	     YPREV, BAD, IRET )                                         *
C*									*
C* Input parameters:							*
C*      XBMIN 		REAL		Norm. x coord of label left     *
C*      XBMAX 		REAL		Norm. x coord of label right    *
C*      YBMIN 		REAL		Norm. x coord of label bottom   *
C*      YBMAX 		REAL		Norm. x coord of label top      *
C*	BXLAB (*)	REAL		Norm. x coords of label box     *
C*	BYLAB (*)	REAL		Norm. y coords of label box     *
C* 	NPREV		INTEGER		Number of prev. lines to compare*
C*	XPREV (2,*)	REAL		Norm. x coords of prev. lines   *
C*	YPREV (2,*)	REAL		Norm. y coords of prev. lines   *
C*                                                                      *
C* Output parameters:							*
C*	BAD		LOGICAL		Flag for overlap                *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return             *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 4/02	                                        *
C* D. Kidwell/NCEP	 3/03	Increased dimensions for previous lines *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	LOGICAL		bad
      	REAL		bxlab (*), bylab (*), xprev (2,*), yprev (2,*)
C*
	CHARACTER	sys*2
     	REAL 		xb (2), yb (2), xmin (7), xmax (7), ymin (7), 
     +			ymax (7)
C-----------------------------------------------------------------------
      	iret = 0
	bad  = .false.
	IF ( nprev .le. 0 ) RETURN
C
	sys  = 'N'
	CALL ST_NULL ( sys, sys, lens, ier )
	iline = 1
C
C*	The first comparison will be with a horizontal bisector of the
C*	label box.
C
	xb ( 1 ) = bxlab ( 1 )
	yb ( 1 ) = ( bylab ( 1 ) + bylab ( 2 ) ) * .5
	xb ( 2 ) = bxlab ( 3 )
	yb ( 2 ) = yb ( 1 )
C
C*	Get the extrema of the previous lines.
C
	DO ii = 1, nprev
	    xmin ( ii ) = AMIN1 ( xprev ( 1, ii ), xprev ( 2, ii ) )
	    xmax ( ii ) = AMAX1 ( xprev ( 1, ii ), xprev ( 2, ii ) )
	    ymin ( ii ) = AMIN1 ( yprev ( 1, ii ), yprev ( 2, ii ) )
	    ymax ( ii ) = AMAX1 ( yprev ( 1, ii ), yprev ( 2, ii ) )
	    IF ( ( xmax ( ii ) - xmin ( ii ) ) .lt. .01 ) THEN
	        xmin ( ii ) = xmin ( ii ) - .01
		xmax ( ii ) = xmax ( ii ) + .01
	    END IF
	    IF ( ( ymax ( ii ) - ymin ( ii ) ) .lt. .01 ) THEN
		ymin ( ii ) = ymin ( ii ) - .01
		ymax ( ii ) = ymax ( ii ) + .01
	    END IF
 	END DO
C
	DO WHILE ( .not. bad .and. ( iline .lt. 4 ) ) 
	    DO ii = 1, nprev
C
C*		Get the intersecting point.
C
 		CALL CGR_SEGINT ( sys, xprev (1,ii), yprev (1,ii), sys, 
     +				  xb, yb, sys, xint, yint, intxn, ier )
		IF ( intxn .eq. 1 ) THEN
		    bad = .true.
		  ELSE
C
C*		    Compare the intersecting point and the extrema.
C
		    IF ( ( xint .le. xbmax ) .and. 
     +			 ( xint .ge. xbmin ) .and.
     +			 ( yint .le. ybmax ) .and.
     +			 ( yint .ge. ybmin ) .and.
     +			 ( xint .le. xmax ( ii ) ) .and. 
     +			 ( xint .ge. xmin ( ii ) ) .and.
     +			 ( yint .le. ymax ( ii ) ) .and.
     +			 ( yint .ge. ymin ( ii ) ) ) THEN
			bad = .true.
		    END IF
		END IF
 	    END DO    
	    IF ( .not. bad ) THEN
		iline = iline + 1
C
C*		The second and third comparisons will be with the label
C*		box diagonals.
C
		IF ( iline .eq. 2 ) THEN
		    xb ( 1 ) = bxlab ( 1 )
		    yb ( 1 ) = bylab ( 1 )
		    xb ( 2 ) = bxlab ( 3 )
		    yb ( 2 ) = bylab ( 3 )
		  ELSE IF ( iline .eq. 3 ) THEN
		    xb ( 1 ) = bxlab ( 2 )
		    yb ( 1 ) = bylab ( 2 )
		    xb ( 2 ) = bxlab ( 4 )
		    yb ( 2 ) = bylab ( 4 )
		END IF
	    END IF
	END DO
C*
	RETURN
	END
