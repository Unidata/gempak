	SUBROUTINE GDFILL  ( kx, ky, grid, ioffx, ioffy, iskip, nlvl, 
     +			     clvl, icolr, linlbl, iret )
C************************************************************************
C* GCFILL								*
C* 									*
C* This subroutine draws contours through a grid of data.  The 		*
C* algorithm used is based on a two-dimensional Lagrangian fitting of	*
C* the grid points.  It is the original GEMPAK contouring program.	*
C* The original subroutine, GCONTR, had a different calling sequence.	*
C* 									*
C* GCFILL  ( KX, KY, GRID, IOFFX, IOFFY, ISKIP, NLVL, CLVL, ICOLR,	*
C*           LINLBL, IRET )						*
C*									*
C* Input parameters:							*
C*									*
C*	KX		INTEGER		Number of x grid points 	*
C*	KY		INTEGER		Number of y grid points		*
C*	GRID (KX,KY)	REAL		Grid data array			*
C*	IOFFX		INTEGER		X offset to first point		*
C*	IOFFY		INTEGER		Y offset to first point		*
C*	ISKIP		INTEGER		Skip factor in original grid	*
C*	NLVL		INTEGER		Number of contour levels	*
C*	CLVL   (NLVL)	REAL		Contour level values		*
C*	ICOLR  (NLVL)	INTEGER		Contour color numbers		*
C*	LINLBL (NLVL)	INTEGER		Contour label types		*
C*									*
C* Output parameters:							*
C*									*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/NMC	11/91	From GCONTR				*
C* S. Jacobs/NCEP	 1/96	Added CADJST to adjust grid values	*
C* T. Lee/GSC		 9/97	Fixed typo, NOMOMO -> NOMONO		*
C* S. Jacobs/NCEP	 2/00	Eliminate subboxing when CONTUR=1	*
C* S. Chiswell/Unidata	 2/01	Copied from GCFILL			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'CONTUR.CMN'
C*
	REAL		grid (*), clvl (*)
	INTEGER		icolr (*), linlbl (*)
C*
	LOGICAL		bad
	REAL		clev (LLCLEV)
	REAL		xg(4), yg(4)
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Move the contour levels into a local array in order to add a
C*	maximum level.
C
	IF  ( nlvl .lt. LLCLEV )  THEN
	    nlev = nlvl
	  ELSE
	    nlev = LLCLEV - 1
	END IF
	bad = .false.
	DO  i = 1, nlev
	    clev (i) = clvl (i)
	    IF  ( i .gt. 1 )  THEN
		IF  ( clev (i) .le. clev (i-1) )  bad = .true.
	    END IF
	END DO
C
C*	If levels are not in the correct order, set error and return.
C
	IF  ( bad )  THEN
	    iret = NOMONO
	    RETURN
	END IF
C
C*	Build dummy maximum level.
C
	CALL GR_STAT  ( grid, kx, ky, 1, 1, kx, ky, gmin, gmax, gavg,
     +			gdev, ier )
	high = 2 * ABS ( gmax ) + ABS ( gmin )
	IF  ( high .le. clev (nlev) ) high = clev (nlev) + 1
	nlev = nlev + 1
	clev (nlev) = high
C
C*	Save the current color index
C
	CALL GQCOLR(isvcol, iret)
C
C*      Loop through all the grid boxes.
C
	sk = FLOAT ( iskip ) + 1.0
	DO  jll = 1, ky
 	  DO  ill = 1, kx
	     xg(1) = ( ill * sk + ioffx ) - ( .5 * sk )
	     yg(1) = ( jll * sk + ioffy ) - ( .5 * sk )
	     xg(2) = ( ill * sk + ioffx ) + ( .5 * sk )
	     yg(2) = ( jll * sk + ioffy ) - ( .5 * sk )
	     xg(3) = ( ill * sk + ioffx ) + ( .5 * sk )
	     yg(3) = ( jll * sk + ioffy ) + ( .5 * sk )
	     xg(4) = ( ill * sk + ioffx ) - ( .5 * sk )
	     yg(4) = ( jll * sk + ioffy ) + ( .5 * sk )
	     i = (jll - 1) * kx + ill
	     IF ( grid(i) .ne. RMISSD ) THEN
		icol = 1
		DO WHILE (( clev(icol) .lt. grid(i) ).and.
     +                    ( icol .lt. nlev ))
		   icol = icol + 1
	        END DO
		IF ( icolr(icol) .ne. 0 ) THEN
		   CALL GSCOLR(icolr(icol), iret)
	           CALL GFILL('G', 4, xg, yg, ier)
		END IF
	     END IF
          END DO
	END DO

	CALL GSCOLR( isvcol, iret )
	RETURN
	END
