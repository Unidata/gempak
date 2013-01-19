	SUBROUTINE SNSGRD  ( iytype, xmin, xmax, ymin, ymax, ratio, 
     +			     rmargn, y, x, iret )
C************************************************************************
C* SNSGRD								*
C*									*
C* This subroutine sets up the graph coordinates and gets the y and	*
C* x coordinates for a LLMAXD by LLMAXD grid on the plot.  The margins	*
C* for the plot are also set.						*
C*									*
C* SNSGRD ( IYTYPE, XMIN, XMAX, YMIN, YMAX, RATIO, RMARGN, Y, X, IRET)	*
C*									*
C* Input parameters:							*
C*	IYTYPE		INTEGER		Y coordinate type		*
C*	XMIN		REAL		Min x value			*
C*	XMAX		REAL		Max x value			*
C*	YMIN		REAL		Min y value			*
C*	YMAX		REAL		Max y value			*
C*	RATIO		REAL		Height to width plot ratio	*
C*	RMARGN (4)	REAL		Margins				*
C*									*
C* Output parameters:							*
C*	Y(*)		REAL		Y grid points			*
C*	X(*)		REAL		X grid points			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-10 = error setting up graph	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/85						*
C* G. Huffman/GSC	11/88	Documentation				*
C* S. Schotz/GSC	 7/90   Add margins and ratio as input		*
C* M. desJardins/GSFC	 9/90	Set up graph coordinates using PTYPE	*
C* K. Brill/EMC		 4/99   Set MAXD grid dimension in PARAMETER	*
C* T. Lee/GSC		 7/00	Moved MAXD to GEMPRM.PRM & named LLMAXD	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		y (*), x (*), rmargn (*)
C*
	REAL		xin (LLMAXD), yin (LLMAXD), dum (LLMAXD)
C------------------------------------------------------------------------
	iret = 0
C
C*	Set up margins.
C
	IF  ( rmargn (1) .lt. 0. ) THEN
	    CALL GSGMGN ( 6., 4., 6., 1., ier )
          ELSE
	    CALL GSGMGN ( rmargn (1), rmargn (2), rmargn (3), 
     +                    rmargn (4), ier )
        END IF
C
C*	Set the graph coordinates.
C
	CALL GSGRAF  ( 1, iytype, ratio, xmin, ymin, xmax, ymax, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -10
	    CALL ER_WMSG  ( 'SNCROSS', iret, ' ', ier )
	    RETURN
	END IF
C
C*	Query the bounds in normalized coordinates.
C
	CALL GQBND  ( 'P', xl, yb, xr, yt, ier )
	gm1 = FLOAT ( LLMAXD - 1 )
	dy = ( yt - yb ) / gm1
C
C*	Get the y values.
C
	yy = yb
	DO  i = 1, LLMAXD
	    xin (i) = xl
	    yin (i) = yy
	    yy = yy + dy
	END DO
	CALL GTRANS ( 'P', 'M', LLMAXD, xin, yin, dum, y, ier )
	y (LLMAXD) = ymax
C
C*	Get the x values which are linear.
C
	xx = xmin
	dx = ( xmax - xmin ) / gm1
	DO  i = 1, LLMAXD
	    x (i) = xx
	    xx = xx + dx
	END DO
	x (LLMAXD) = xmax
C*
	RETURN
	END
