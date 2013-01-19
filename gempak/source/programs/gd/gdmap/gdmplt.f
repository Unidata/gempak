	SUBROUTINE GDMPLT  ( grid, kx, ky, ix1, iy1, ix2, iy2, ixinc,
     +			     istag, iyinc, color, positn, rmind, rmaxd,
     +			     iret )
C************************************************************************
C* GDMPLT								*
C*									*
C* This subroutine plots grid data for GDMAP.				*
C*									*
C* GDMPLT  ( GRID, KX, KY, IX1, IY1, IX2, IY2, IXINC, ISTAG, IYINC, 	*
C*	     COLOR, POSITN, RMIND, RMAXD, IRET )			*
C*									*
C* Input parameters:							*
C*	GRID (KX, KY)	REAL		Grid data			*
C*	KX		INTEGER		Number of points in x dir	*
C*	KY		INTEGER		Number of points in y dir	*
C*	IX1		INTEGER		First point in x dir		*
C*	IY1		INTEGER		First point in y dir		*
C*	IX2		INTEGER		Last point in x dir		*
C*	IY2		INTEGER		Last point in y dir		*
C*	IXINC		INTEGER		Increment in x dir		*
C*	ISTAG		INTEGER		Increment for stagger		*
C*	IYINC		INTEGER		Increment in y dir		*
C*	COLOR		CHAR*		Color				*
C*	POSITN	        CHAR*		Position number	   		*
C*	RMIND		REAL		Minimum valid value		*
C*	RMAXD		REAL		Maximum valid value		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/85						*
C* G. Huffman/GSC	 1/89	Don't plot colors less than 1		*
C* M. desJardins/GSFC	 2/91	Added valid range			*
C* S. Jacobs/NMC	 9/94	Added staggered plotting		*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *
C*                              DATA statement                          *
C* R. Jones/NCEP         6/05	Added code to support temperature plots *
C*				for WAF charts				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		grid ( kx, ky )
	CHARACTER*(*)	color, positn
C*
	CHARACTER	pstr*10, cpos(2)*12, ptype*12
C*
	INTEGER		iposx (9), iposy (9) 
	INCLUDE		'ERMISS.FNC'
	DATA		iposx  / 0, -1, -1, 3, 3, -1,  3, 0,  0 /
	DATA		iposy  / 0,  2,  0, 2, 0, -2, -2, 4, -4 /
C*
C------------------------------------------------------------------------
	iret  = 0
C
C*	Set the color for the plot.
C
	CALL IN_COLR  ( color, 1, icolor, ier )
C
C*	Only plot data if color is positive.
C
	IF  ( icolor .gt. 0 )  THEN
	    CALL GSCOLR  ( icolor, ier )
C
C*	    Get the offsets for the position number.
C
	    CALL ST_CLST  ( positn, '/', ' ', 2, cpos, n, ier )
            CALL ST_NUMB ( cpos(1), ipos, ier )
	    CALL ST_LCUC ( cpos(2), ptype, ier )
C
	    IF  ( ( ipos .lt. 0 ) .or. ( ipos .gt. 8 ) )  ipos = 0
	    iyoff = iposy ( ipos + 1 )
	    ixoff = iposx ( ipos + 1 )
	    ix    = ixoff
C
C*	    Loop through the grid.
C
	    ixstrt = ix1
	    DO  j = iy1, iy2, iyinc
		fy = FLOAT (j)
C*
		DO  i = ixstrt, ix2, ixinc
		    fx = FLOAT (i)
		    d  = grid ( i, j )
		    IF  ( ( .not. ERMISS (d) ) .and.
     +			  ( d .ge. rmind ) .and. ( d .le. rmaxd ) )  
     +								THEN
	                IF ( ptype .eq. 'WAFT' ) THEN
                            CALL WAFST ( d, pstr )
                        ELSE		
		            id = NINT (d)
			    CALL ST_INCH  ( id, pstr, ier )
			END IF
			CALL ST_LSTR  ( pstr, len, ier )
			IF  ( ( ipos .eq. 0 ) .or. ( ipos .eq. 7 ) .or.
     +			      ( ipos .eq. 8 ) )  THEN
			    ix = ixoff - len + 1
			ELSE IF  ( (ipos.eq.1) .or. (ipos.eq.2) .or.
     +			   	   ( ipos.eq.5) )  THEN
			    ix = ixoff - 2 * len
			END IF
			CALL GTEXT  ( 'G', fx, fy, pstr, 0.0, ix, iyoff,
     +				      ier )
		    END IF
		END DO
		IF  ( ixstrt .eq. ix1 )  THEN
		    ixstrt = ixstrt + istag
		ELSE
		    ixstrt = ix1
		END IF
	    END DO
	END IF
C*
	RETURN
	END
C*=======================================================================
	SUBROUTINE WAFST ( val, pstr )
C************************************************************************
C*  WAFST								*
C*									*
C*  This subroutine produces a character string that reflects the WAF	*
C*  convention for plotting temperature values.				*
C*									*
C*  WAFST ( VAL, PSTR )							*
C*									*
C*  Input parameters:							*
C*	VAL		REAL		Value to be checked		*
C*									*
C*  Output parameters:							*
C*	PSTR		CHAR*		Value as a character		*
C**									*
C*  Log:								*
C*  R. Jones/NCEP	6/05	Original code				*
C************************************************************************
       CHARACTER 	pstr*10, tstr*10
C------------------------------------------------------------------------
C*	Test for sign of number
C
	kval = NINT ( ABS (val) )
	CALL ST_INCH ( kval, pstr, ier )
C
	IF  ( val .gt. 0.0 )  THEN
	    tstr = '+' // pstr
	    pstr = tstr
	END IF
C*
	RETURN
	END
