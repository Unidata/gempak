	SUBROUTINE GDMMRK  ( marker, kx, ky, ix1, iy1, ix2, iy2, ixinc,
     +			     istag, iyinc, iret )
C************************************************************************
C* GDMMRK								*
C*									*
C* This subroutine plots markers for GDMAP.				*
C*									*
C* GDMMRK  ( MARKER, KX, KY, IX1, IY1, IX2, IY2, IXINC, ISTAG, IYINC, 	*
C*	     IRET )							*
C*									*
C* Input parameters:							*
C*	MARKER		CHAR*		Input for MARKER		*
C*	KX		INTEGER		Number of points in x dir	*
C*	KY		INTEGER		Number of points in y dir	*
C*	IX1		INTEGER		Initial x point			*
C*	IY1		INTEGER		Initial y point			*
C*	IX2		INTEGER		Last x point			*
C*	IY2		INTEGER		Last y point			*
C*	IXINC		INTEGER		X increment			*
C*	ISTAG		INTEGER		Stagger increment		*
C*	IYINC		INTEGER		Y increment			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/85						*
C* M. desJardins/GSFC	 8/88	Cleaned up				*
C* G. Huffman/GSC	 1/89	No plotting for color .lt. 1		*
C* S. Jacobs/NMC	 9/94	Added staggered plotting		*
C************************************************************************
	CHARACTER*(*)	marker
C------------------------------------------------------------------------
	iret = 0
C
C*	Translate marker input.
C
	CALL IN_MARK  ( marker, imcolr, ier )
C
C*	If color number .lt. 1, don't plot.
C
	IF  ( imcolr .gt. 0 )  THEN
C
C*	    Set marker color.
C
	    CALL GSCOLR  ( imcolr, ier )
C
C*	    Plot markers.
C
	    ixstrt = ix1
	    DO  j = iy1, iy2, iyinc
		fj = FLOAT (j)
		DO  i = ixstrt, ix2, ixinc
		    fi = FLOAT (i)
		    CALL GMARK  ( 'G', 1, fi, fj, ier )
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
