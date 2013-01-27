	SUBROUTINE GDXSUG  ( iyplot, ystrt, ystop, xstrt, xstop, ratio,
     +			     rmargn, iret )
C************************************************************************
C* GDXSUG								*
C*									*
C* This subroutine sets up the graph for a cross section.		*
C*									*
C* GDXSUG  ( IYPLOT, YSTRT, YSTOP, XSTRT, XSTOP, RATIO, RMARGN, IRET )	*
C*									*
C* Input parameters:							*
C*	IYPLOT		INTEGER		Y coordinate type		*
C*	YSTRT		REAL		Bottom y value			*
C*	YSTOP		REAL		Top y value			*
C*	XSTRT		REAL		Left x value			*
C*	XSTOP		REAL		Right x value			*
C*	RATIO		REAL		Height to width ratio		*
C*	RMARGN (4)	REAL		Input Margins			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -7 = invalid vert coord type	*
C**									*
C* Log:									*
C* K. F. Brill/NMC	 1/92   					*
C* K. Brill/NMC         01/92   Replace GERROR with ER_WMSG             *
C************************************************************************
	REAL		 rmargn(*)
C*
C------------------------------------------------------------------------
	iret = 0
C
C*	Set graph margins.
C
	IF  ( rmargn (1) .lt. 0 )  THEN
	    CALL GSGMGN  ( 6., 4., 4., 2., ier )
	  ELSE
	    CALL GSGMGN  ( rmargn (1), rmargn (2), rmargn (3), 
     +			   rmargn (4), ier )
	END IF
C
C*	Set graph.
C
	jxtype = 1
	jytype = iyplot
	IF  ( iyplot .eq. 4 )  THEN
	    iret = -7
            RETURN
	END IF
	CALL GSGRAF  ( jxtype, jytype, ratio, xstrt, ystrt, xstop,
     +		       ystop,  iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
	    iret = -8
	    CALL ER_WMSG  ( 'GDCROSS', iret, ' ', ier )
	    RETURN
	END IF
	RETURN
	END
