	SUBROUTINE SNHGRF  ( xstrt, xstop, ystrt, ystop, margin, iret )
C************************************************************************
C* SNHGRF								*
C*									*
C* This subroutine sets the graph margins and sets up the graph in	*
C* GEMPLT.  								*
C*									*
C* SNHGRF  ( XSTRT, XSTOP, YSTRT, YSTOP, MARGIN, IRET )		 	*
C*									*
C* Input parameters:							*
C*	XSTRT		REAL		Left x				*
C*	XSTOP		REAL		Right x				*
C*	YSTRT		REAL		Bottom y			*
C*	YSTOP		REAL		Top y				*
C*	MARGIN		CHAR*		Input margins			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -8 = error setting graph	*
C**									*
C* Log:									*
C* G. Huffman/USRA	 8/89	Adapted from SNPGRF			*
C* S. Schotz/GSC	 8/90	Increased default margin size		*
C* K. Brill/NMC         01/92   Replace GERROR with ER_WMSG             *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	margin
C*
	REAL		rmgn (4)
C----------------------------------------------------------------------
	iret = 0
C
C*	Set graph margins.
C
	IF  ( margin .eq. ' ' )  THEN
	    CALL GSGMGN  ( 5., 5., 5., 5., ier )
	  ELSE
	    CALL ST_RLST  ( margin, ';', 0., 4, rmgn, n, ier )
	    IF  ( n .eq. 4 )  THEN
		CALL GSGMGN  ( rmgn (1), rmgn (2), rmgn (3), rmgn (4),
     +			       ier )
	      ELSE
		CALL GSGMGN  ( 5., 5., 5., 5., ier )
	    END IF
	END IF
C
C*	Compute the aspect ratio and set the graph.
C
	IF  ( ( ( xstop - xstrt ) .ne. 0. ) .and.
     +	      ( ( ystop - ystrt ) .ne. 0. ) )  THEN
	    ratio = ( ystop - ystrt ) / ( xstop - xstrt )
	    IF  ( ratio .lt. 0. )  ratio = -ratio
C
	    CALL GSGRAF  ( 1, 1, ratio, xstrt, ystrt, xstop, ystop, 
     +			   				      iret )
	    IF  ( iret .ne. 0 )  THEN
	        CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
	        iret = -8
	        CALL ER_WMSG  ( 'SNHODO', iret, ' ', ier )
	    END IF
C
C*	  Error - one of the ranges is zero.
C
	  ELSE
	    iret = -8
	    CALL ER_WMSG  ( 'SNHODO', iret, ' ', ier )
	END IF
C*
	RETURN
	END
