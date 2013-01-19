	SUBROUTINE GDPGRF  ( iytype, ratio, xstrt, xstop, ystrt, ystop,
     +			     rmargn, windxn, windyn, iret )
C************************************************************************
C* GDPGRF								*
C*									*
C* This subroutine sets the graph margins and sets up the graph in	*
C* GEMPLT.  The margins are set to 6, 3, W, 5, where W = 6 * the	*
C* default wind barb size.						*
C*									*
C* GDPGRF  ( IYTYPE, RATIO, XSTRT, XSTOP, YSTRT, YSTOP, RMARGN,		*
C*           WINDXN, WINDYN, IRET )					*
C*									*
C* Input parameters:							*
C*	IYTYPE		INTEGER		Coordinate system		*
C*					  1 = linear			*
C*					  2 = log			*
C*					  3 = ** KAPPA			*
C*					  4 = log with skew x		*
C*	RATIO		REAL		Height to width plot ratio	*
C*	XSTRT		REAL		Left x				*
C*	XSTOP		REAL		Right x				*
C*	YSTRT		REAL		Bottom y			*
C*	YSTOP		REAL		Top y				*
C*	RMARGN (4)	REAL		Margins				*
C*									*
C* Output parameters:							*
C*	WINDXN		REAL		Default wind barb x size	*
C*	WINDYN		REAL		Default wind barb y size	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -8 = error setting graph	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* M. desJardins/GSFC	 1/89	Added margin				*
C* S Schotz/GSC		 1/90	Update call to gqbarb			*
C* K. Brill/NMC          7/90   RETURN if Skew T			*
C* S. Schotz/GSC	 7/90	Update for rmargn array			*
C* K. Brill/NMC         01/92   Replace GERROR with ER_WMSG             *
C* S. Jacobs/EAI	 2/94	Changed default margins to match SNPROF	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		rmargn (*)
C----------------------------------------------------------------------
	iret = 0
C
C*	Get wind barb sizes.
C
	CALL GQSYSZ  ( rmx, rmy, rtx, rty, rbx, rby, ier )
	CALL GQBARB  ( bsize, ibwid, ibtype, ier )
C
C*	Correct wind sizes back to default.
C
	windxn = rbx / bsize
	windyn = rby / bsize
C
C*	Get number of characters in six wind barbs.
C
	xleft = 6. * windxn / rtx
C
C*	Set graph margins.
C
	IF  ( rmargn (1) .lt. 0. ) THEN
	    CALL GSGMGN  ( 6., 3., xleft, 5., ier )
	  ELSE
	    CALL GSGMGN  ( rmargn (1), rmargn (2), rmargn (3), 
     +			   rmargn (4),    ier )
	END IF
C
C*	RETURN if this is a Skew T -- graph must be set by GG_SKEW.
C
	IF  ( iytype .eq. 4 )  THEN
C*
	    RETURN
C*
	END IF
C
C*	Set graph.
C
	jxtype = 1
	jytype = iytype
C*
	CALL GSGRAF  ( jxtype, jytype, ratio, xstrt, ystrt, xstop,
     +		       ystop,  iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
	    iret = -8
	    CALL ER_WMSG  ( 'GDPROF', iret, ' ', ier )
	END IF
C*
	RETURN
	END
