	SUBROUTINE SNPGRF  ( iytype, ratio, xstrt, xstop, ystrt, ystop,
     +			     rmargn, windxn, windyn, iret )
C************************************************************************
C* SNPGRF								*
C*									*
C* This subroutine sets the graph margins and sets up the graph in	*
C* GEMPLT.  The margins are set to 6, 3, W, 5, where W = 6 * the	*
C* default wind barb size.						*
C*									*
C* SNPGRF  ( IYTYPE, RATIO, XSTRT, XSTOP, YSTRT, YSTOP, RMARGN, 	*
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
C*	RMARGN (4)	REAL		Input margins			*
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
C* M. desJardins/GSFC	 1/89	Added margins				*
C* S. Schotz/GSC	 1/90	Updated call to GQBARB			*
C* K. Brill/NMC          7/90   Removed Skew T setup			*
C* S. Schotz/GSC	 8/90	Changed default margin size		*
C* K. Brill/NMC         01/92   Replace GERROR with ER_WMSG             *
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
	CALL GQBARB  ( bsize, ibrwid, ibrtyp, ier )
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
	IF  ( rmargn (1) .lt. 0. )  THEN
	    CALL GSGMGN  ( 6., 3., xleft, 5., ier )
	  ELSE
	    CALL GSGMGN  ( rmargn (1), rmargn (2), rmargn (3),
     +			   rmargn (4), ier )
	END IF
C
C*	Set the graph if it is not a skew T.
C
	IF  ( iytype .eq. 4 )  RETURN
C*
	CALL GSGRAF  ( 1, iytype, ratio, xstrt, ystrt, xstop,
     +		       ystop,  iret )
	IF  ( iret .ne. 0 )  THEN
            CALL GEPLOT  ( ier )
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
	    iret = -8
	    CALL ER_WMSG  ( 'SNPROF', iret, ' ', ier )
	END IF
C*
	RETURN
	END
