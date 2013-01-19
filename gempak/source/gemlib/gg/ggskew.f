	SUBROUTINE GG_SKEW  ( xaxis, yaxis, parm, ratio, xstrt, ystrt,
     +			      xstop, ystop, xlbl, nxlbl, iret )
C************************************************************************
C* GG_SKEW								*
C*									*
C* This subroutine sets the graphics for a skew T plot.  The aspect	*
C* ratio is computed.  A section of a standard skew T is determined	*
C* and GSGRAF is called.  IN_AXIS should be called first for both 	*
C* XAXIS and YAXIS to establish user input or default bounds.		*
C*									*
C* GG_SKEW  ( XAXIS, YAXIS, PARM, RATIO, XSTRT, YSTRT, XSTOP,		*
C*            YSTOP, XLBL, NXLBL, IRET )				*
C*									*
C* Input parameters:							*
C*	XAXIS		CHAR*		Input for X axis		*
C*	YAXIS		CHAR*		Input for Y axis		*
C*	PARM		CHAR*		Parameter list or function	*
C*									*
C* Input and output parameters:						*
C*	RATIO		REAL		Aspect ratio			*
C*	XSTRT		REAL		Minimum on T axis		*
C*	YSTRT		REAL		Maximum on P axis		*
C*	XSTOP		REAL		Maximum on T axis		*
C*	YSTOP		REAL		Minimum on P axis		*
C*	XLBL (NXLBL)	REAL		Label values for T axis		*
C*	NXLBL		INTEGER		Number of label values		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-12 = no temperature parm	*
C**									*
C* Log:									*
C* K. Brill/GSC		06/90     					*
C* S. Jacobs/SSAI	10/91	Changed number of extra lines 		*
C*				when plotting skewt to 30		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	xaxis, yaxis, parm
	REAL            xlbl (*)
C*
	REAL		tbnd ( 2, 3 )
	DATA		tbnd / -35., 35., -31., 95., 238.16, 308.16 /
	DATA            aspect / 1.16 / pbot, ptop / 1000., 100. /
C------------------------------------------------------------------------
	iret = 0
C
C*	Check PARM for temperature scale.
C
	CALL ST_LCUC ( parm, parm, ier )
	in = INDEX ( parm, 'TMPC' ) + INDEX ( parm, 'TEMP' ) +
     +       INDEX ( parm, 'DWPC' ) + INDEX ( parm, 'DWPT' ) +
     +       INDEX ( parm, 'TVRC' )
	IF ( in .ne. 0 ) THEN
	  itmp = 1
	ELSE
          in = INDEX ( parm, 'TMPF' ) + INDEX ( parm, 'DWPF' ) +
     +         INDEX ( parm, 'TVRF' )
	  IF ( in .ne. 0 ) THEN
	    itmp = 2
	  ELSE
	    in = INDEX ( parm, 'TMPK' ) + INDEX ( parm, 'DWPK' ) +
     +           INDEX ( parm, 'TVRK' )
	    IF ( in .ne. 0 ) THEN
	      itmp = 3
	    ELSE
	      iret = -12
	      CALL ER_WMSG ( 'GG', iret, ' ' , ier )
	      RETURN
	    END IF
	  END IF
	END IF
C
C*	Set the standard skew T.
C
	jxtype = 4
	jytype = 2
	CALL GSGRAF  ( jxtype, jytype, aspect, tbnd (1, itmp),
     +                 pbot, tbnd (2, itmp), ptop, iret )
C
C*      Set start and stop values for X axis.  For the Y axis, use
C*      the start and stop values from IN_AXIS.
C
	IF ( xaxis .ne. ' ' .and. xaxis (1:2) .ne. '//' ) THEN
C
C*	  Do nothing -- use the start and stop values from IN_AXIS.
C
	ELSE
C
C*	  Set start and stop values from the standard skew T.
C
	  CALL GTRANS ( 'M', 'N', 1, tbnd (1, itmp), pbot,
     +                  xn, ydum, ier )
	  CALL GTRANS ( 'M', 'N', 1, tbnd (1, itmp), ystrt,
     +                  xdum, yn, iret )
	  CALL GTRANS ( 'N', 'M', 1, xn, yn, xstrt, ydum, ier )
C*
	  CALL GTRANS ( 'M', 'N', 1, tbnd (2, itmp), pbot,
     +                  xn, ydum, ier )
	  CALL GTRANS ( 'N', 'M', 1, xn, yn, xstop, ydum, ier )
C
C*	  Set up label values for the X axis.
C
	  dmin = RMISSD
	  dmax = RMISSD
	  rint = RMISSD
	  CALL GR_AXLV ( dmin, dmax, xstrt, xstop, rint, .false.,
     +                   .false., xlbl, nxlbl, ier )
C
C*	  Add 30 lines; this number is arbitrary.
C
          rinc = xlbl (2) - xlbl (1)
	  IF  ( nxlbl+30 .le. LLAXIS )  THEN
	    iadd = 30
	  ELSE
	    iadd = LLAXIS - nxlbl
	  END IF
	  DO  i = nxlbl, 1, -1
	    xlbl ( i+iadd ) = xlbl ( i )
	  END DO
	  DO  i = iadd, 1, -1
	    xlbl ( i ) = xlbl ( i+1 ) - rinc
	  END DO
	  nxlbl = nxlbl + iadd
	END IF
C
C*	Compute the aspect ratio of this section of the skew T.
C
	CALL GTRANS ( 'M', 'N', 1, xstrt, ystrt, xn1, yn1, ier )
	CALL GTRANS ( 'M', 'N', 1, xstop, ystrt, xn2, ydum, ier )
	CALL GTRANS ( 'M', 'N', 1, xstop, ystop, xdum, yn2, ier )
	ratio = ABS ( ( yn2 - yn1 ) / ( xn2 - xn1 ) )
C
C*	Set up the graph.
C
	CALL GSGRAF  ( jxtype, jytype, ratio, xstrt, ystrt, xstop,
     +		       ystop,  iret )
C*
	RETURN
	END
