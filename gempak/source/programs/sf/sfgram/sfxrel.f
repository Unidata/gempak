	SUBROUTINE SFXREL  ( parms, icolor, data, ntime, mkcolr,
     +			     xmndst, xval, iret )
C************************************************************************
C* SFXREL								*
C*									*
C* This subroutine plots real data for SFGRAM.				*
C*									*
C* SFXREL  ( PARMS, ICOLOR, DATA, NTIME, MKCOLR, XMNDST, XVAL, IRET )	*
C*									*
C* Input parameters:							*
C*	PARMS		CHAR*		Parm*condition			*
C*	ICOLOR		INTEGER		Color				*
C*	DATA (NTIME)	REAL		Data				*
C*	NTIME		INTEGER		Number of times			*
C*	MKCOLR		INTEGER		Marker color			*
C*	XMNDST		REAL		Max space for connecting pts	*
C*	XVAL (NTIME)	REAL		Points on x axis		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/90						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	parms
	REAL		xval (*), data (*)
C*
	CHARACTER	condtn*8, ppp
	REAL		xxx (LLMXTM), yyy (LLMXTM)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret  = 0
	xdist = 3. * xmndst
C
C*	Get the condition and parameter parts.
C
	condtn = parms (5: )
	ppp    = parms
C
C*	Set color.
C
	CALL GSCOLR  ( icolor, ier )
C
C*	Get conditions.
C
	CALL SFXCND  ( condtn, size, iwidth, ier )
	lintyp = size
C
C*	Set the line type and width.
C
	CALL GQLINE  ( ilntyp, ilthw, jwidth, iwhw, iret )
	CALL GSLINE  ( lintyp, 0, iwidth, 0, iret )
C
C*	Loop through points connecting lines.
C
	npt = 0
	DO  i = 1, ntime
	    IF  ( ERMISS ( data (i) ) )  THEN
		IF  ( npt .gt. 0 )  THEN
		    IF  ( ABS ( oldx - xval (i) ) .ge. xdist )  THEN
			CALL GLINE  ( 'M', npt, xxx, yyy, ier )
			IF  ( mkcolr .ne. 0 )  THEN
			    CALL GMARK  ( 'M', npt, xxx, yyy, ier )
			END IF
			npt = 0
		    END IF
		END IF
	      ELSE 
		npt = npt + 1
		xxx (npt) = xval (i)
		yyy (npt) = data (i)
		oldx = xval (i)
	    END IF
	END DO
C
C*	Flush last line segments.
C
	IF  ( npt .gt. 0 )  THEN
	    CALL GLINE  ( 'M', npt, xxx, yyy, ier )
	    IF  ( mkcolr .ne. 0 )  THEN
		CALL GMARK  ( 'M', npt, xxx, yyy, ier )
	    END IF
	END IF
C
C*	Reset line attributes.
C
	CALL GSLINE  ( ilntyp, 0, jwidth, 0, ier )
C*
	RETURN
	END
