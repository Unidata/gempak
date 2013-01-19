	SUBROUTINE SFXCHR  ( iloc, parms, icolor, cdata, ntime, xval, 
     +			     iret )
C************************************************************************
C* SFXCHR								*
C*									*
C* This subroutine plots character data for SFGRAM.			*
C*									*
C* SFXCHR  ( ILOC, PARMS, ICOLOR, CDATA, NTIME, XVAL, IRET )		*
C*									*
C* Input parameters:							*
C*	ILOC		INTEGER		Location on axis (1,2,3)	*
C*	PARMS		CHAR*		Parm*condition			*
C*	ICOLOR		INTEGER		Color				*
C*	CDATA (NTIME)	REAL		Character data			*
C*	NTIME		INTEGER		Number of times			*
C*	XVAL (NTIME)	REAL		Points on x axis		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/90						*
C* S. Jacobs/NCEP	 9/97	Changed call to GQTEXT and GSTEXT	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
C*
	CHARACTER*(*)	parms, cdata (*)
	REAL		xval (*)
C*
	CHARACTER	condtn*8, ppp*4
C*
C------------------------------------------------------------------------
	iret = 0
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
C*	Get value on y axis.
C
	CALL SFXLOC  ( iloc, 'C', yval, ier )
C
C*	Get conditions.
C
	CALL SFXCND  ( condtn, size, iwidth, ier )
C
C*	Set the character size and width.
C
	CALL GQTEXT  ( itxfn, itxhw, sztext, itxwid,
     +		       ibrdr, irrotn, ijust, ier )
	CALL GSTEXT  ( 0, 0, size, iwidth, 0, 0, 0, ier )
C
C*	Loop through points writing text.
C
	DO  i = 1, ntime
	    IF  ( cdata (i) .ne. ' ' )  THEN
		CALL GTEXT  ( 'M', xval (i), yval, cdata (i), 90.,
     +			       0, 0, ier )
	    END IF
	END DO
C
C*	Reset text attributes.
C
	CALL GSTEXT  ( 0, 0, sztext, itxwid, 0, 0, 0, ier )
C*
	RETURN
	END
