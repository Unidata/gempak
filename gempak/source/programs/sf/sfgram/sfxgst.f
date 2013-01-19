	SUBROUTINE SFXGST  ( parms, icolor, data, ntime, xval, iret )
C************************************************************************
C* SFXGST								*
C*									*
C* This subroutine plots character data for SFGRAM.			*
C*									*
C* SFXGST  ( PARMS, ICOLOR, DATA, NTIME, XVAL, IRET )			*
C*									*
C* Input parameters:							*
C*	PARMS		CHAR*		Parm*condition			*
C*	ICOLOR		INTEGER		Color				*
C*	DATA (NTIME)	REAL		Data				*
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
C* T. Piper/SAIC	07/05	Added gchar variable			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	parms
	REAL		xval (*), data (*)
C*
	CHARACTER	condtn*8, gchar*4, ppp*4
C*
	INCLUDE		'ERMISS.FNC'
	DATA		gchar / 'G' /
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
	    IF  ( .not. ERMISS ( data (i) ) .and. 
     +		  ( data (i) .gt. 0. ) )  THEN
		CALL GTEXT  ( 'M', xval (i), data (i), gchar, 0., 0, 0, 
     +					ier )
	    END IF
	END DO
C
C*	Reset text attributes.
C
	CALL GSTEXT  ( 0, 0, sztext, itxwid, 0, 0, 0, ier )
C*
	RETURN
	END
