	SUBROUTINE IN_TEXT  ( text, iret )
C************************************************************************
C* IN_TEXT								*
C*									*
C* This subroutine decodes the text string which is in the form:	*
C*									*
C*       text size / font / width / border / rel rotn / 		*
C*	 just / hw,sw flag						*
C*									*
C* Note that the hw,sw flag can appear anywhere in the string.  The	*
C* specified characteristics are set in GEMPLT.				*
C*									*
C* If any parameter is not input, the current default will be used.	*
C* The GEMPLT graphics package must be initialized before this 		*
C* subroutine is called.						*
C*									*
C* IN_TEXT  ( TEXT, IRET )						*
C*									*
C* Input parameters:							*
C*	TEXT		CHAR*		Text input			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/88						*
C* S. Schotz/GSC	 1/90	Added text width			*
C* S. Maxwell/GSC	 1/97	Added call to IN_TXTN			*
C* S. Jacobs/NCEP	 8/97	Changed call to GSTEXT			*
C* I. Durham/GSC	12/97	Added ibrdr, irrotn, and ijust variables*
C************************************************************************
	CHARACTER*(*)	text
C------------------------------------------------------------------------
	CALL IN_TXTN ( text, ifont, ihwsw, siztxt, itxwid, ibrdr, irrotn,
     +  		ijust, iret )
C
C*	Set text attributes.
C
	CALL GSTEXT  ( ifont, ihwsw, siztxt, itxwid, ibrdr, irrotn,
     +  		ijust, ier )
C*
	RETURN
	END
