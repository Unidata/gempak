	SUBROUTINE G2T_APPTXT ( ktype, nt, string, iret )
C************************************************************************
C* G2T_APPTXT								*
C*									*
C* This program appends trending wind/wave RANGE text from the second	*
C* trending buffer to OFF text, OFFTXT.	 Three dots will be added	*
C* if the EXCEPT part exists.						*
C*									*
C* G2T_APPTXT ( KTYPE, NT, STRING, IRET )				*
C*									*
C* Input parameters:							*
C*	KTYPE		INTEGER			Data type		*
C*						 1 = wave		*
C*						 2 = wind		*
C*	NT		INTEGER		Nth time step			*
C*	STRING		CHAR*		String append to G2T text	*
C*									*
C* Output Parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		03/07						*
C* T. Lee/SAIC		04/07	Checked fsuba_d, subarea flag		*
C* T. Lee/SAIC		11/07	Added NT for combining period		*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	CHARACTER*(*)	string
	CHARACTER	dot3*3
	DATA		dot3 / '...' /
C-----------------------------------------------------------------------
	iret = 0
	IF  ( string .eq. ' ' ) RETURN
C
	CALL ST_LSTR ( offtxt ( nt ), lout, ier )
	CALL ST_LSTR ( string, lstr, ier )
C
C*      Add '...' between EXCEPT portion if needed.
C
	IF  ( eflag_d ( ktype, 1 ) )  THEN
	    offtxt ( nt ) = offtxt ( nt ) ( : lout - 1  ) // dot3 //
     +               	    string ( 2 : lstr )
	  ELSE
	    offtxt ( nt ) = offtxt ( nt ) ( : lout - 1  ) // 
     +			    string ( : lstr )
	END IF
C*
	RETURN
	END

