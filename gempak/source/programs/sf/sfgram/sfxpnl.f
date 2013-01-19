	SUBROUTINE SFXPNL  ( view, panin, iret )
C************************************************************************
C* SFXPNL								*
C*									*
C* This subroutine sets the view region of a trace within a panel.  The	*
C* view region of the trace is scaled based off the size of the view	*
C* region of the panel. 						*
C*									*
C* SFXPNL  ( VIEW, PANIN, IRET )					*
C*									*
C* Input parameters:							*
C*	VIEW (*)	REAL		View region of panel		*
C*	PANIN		CHAR*		Input for TRACE panel		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER 	Return code			*
C*					  0 = normal return		*
C*					-11 = error in setting view	*
C**									*
C* Log:									*
C* J. Whistler/SSAI	 7/91						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	panin
	REAL		view (*)
C*
	REAL		plot (4), trac (4)
C*
C------------------------------------------------------------------------
	iret= 0
C
C*	Break up the coordinates for the trace.
C
	CALL ST_RLST  ( panin, ';', RMISSD, 4, trac, num, ier )
C
C*	    Compute the ratio of the x and y direction of the panel
C*	    region to scale the trace regions.
C
	    ratx = view (3) - view (1)
	    raty = view (4) - view (2)
C*
	    plot (1) = view (1) + ( trac (1) * ratx )
	    plot (2) = view (2) + ( trac (2) * raty )
	    plot (3) = view (1) + ( trac (3) * ratx )
	    plot (4) = view (2) + ( trac (4) * raty )
C
C*	Set view region.
C
	    CALL GSVIEW  ( plot(1), plot (2), plot (3), plot (4), ier )
	    IF  ( ier .ne. 0 ) iret = -11
C*
	RETURN
	END
