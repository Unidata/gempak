	SUBROUTINE GG_WSTR  ( string, line, iret )
C************************************************************************
C* GG_WSTR								*
C*									*
C* This subroutine writes a string on a graphics plot.  The		*
C* string will be centered on the line specified.  If LINE = 0, the	*
C* string will be written one line from the bottom of the plot.		*
C*									*
C* GG_WSTR  ( STRING, LINE, IRET )					*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String to be written		*
C*	LINE		INTEGER		Line number			*
C*				  	  <0 = lines from bottom	*
C*				   	   0 = bottom line		*
C*				  	  >0 = lines from top		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				  	   0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/85						*
C* M. desJardins/GSFC	 5/88	Documentation				*
C* G. Krueger/EAI	 9/93	Modified to attach to view space.	*
C* S. Jacobs/EAI	 9/93	Modified to attach to plot space;	*
C*				   Use margins to determine location	*
C* S. Jacobs/EAI	 1/94	Added fix for plotting graph titles	*
C* S. Jacobs/NMC	 8/94	Added GTEXTC for centering		*
C* S. Jacobs/NCEP	 8/97	Changed call to GQTEXT			*
C* S. Jacobs/NCEP	12/97	Changed GSTEXT to set Centered No Clip	*
C* S. Jacobs/NCEP	 7/98	Changed to call GTEXTC again		*
C************************************************************************
	CHARACTER*(*)	string
C------------------------------------------------------------------------
	iret = 0
C
C*	Check length of string.  If 0, exit.
C
	CALL ST_LSTR  ( string, ilen, ier )
	IF  ( ilen .gt. 0 ) THEN
C
C*	    Get bounds of view region.
C
	    CALL GQBND  ( 'V', xl, yb, xr, yt, ier )
	    CALL GQBND  ( 'P', al, bb, ar, bt, ier )
C
C*	    Get the graphics mode. Then get the margins for the plot.
C
	    CALL GQMODE ( mode, ier )
	    IF  ( mode .eq. 1 )  THEN
		CALL GQMMGN ( xml, ymb, xmr, ymt, ier )
	    ELSE IF  ( mode .eq. 2 )  THEN
		CALL GQGMGN ( xml, ymb, xmr, ymt, ier )
	    ELSE
		iret = -1
		RETURN
	    END IF
C
C*	    Calculate the starting point for the string.
C*	    For the starting line, start from the top or bottom as
C*	    specified by line.
C*	    For the x direction, start at the center using length
C*	    as the offset.
C
	    IF  ( line .eq. 0 )  THEN
		iyoff = 1 - 2 * ymb
		yline = bb
	    ELSE IF  ( line .lt. 0 )  THEN
		iyoff = 1 - 2 * ( ymb + line + 1 )
		yline = bb
	    ELSE
		iyoff = 2 * ( ymt - line ) + 1
		yline = bt
	    END IF
C*
	    xline = ( xr + xl ) / 2.
	    ixoff =   ilen - 1
C
C*	    Query and save the current text attributes, then set the
C*	    justification to "Centered", write the string and reset
C*	    the justification to the saved value.
C
	    CALL GQTEXT ( itxfn, itxhw, sztext, itxwid, 
     +			  ibrdr, irrotn, ijust, ier )
	    CALL GSTEXT ( 0, 0, 0, 0, 0, 0, 2, ier )
	    CALL GTEXTC ( 'V', xline, yline, string, 0.,
     +			  0, iyoff, ier )
	    CALL GSTEXT ( 0, 0, 0, 0, 0, 0, ijust, ier )
	END IF
C*
	RETURN
	END
