	SUBROUTINE IN_COLR ( colors, nexp, icolor, iret )
C************************************************************************
C* IN_COLR								*
C*									*
C* This subroutine converts the input for the COLORS variable into a	*
C* list of colors.  If the number of colors is less than the number	*
C* expected, the input colors will be repeated to fill the buffer.	*
C* If COLORS is blank, the default is color 1.				*
C*									*
C* The colors can be queried or set by name.  The color names		*
C* corresponding to the color numbers can be listed by ending the	*
C* color list with a ?.  Color numbers can be set to specific colors	*
C* by using the equal sign.  For example, 1=red;2=orange;3=blue;4;5?	*
C* sets color number 1 to red, 2 to orange, 3 to blue, and it lists	*
C* the current color names for all color numbers.  If =init follows	*
C* any number, the initial color assignment is restored, but any	*
C* accompanying color assignments by name are applied to alter the	*
C* initial colors.							*
C*									*
C* The color numbers can also be assigned RGB values following the	*
C* equal sign.  The values are separated by colons.  For example,	*
C* 3=51:76:204 assigns the color corresponding to RGB values 51, 76,	*
C* and 204 to color number 3.						*
C*									*
C* A range of colors can be entered as a starting number, ending	*
C* number and increment separated by dashes.  The increment may be	*
C* negative to indicate a decrement.  This is functionally just a	*
C* shorter way of entering a list of color numbers.  For example,	*
C* 9-3--3 or 9-3-3 is the same as specifying 9;6;3.  If the increment	*
C* is omitted, it is assumed to be 1 or -1.				*
C*									*
C* IN_COLR ( COLORS, NEXP, ICOLOR, IRET )				*
C*									*
C* Input parameters:							*
C*	COLORS		CHAR*		COLORS input			*
C*	NEXP		INTEGER		Number of colors		*
C*									*
C* Output parameters:							*
C*	ICOLOR (NEXP)	INTEGER		Color number array		*
C*	IRET		INTEGER		Return code			*
C*					+1 = colors blank		*
C*					 0 = normal return		*
C*					-1 = nexp less than 1?		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/88	Rewrote					*
C* G. Huffman/GSC	 1/89	Color .lt. 0 reset to 0; clean up IF's	*
C* S. Schotz/GSC	 8/90	Set or query color name			*
C* M. desJardins/GSFC	10/90	Eliminate write of blank line at end	*
C* S. Schotz/GSC	10/90	Add call to GEPLOT for list option	*
C* K. Brill/NMC		11/91	Dimension CNAME (LLCLEV)		*
C* K. Brill/NMC		01/92	Added RGB, INIT and range features	*
C*				Replaced CALL ST_C2I with ST_NUMB	*
C* K. Brill/NMC		01/92	List out NNCOLR colors upon query	*
C* M. desJardins/NMC	02/92	Clean up				*
C* M. desJardins/NMC	04/92	Correct setting RGB values		*
C* G. Krueger/EAI	11/95	Removed HLS;Added XNAME;Mod. RGB range	*
C* S. Jacobs/NCEP	 4/96	Changed default for RGBs from 0 to -1	*
C* S. Jacobs/NCEP	 6/96	Fixed typo in FORMAT statement		*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C* T. Lee/GSC		12/98	Fixed FORMAT statement, X -> 1X		*
C* T. Lee/SAIC		 2/02	Set KCOL to no greater than NEXP	*	
C* T. Lee/SAIC		 3/02	Set NCOL to no greater than NEXP	*
C* T. Piper/SAIC	07/06	Verify inputs, nexp and colrin		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	colors
	INTEGER		icolor (*)
C*
	LOGICAL		query
	INTEGER		irgb (3)
	CHARACTER	cname (LLCLEV)*24, carr (2)*16, name*16
	CHARACTER	colrin*80, first*4, last*4, inc*4, xname*32
C------------------------------------------------------------------------
	IF ( nexp .lt. 1 )  THEN
	    iret = -1
	    RETURN
	ENDIF
	colrin = colors
C
C*	Check if ? is last character of colors list.
C
	CALL ST_LSTR ( colrin, ilen, ier)
	IF ( ilen .lt. 1 )  THEN
	    iret = +1
	    icolor(1) = 0
	    RETURN
	ENDIF
	IF ( colrin(ilen:ilen) .eq. '?' )  THEN
	    query = .true.
	    colrin( ilen:ilen ) = ' '
	ELSE
	    query = .false.
	ENDIF
	iret = 0
	ncol = 0
C
C*	Check for semicolons to separate colors.
C
	CALL ST_CLST  ( colrin, ';', ' ', LLCLEV, cname, kcol, ier )
	kcol = MIN ( kcol, nexp )
	IF  ( kcol .eq. 0 )  kcol = 1
C
C*      Loop through each of the colors.
C
        DO  i = 1, kcol
C
C*	    Separate color number from the name or components to set.
C
	    CALL ST_CLST ( cname (i), '=', ' ', 2, carr, num, ier )
C
C*	    Check for a range.
C
	    CALL ST_RANG  ( carr (1), first, last, inc, ityp, ier )
C
C*	    First, consider the case that this is not a range.
C*	    Check to see if the rgb components are given ( icolon > 0 )
C*	    or if a name is given ( icolon = 1 ) or no color is to be
C*	    set ( icolon = -1 ).
C
	    IF  ( ityp .eq. 0 )  THEN
		ncol = ncol + 1
		IF  ( num .lt. 2 )  THEN
		    icolon = -1
		  ELSE
		    icolon = INDEX  ( carr (2), ':' )
		END IF
C
C*		Get the color number.
C
		CALL ST_NUMB  ( carr (1), jcolor, ier )
		IF  ( jcolor .eq. IMISSD )  jcolor = 1
		IF  ( ncol .le. nexp )  icolor (ncol) = jcolor
C
C*		Set the color by name.
C
	        IF  ( icolon .eq. 0 )  THEN
		    CALL ST_LCUC  ( carr (2), carr (2), ier )
		    IF  ( carr (2) .ne. 'INIT' )  THEN
			CALL GSCNAM  ( jcolor, carr (2), ier )
		      ELSE
			CALL GSCINT  ( ier )
		    END IF
C
C*		    Set the color by RGB components.
C
		  ELSE
		    CALL ST_ILST (carr (2), ':', -1, 3, irgb, num, ier)
		    IF  ( num .eq. 3 )  THEN
			CALL GSCRGB  ( jcolor, irgb (1), irgb (2),
     +				       irgb (3), ier )
		    END IF
                END IF
C
C*		Check for color range.
C*		Check for color initialization.
C
	      ELSE
		IF  ( num .ge. 2 )  THEN
		    CALL ST_LCUC  ( carr (2), carr (2), ier )
		    IF  ( carr (2) .eq. 'INIT' )  THEN
			CALL GSCINT  ( ier )
		    END IF
		END IF
C
C*		Decode range.
C
		CALL ST_NUMB  ( first, ifirst, ier )
		CALL ST_NUMB  ( last,  ilast,  ier )
		CALL ST_NUMB  ( inc,   incr,   ier )
	        IF  ( ityp .eq. 1 )  THEN
		    incr  = 1
	          ELSE
		    incr  = ABS  ( incr )
		END IF
		IF  ( ifirst .gt. ilast )  incr = - incr
		DO  icol = ifirst, ilast, incr
		    ncol = ncol + 1
		    IF  ( ncol .le. nexp )  THEN
			icolor (ncol) = icol
		    END IF
		END DO
		ncol = MIN ( ncol, nexp )
	    END IF
	END DO
C
C*	Set negative colors to 0.
C
	DO  ii = 1, ncol
	    IF  ( icolor (ii) .lt. 0 )  icolor (ii) = 0
	END DO
C
C*	Move colors into output array.
C
	DO  ii = ncol + 1, nexp
	    jj = MOD ( ii, ncol )
	    IF  ( jj .eq. 0 )  jj = ncol
	    icolor (ii) = icolor (jj)
	END DO
C
C*	Write out color information if ? specified by user.
C
	IF  ( query ) THEN
C
C*	    Find out how many colors there are.  Only check MXCLNM
C*	    colors.
C
	    CALL GQNCOL ( nncolr, ier )
	    IF  ( nncolr .ge. MXCLNM )  nncolr = MXCLNM
C
C*	    Write title.
C
	    WRITE (6, 500)
500	    FORMAT ( 2X, 'COLOR', 2X, 'COLOR NAME', 8X, 'RED', 2X,
     +               'GREEN', 2X, 'BLUE', 2X, 'X COLOR NAME' )
C
C*	    Loop through and get names and components of colors.
C
	    DO  ii = 1, nncolr
		name = ' '
		CALL GQCOMP  (  ii, name, ired, igreen, iblue, xname,
     +				ier )
		IF ( name  .eq. ' ' ) name  = 'redefined '
		IF ( xname .eq. ' ' ) xname = 'redefined '
		WRITE  ( 6, 510 ) ii, name, ired, igreen, iblue, xname
510		FORMAT ( 4X, I3, 2X, A16, 1X, I4, 3X, I4, 2X, I4, 2X,
     +                   A32 )
	    END DO
	    WRITE  ( 6, * )
	END IF
C*
	RETURN
	END
