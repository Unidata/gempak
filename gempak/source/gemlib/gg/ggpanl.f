	SUBROUTINE GG_PANL  ( panel, iret )
C************************************************************************
C* GG_PANL								*
C*									*
C* This subroutine sets the view region for the panel specified.	*
C* If requested, a box will be drawn around the region.			*
C*									*
C* The input for PANEL specifies the panel location, panel outline	*
C* color, line type and width separated with slashes.  The panel 	*
C* location determines the location of the view region on the 		*
C* graphics device.  It may be specified using a number or 		*
C* abbreviation as follows:						*
C*									*
C*          NUMBER    ABBREVIATION       DESCRIPTION			*
C*            0           ALL            Entire device			*
C*            1            UL            Upper left quadrant		*
C*            2            UR            Upper right quadrant		*
C*            3            LL            Lower left quadrant		*
C*            4            LR            Lower right quadrant		*
C*            5             L            Left half			*
C*            6             R            Right half			*
C*            7             T            Top half			*
C*            8             B            Bottom half			*
C*									*
C* Horizontal or vertical panels which divide the screen into thirds	*
C* or fourths may be created using the syntax Tij where T is either	*
C* V for vertical or H for horizontal, i is 3 for thirds or 4 for	*
C* fourths, and j is the actual panel counting from the top or left.	*
C*									*
C* The view region may also be specified as four numbers separated 	*
C* with semicolons, giving the lower left and upper right corners 	*
C* in fractions of the graphics display area.  				*
C*									*
C* GG_PANL  ( PANEL, IRET )						*
C*									*
C* Input parameters:							*
C*	PANEL		CHAR*		Input for PANEL			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER 	Return code			*
C*					  0 = normal return		*
C*					 -9 = invalid region		*
C*					-10 = panel not recognized	*
C*					-11 = error in setting view	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/88						*
C* M. desJardins/GSFC	 5/89	Add vertical and horizontal panels	*
C* M. desJardins/NMC	10/91	Make panel *48				*
C* J. Nielsen/SUNYA	10/91	Add 9-panel capability--implemented MdJ	*
C* J. Whistler/SSAI	 7/92	Added ability to set REGION for box	*
C* S. Jacobs/EAI        10/92   Changed 9-panel from Mxx to Mx          *
C* S. Jacobs/NCEP	 4/96	Changed to use IN_COLR to parse color #	*
C* S. Jacobs/NCEP	10/99	Changed size of middle panel for thirds	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	panel
C*
	CHARACTER	panl1*48, panl2*48, region*1, line(3)*48
	REAL		view (4), sval (4), third (6), fourth (8)
	LOGICAL		setv
C*
	DATA		sval   / 0.0, .49, .51, 1.0 /
	DATA		third  / 0.0, .323, .338, .662, .677, 1.0 /
	DATA		fourth / 0.0, .24, .26, .49, .51, .74, .76, 1./
C------------------------------------------------------------------------
	iret= 0
C
C*	Break the panel into 2 parts, one for the view region and
C*	the second for the box.
C
	ibreak = INDEX ( panel, '/' )
	IF  ( ibreak .gt. 1 )  THEN
	    panl1 = panel ( 1 : ibreak - 1 )
	    panl2 = panel ( ibreak + 1 : )
	  ELSE IF  ( ibreak .eq. 1 )  THEN
	    panl1 = ' '
	    panl2 = panel ( 2 : )
	  ELSE
	    panl1 = panel
	    panl2 = ' '
	END IF
C
C*	First, check for 4 numbers which specify view region exactly.
C
	CALL ST_LCUC  ( panl1, panl1, ier )
	setv = .true.
	CALL ST_RLST  ( panl1, ';', RMISSD, 4, view, num, ier )
	IF  ( ( ier .eq. 0 ) .and. ( num .eq. 4 ) )  THEN
C
C*	    Check for different possible strings as numbers.
C
	  ELSE IF  ( ( ( num .eq. 1 ) .and. ( view (1) .eq. 1. ) ) .or.
     +		       ( panl1 (1:2) .eq. 'UL' ) )  THEN
	    view ( 1 ) = sval ( 1 )
	    view ( 2 ) = sval ( 3 )
	    view ( 3 ) = sval ( 2 )
	    view ( 4 ) = sval ( 4 )
C*
	  ELSE IF  ( ( ( num .eq. 1 ) .and. ( view (1) .eq. 2. ) ) .or.
     +		       ( panl1 (1:2) .eq. 'UR' ) )  THEN
	    view ( 1 ) = sval ( 3 )
	    view ( 2 ) = sval ( 3 )
	    view ( 3 ) = sval ( 4 )
	    view ( 4 ) = sval ( 4 )
C*
	  ELSE IF  ( ( ( num .eq. 1 ) .and. ( view (1) .eq. 3. ) ) .or.
     +		       ( panl1 (1:2) .eq. 'LL' ) )  THEN
	    view ( 1 ) = sval ( 1 )
	    view ( 2 ) = sval ( 1 )
	    view ( 3 ) = sval ( 2 )
	    view ( 4 ) = sval ( 2 )
C*
	  ELSE IF  ( ( ( num .eq. 1 ) .and. ( view (1) .eq. 4. ) ) .or.
     +		       ( panl1 (1:2) .eq. 'LR' ) )  THEN
	    view ( 1 ) = sval ( 3 )
	    view ( 2 ) = sval ( 1 )
	    view ( 3 ) = sval ( 4 )
	    view ( 4 ) = sval ( 2 )
C*
	  ELSE IF  ( ( ( num .eq. 1 ) .and. ( view (1) .eq. 5. ) ) .or.
     +		       ( panl1 (1:1) .eq. 'L' ) )  THEN
	    view ( 1 ) = sval ( 1 )
	    view ( 2 ) = sval ( 1 )
	    view ( 3 ) = sval ( 2 )
	    view ( 4 ) = sval ( 4 )
C*
	  ELSE IF  ( ( ( num .eq. 1 ) .and. ( view (1) .eq. 6. ) ) .or.
     +		       ( panl1 (1:1) .eq. 'R' ) )  THEN
	    view ( 1 ) = sval ( 3 )
	    view ( 2 ) = sval ( 1 )
	    view ( 3 ) = sval ( 4 )
	    view ( 4 ) = sval ( 4 )
C*
	  ELSE IF  ( ( ( num .eq. 1 ) .and. ( view (1) .eq. 7. ) ) .or.
     +		       ( panl1 (1:1) .eq. 'T' ) )  THEN
	    view ( 1 ) = sval ( 1 )
	    view ( 2 ) = sval ( 3 )
	    view ( 3 ) = sval ( 4 )
	    view ( 4 ) = sval ( 4 )
C*
	  ELSE IF  ( ( ( num .eq. 1 ) .and. ( view (1) .eq. 8. ) ) .or.
     +		       ( panl1 (1:1) .eq. 'B' ) )  THEN
	    view ( 1 ) = sval ( 1 )
	    view ( 2 ) = sval ( 1 )
	    view ( 3 ) = sval ( 4 )
	    view ( 4 ) = sval ( 2 )
C*
	  ELSE IF  ( ( ( num .eq. 1 ) .and. ( view (1) .eq. 0. ) ) .or.
     +		       ( panl1 (1:1) .eq. 'A' ) )  THEN
	    view ( 1 ) = sval ( 1 )
	    view ( 2 ) = sval ( 1 )
	    view ( 3 ) = sval ( 4 )
	    view ( 4 ) = sval ( 4 )
C*
	  ELSE IF  ( ( num .eq. 1 ) .and. ( panl1 (1:3) .eq. 'H31' ) )
     +						    THEN
	    view ( 1 ) = 0.0
	    view ( 2 ) = third (5)
	    view ( 3 ) = 1.0
	    view ( 4 ) = third (6)
C*
	  ELSE IF  ( ( num .eq. 1 ) .and. ( panl1 (1:3) .eq. 'H32' ) )
     +						    THEN
	    view ( 1 ) = 0.0
	    view ( 2 ) = third (3)
	    view ( 3 ) = 1.0
	    view ( 4 ) = third (4)
C*
	  ELSE IF  ( ( num .eq. 1 ) .and. ( panl1 (1:3) .eq. 'H33' ) )
     +						    THEN
	    view ( 1 ) = 0.0
	    view ( 2 ) = third (1)
	    view ( 3 ) = 1.0
	    view ( 4 ) = third (2)
C*
	  ELSE IF  ( ( num .eq. 1 ) .and. ( panl1 (1:3) .eq. 'V31' ) )
     +						    THEN
	    view ( 1 ) = third (1)
	    view ( 2 ) = 0.0
	    view ( 3 ) = third (2)
	    view ( 4 ) = 1.0
C*
	  ELSE IF  ( ( num .eq. 1 ) .and. ( panl1 (1:3) .eq. 'V32' ) )
     +						    THEN
	    view ( 1 ) = third (3)
	    view ( 2 ) = 0.0
	    view ( 3 ) = third (4)
	    view ( 4 ) = 1.0
C*
	  ELSE IF  ( ( num .eq. 1 ) .and. ( panl1 (1:3) .eq. 'V33' ) )
     +						    THEN
	    view ( 1 ) = third (5)
	    view ( 2 ) = 0.0
	    view ( 3 ) = third (6)
	    view ( 4 ) = 1.0
	  ELSE IF  ( ( num .eq. 1 ) .and. ( panl1 (1:3) .eq. 'H41' ) )
     +						    THEN
	    view ( 1 ) = 0.0
	    view ( 2 ) = fourth (7)
	    view ( 3 ) = 1.0
	    view ( 4 ) = fourth (8)
C*
	  ELSE IF  ( ( num .eq. 1 ) .and. ( panl1 (1:3) .eq. 'H42' ) )
     +						    THEN
	    view ( 1 ) = 0.0
	    view ( 2 ) = fourth (5)
	    view ( 3 ) = 1.0
	    view ( 4 ) = fourth (6)
C*
	  ELSE IF  ( ( num .eq. 1 ) .and. ( panl1 (1:3) .eq. 'H43' ) )
     +						    THEN
	    view ( 1 ) = 0.0
	    view ( 2 ) = fourth (3)
	    view ( 3 ) = 1.0
	    view ( 4 ) = fourth (4)
	  ELSE IF  ( ( num .eq. 1 ) .and. ( panl1 (1:3) .eq. 'H44' ) )
     +						    THEN
	    view ( 1 ) = 0.0
	    view ( 2 ) = fourth (1)
	    view ( 3 ) = 1.0
	    view ( 4 ) = fourth (2)
C*
	  ELSE IF  ( ( num .eq. 1 ) .and. ( panl1 (1:3) .eq. 'V41' ) )
     +						    THEN
	    view ( 1 ) = fourth (1)
	    view ( 2 ) = 0.0
	    view ( 3 ) = fourth (2)
	    view ( 4 ) = 1.0
C*
	  ELSE IF  ( ( num .eq. 1 ) .and. ( panl1 (1:3) .eq. 'V42' ) )
     +						    THEN
	    view ( 1 ) = fourth (3)
	    view ( 2 ) = 0.0
	    view ( 3 ) = fourth (4)
	    view ( 4 ) = 1.0
C*
	  ELSE IF  ( ( num .eq. 1 ) .and. ( panl1 (1:3) .eq. 'V43' ) )
     +						    THEN
	    view ( 1 ) = fourth (5)
	    view ( 2 ) = 0.0
	    view ( 3 ) = fourth (6)
	    view ( 4 ) = 1.0
C*
	  ELSE IF  ( ( num .eq. 1 ) .and. ( panl1 (1:3) .eq. 'V44' ) )
     +						    THEN
	    view ( 1 ) = fourth (7)
	    view ( 2 ) = 0.0
	    view ( 3 ) = fourth (8)
	    view ( 4 ) = 1.0
C*
	  ELSE IF  ( ( num .eq. 1 ) .and. ( panl1 (1:2) .eq. 'M1' ) )
     +							THEN
	    view ( 1 ) = third (1)
	    view ( 2 ) = third (5)
	    view ( 3 ) = third (2)
	    view ( 4 ) = third (6)
C*
	  ELSE IF  ( ( num .eq. 1 ) .and. ( panl1 (1:2) .eq. 'M2' ) )
     +							THEN
	    view ( 1 ) = third (3)
	    view ( 2 ) = third (5)
	    view ( 3 ) = third (4)
	    view ( 4 ) = third (6)
C*
	  ELSE IF  ( ( num .eq. 1 ) .and. ( panl1 (1:2) .eq. 'M3' ) )
     +							THEN
	    view ( 1 ) = third (5)
	    view ( 2 ) = third (5)
	    view ( 3 ) = third (6)
	    view ( 4 ) = third (6)
C*
	  ELSE IF  ( ( num .eq. 1 ) .and. ( panl1 (1:2) .eq. 'M4' ) )
     +							THEN
	    view ( 1 ) = third (1)
	    view ( 2 ) = third (3)
	    view ( 3 ) = third (2)
	    view ( 4 ) = third (4)
C*
	  ELSE IF  ( ( num .eq. 1 ) .and. ( panl1 (1:2) .eq. 'M5' ) )
     +							THEN
	    view ( 1 ) = third (3)
	    view ( 2 ) = third (3)
	    view ( 3 ) = third (4)
	    view ( 4 ) = third (4)
C*
	  ELSE IF  ( ( num .eq. 1 ) .and. ( panl1 (1:2) .eq. 'M6' ) )
     +							THEN
	    view ( 1 ) = third (5)
	    view ( 2 ) = third (3)
	    view ( 3 ) = third (6)
	    view ( 4 ) = third (4)
C*
	  ELSE IF  ( ( num .eq. 1 ) .and. ( panl1 (1:2) .eq. 'M7' ) )
     +							THEN
	    view ( 1 ) = third (1)
	    view ( 2 ) = third (1)
	    view ( 3 ) = third (2)
	    view ( 4 ) = third (2)
C*
	  ELSE IF  ( ( num .eq. 1 ) .and. ( panl1 (1:2) .eq. 'M8' ) )
     +							THEN
	    view ( 1 ) = third (3)
	    view ( 2 ) = third (1)
	    view ( 3 ) = third (4)
	    view ( 4 ) = third (2)
C*
	  ELSE IF  ( ( num .eq. 1 ) .and. ( panl1 (1:2) .eq. 'M9' ) )
     +							THEN
	    view ( 1 ) = third (5)
	    view ( 2 ) = third (1)
	    view ( 3 ) = third (6)
	    view ( 4 ) = third (2)
C
C*	    Return without changing view region if panel is not
C*	    recognized.
C
	  ELSE
	    setv = .false.
	    iret = -10
	END IF
C
C*	Set view region.
C
	IF  ( setv )  THEN
	    CALL GSVIEW  ( view (1), view (2), view (3), view (4), ier )
	    IF  ( ier .ne. 0 )  iret = -11
	END IF
C
C*	Now break box into 3 integers for color, line type and width.
C
	CALL ST_CLST  ( panl2, '/', ' ', 3, line, num, ier )
C
	IF  ( line (1) .ne. ' ' )  THEN
	    CALL IN_COLR ( line (1), 1, icolr, ier )
	  ELSE
	    icolr = 0
	END IF
	CALL ST_NUMB ( line (2), iltyp, ier )
	CALL ST_NUMB ( line (3), ilwid, ier )
C
C*	Set region to draw box around.
C
	CALL ST_NOCC  ( panl2, '/', 3, ipoint, ier )
	CALL ST_LSTR  ( panl2, ilen, ier1 )
	IF ( ipoint .eq. ilen ) ier = -1
C
	IF ( ( ier .eq. 0 ) .and. ( icolr .gt. 0 ) )  THEN
	    region = panl2 ( ipoint+1 : ipoint+1 )
	    CALL GG_BOX  ( region, icolr, iltyp, ilwid, iret )
	ELSE IF ( icolr .gt. 0 )  THEN
	    CALL GG_BOX  ( 'V', icolr, iltyp, ilwid, iret )
	END IF
C*
	RETURN
	END
