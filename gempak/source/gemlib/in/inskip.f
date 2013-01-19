	SUBROUTINE IN_SKIP  ( skip, iskpcn, iskplt, iret )
C************************************************************************
C* IN_SKIP								*
C*									*
C* This subroutine decodes the skip factor which is in the form:	*
C*		skip_contour / skip_plot_x; skip_plot_y			*
C*									*
C* IN_SKIP  ( SKIP, ISKPCN, ISKPLT, IRET )				*
C*									*
C* Input parameters:							*
C*	SKIP		CHAR*		Skip factor input		*
C*									*
C* Output parameters:							*
C*	ISKPCN		INTEGER		Skip for contours		*
C*	ISKPLT (2)	INTEGER		Skip for plotting at x, y locs	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/NMC	12/91						*
C* L. Sager/NMC		 6/93		Stagger wind plots		*
C* S. Jacobs/NMC	 8/94		Cleaned up			*
C* S. Jacobs/NMC	 8/94		Force the user to type a / to	*
C*					   get the plot skip values	*
C* S. Jacobs/NMC	 9/94		Force neg values to be odd	*
C* S. Jacobs/NMC	 9/94		Default the Y-skip to 1/2 the	*
C*					   X-skip for staggering,	*
C*					   otherwise use the requested	*
C*					   value			*
C* D.W.Plummer/NCEP	10/96		Allow iskplt(2) to be negative.	*
C* T. Piper/GSC		11/98		Updated prolog			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	skip
	INTEGER		iskplt (2)
C*
	CHARACTER	carr (2)*72
	INTEGER		iarr (2)
C------------------------------------------------------------------------
	iret = 0
C
C*	Break input into data and vector elements.
C
	CALL ST_CLST  ( skip, '/', ' ', 2, carr, num, ier )
C
C*	Get the contour skip value from the first string.
C
	CALL ST_ILST ( carr (1), ';', 0, 1, iskpcn, num, ier )
	IF  ( iskpcn .lt. 0 )  iskpcn = 0
C
C*	Parse the second string. Get the values for the x and y point
C*	increments.
C
	CALL ST_ILST ( carr (2), ';', IMISSD, 2, iarr, num, ier )
C
C*	Check for a missing x value.
C*	Also check for negative values which signal staggering.
C
	IF  ( iarr (1) .eq. IMISSD )  THEN
	    iskplt (1) = 0
	ELSE IF  ( iarr (1) .lt. 0 )  THEN
	    ival = ABS ( iarr (1) )
	    IF  ( MOD ( ival, 2 ) .eq. 0 )  THEN
		iskplt (1) = iarr (1) + 1
	    ELSE
		iskplt (1) = iarr (1)
	    END IF
	ELSE
	    iskplt (1) = iarr (1)
	END IF
C
C*	Check for a missing y value.
C
	IF  ( iarr (2) .eq. IMISSD )  THEN
	    IF  ( iskplt (1) .lt. 0 )  THEN
		iskplt (2) = ABS ( iskplt (1) ) / 2
	    ELSE
		iskplt (2) = iskplt (1)
	    END IF
	ELSE
	    iskplt (2) = iarr (2)
	END IF
C*
	RETURN
	END
