	SUBROUTINE GAAXIS ( iaxis, axpos, laxis, lbstfq, mtstfq, lgstfq,
     +			    np, axary, chary, iret )
C************************************************************************
C* GAAXIS								*
C* 									*
C* This subroutine draws a graph axis with labels, tick marks, and	*
C* grid lines.  The graph coordinates must be defined by a call to	*
C* GSGRAF before this subroutine is called.				*
C*									*
C* Line attributes apply to grid lines.  The axis line is a solid       *
C* line.  Tick mark attributes can be set in GSTICK.		        *
C*									*
C* The label string (CHARY) and position (AXARY) arrays are allowed a 	*
C* maximum of 530 elements.  Each CHARY element contains up to 24	*
C* characters.								*
C* 									*
C* LBSTFQ, MTSTFQ and LGSTFQ are specified in the form SSFF where	*
C* SS is the position of the first item and FF is the frequency with	*
C* which to plot items in the AXARY positions.  For example,		*
C* LBSTFQ = 205 plots every fifth CHARY item starting with the second,	*
C* CHARY (2) at the position AXARY (2), CHARY (7) at AXARY (7), ...	*
C* 									*
C* In a polar coordinate system, the axis is not drawn.  When		*
C* IAXIS is 1 or 3, grid lines are circles with the radii specified	*
C* in AXARY.  When IAXIS is 2 or 4, AXARY specifies the angle for	*
C* radial lines which are drawn from the center of the circle to	*
C* AXPOS.  Tick marks are not drawn in polar coordinates.		*
C*									*
C* Except for CHARY, GAAXIS is identical to GDAXIS.			*
C*									*
C* GAAXIS ( IAXIS, AXPOS, LAXIS, LBSTFQ, MTSTFQ, LGSTFQ, NP,		*
C*	    AXARY, CHARY, IRET )					*
C* 									*
C* Input parameters:							*
C* 	IAXIS		INTEGER 	Axis				*
C*				 	  1 = x axis labelled below	*
C*					  2 = y axis labelled left	*
C*					  3 = x axis labelled above	*
C*					  4 = y axis labelled right	*
C* 	AXPOS		REAL		Intersection with other axis	*
C* 	LAXIS		INTEGER		Axis draw flag			*
C* 	LBSTFQ		INTEGER		Start/frequency of labels	*
C*	MTSTFQ		INTEGER		Start/frequency of tick marks	*
C* 	LGSTFQ		INTEGER		Start/frequency of grid lines	*
C* 	NP		INTEGER		Number of values in AXARY	*
C* 	AXARY (NP)	REAL		Locations on axis		*
C*	CHARY (NP)	CHAR*		Label strings			*
C* 									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 						*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* G. Huffman/GSC	 1/89	Documentation				*
C* M. desJardins/GSFC	 7/89	Changed maximum number of points	*
C* L. Williams/EAi	 3/94	Removed blank comments from header	*
C* M. Linda/GSC		 3/96	Added check for GPLT buffer overflow	*
C* T. Piper/GSC		 5/98	Removed NDEC parameter from prolog      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
C*
	REAL 		axary (*)
	CHARACTER*(*) 	chary (*)
C
	INTEGER 	isend (3), jsend (5), icsend (6)
	CHARACTER	string*24
C------------------------------------------------------------------------
C
C*	Check if GPLT buffer will overflow.
C
	IF ( np .gt. 530 ) THEN
	   iret = NLBLEX
	   RETURN
	END IF
C
	isiz   = LEN ( chary (1) )
	nwpchr = ( ( isiz - 1 ) / 4 ) + 1
	IF ( nwpchr .gt. 6 ) nwpchr = 6
C
	isnd = 2 + ( 8 + np + ( np * nwpchr ) )
	ircv = 1 + ( 1 )
	IF ( ( isnd + ircv ) .gt. IGBSIZ ) THEN
	    iret = NOBUFF
	    RETURN
	END IF
C
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = isnd
	isend (2) = FAAXIS
	isend (3) = iaxis
C
	jsend (1) = laxis
	jsend (2) = lbstfq
	jsend (3) = mtstfq
	jsend (4) = lgstfq
	jsend (5) = np
C
	CALL GPUT ( isend, 3, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUTR ( axpos, 1, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUT ( jsend, 5, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUTR ( axary, np, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUT ( nwpchr, 1, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Send the character array one element at a time.
C
	DO i = 1, np
	    string = chary (i)
	    CALL ST_STOI ( string, isiz, nv, icsend, ier )
	    CALL GPUT ( icsend, nwpchr, iret )
	    IF ( iret .ne. NORMAL ) RETURN
	END DO
C
C*	Get output parameters.
C
	CALL GGET ( iret, 1, ier )
	IF ( ier .ne. NORMAL ) iret = ier
C*
	RETURN
	END
