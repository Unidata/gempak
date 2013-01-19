	SUBROUTINE GG_DLTN  ( lincol, lintyp, linwid, ifreq, dlltln, 
     +			      aloc, ifrmat, iret )
C************************************************************************
C* GG_DLTN								*
C*									*
C* This subroutine draws latitude/longitude lines on the graphics	*
C* device.  								*
C*									*
C* GG_DLTN ( LINCOL, LINTYP, LINWID, IFREQ, DLLTLN, ALOC, IFRMAT, IRET )*
C*									*
C* Input parameters:							*
C*	LINCOL		INTEGER		Line color			*
C*	LINTYP		INTEGER		Line type			*
C*	LINWID		INTEGER		Line width			*
C*	IFREQ(2)	INTEGER		Frequency of labels		*
C*	DLLTLN(2)	REAL		Frequency of lat/lon lines	*
C*	ALOC(2)		REAL		Lat/lon label locations		*
C*	IFRMAT		INTEGER		Lat/lon format			*
C*					  1 = +/- value			*
C*					  2 = N,S,E,W addded, no '-'    *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				  	  0 = normal return		*
C*				 	-13 = lines not drawn		*
C**									*
C* Log:									*
C* S. Maxwell/GSC	 1/97	Taken from GG_LTLN			*
C* T. Lee/GSC		 9/97	Fixed label freq in graphic mode	*
C* A. Hardy/GSC		12/00   Added lat/lon label locations, format	*
C* T. Piper/GSC		 5/01	Added GSSMTH				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		ifreq (*)
	REAL		dlltln(*), aloc(*)
C*
	REAL		xlb (LLAXIS), ylb (LLAXIS)
	LOGICAL		polar
C-----------------------------------------------------------------------
C*	Save current line and smooth information.
C
	CALL GQLINE  ( ilto, ilhwo, iwdo, iwdhwo, ier )
	CALL GQSMTH  ( ismtyp, dens, ier )
	CALL GSLINE  ( lintyp, 0, linwid, 0, ier )
	CALL GSSMTH  ( 0, 0., ier )
C
C*      Get mode to see if this is a graph or map.
C
	CALL GQMODE  ( mode, ier )
C
C*	Set grid line color and draw lines.
C
	CALL GSCOLR  ( lincol, ier )
	IF ( mode .eq. 1 )  THEN
	    CALL GDRGRD  ( dlltln(1), dlltln(2), ifreq, aloc(1), 
     +                     aloc(2), ifrmat, ier )
	    IF  ( ier .ne. 0 )  THEN	
		CALL ER_WMSG  ( 'GEMPLT', ier, ' ', iret )
		iret = -13
		CALL ER_WMSG  ( 'GG', iret, ' ', ier )
	    END IF
	ELSE IF  ( mode .eq. 2 )  THEN
	    CALL GQGRAF  ( ixtyp, iytyp, ysiz, xlm, ylm, xrm, ytm,
     +                     ier )
C
C*	    Check for special case of polar plots.
C
	    IF  ( ixtyp .eq. 5 )  THEN
		polar = .true.
		IF  ( ( xlm .eq. xrm ) .and.
     +                 ( NINT ( ylm ) .eq. 225 ) .and.
     +                 ( NINT ( ytm ) .eq. 45 ) )  THEN
		    xrm = xlm / SQRT ( 2. )
		    xlm = 0.
		    ylm = 0.
		    ytm = 360.
		END IF
	    ELSE
		polar = .false.
	    END IF
C
C*          Get number of points for labels.
C
	    nx = ABS ( (xrm - xlm) / dlltln (1) ) + 1
	    ny = ABS ( (ytm - ylm) / dlltln (2) ) + 1
	    xlb (1) = xlm
	    ylb (1) = ylm
C
C*	    Get multiplier and label values.
C
	    IF  ( xlm .gt. xrm )  THEN
		ixmul = -1
	    ELSE
		ixmul = 1
	    END IF
	    IF  ( ylm .gt. ytm )  THEN
		iymul = -1
	    ELSE
		iymul = 1
	    END IF
C*
	    DO  i = 2, nx
		xlb ( i ) = xlb ( i-1 ) + ( dlltln (1) * ixmul )
	    END DO
	    DO  i = 2, ny
		ylb ( i ) = ylb ( i-1 ) + ( dlltln (2) * iymul )
	    END DO
C
C*	    Draw the x axis.
C
	    CALL GDAXIS  ( 1, ylm, .true., ifreq (1), ifreq (1),
     +                     ifreq (1), 0, nx, xlb, ier )
	    IF  ( ier .ne. 0 ) CALL ER_WMSG ( 'GEMPLT', ier, ' ', ier1 )
C
C*	    Draw the y axis.
C
	    IF  ( polar )  xlm = xrm
	    CALL GDAXIS  ( 2, xlm, .true., ifreq (2), ifreq (2),
     +                     ifreq (2), 0, ny, ylb, ier )
	    IF  ( ier .ne. 0 ) CALL ER_WMSG ( 'GEMPLT', ier, ' ', ier1 )
	ELSE
	    CALL ER_WMSG  ( 'GEMPLT', -2, 'invalid mode', ier )
	END IF
C
C*	Restore current line and smooth type.
C
	CALL GSLINE  ( ilto, 0, iwdo, 0, ier )
	CALL GSSMTH  ( ismtyp, dens, ier )
C*
	RETURN
	END
