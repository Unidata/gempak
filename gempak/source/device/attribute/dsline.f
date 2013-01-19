	SUBROUTINE DSLINE  ( iltyp, ilthw, iwidth, ilwhw, 
     +			     jltyp, jlthw, jwidth, jlwhw, iret )
C************************************************************************
C* DSLINE								*
C* 									*
C* This subroutine sets the line attributes including the line type 	*
C* number, the software/hardware line type flag, the line width size	*
C* multiplier and the software/hardware line width flag.  		*
C*									*
C* DSLINE  ( ILTYP, ILTHW, IWIDTH, ILWHW, JLTYP, JLTHW, JWIDTH, JLWHW,	*
C*           IRET )							*
C*									*
C* Input parameters:							*
C*	ILTYP		INTEGER		Line type			*
C*					  <=0 = no change		*
C*	ILTHW		INTEGER		Sw/hw line type flag		*
C*					  1 = software line type	*
C*					  2 = hardware line type	*
C*					  otherwise no change		*
C*	IWIDTH		INTEGER		Line width size multiplier	*
C*					  <=0 = no change		*
C*	ILWHW		INTEGER		Sw/hw line width flag		*
C*					  1 = software line width	*
C*					  2 = hardware line width	*
C*					  otherwise no change		*
C*									*
C* Output parameters:							*
C*	JLTYP		INTEGER		Line type			*
C*	JLTHW		INTEGER		Sw/hw line type flag		*
C*	JWIDTH		INTEGER		Line width size multiplier	*
C*	JLWHW		INTEGER		Sw/hw line width flag		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/86	Corrected error in line width hw def	*
C* M. desJardins/GSFC	 5/89	Make size = 0 no change			*
C* M. desJardins/GSFC	 6/89	Modified to retain dashing pattern	*
C* S. Schotz		 2/90	Modified for dash pattern enhancements	*
C* S. Jacobs/NCEP	 3/97	Added check for VG driver		*
C* T. Piper/SAIC	11/01	Initialized new to false		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DVWNDW.CMN'
C*
	LOGICAL		new
C------------------------------------------------------------------------
	iret   = NORMAL
	new    = .false.
	jltyp  = mltyp
	jlthw  = mlthw
	jwidth = mlwid
	jlwhw  = mlwhw
C
C*	Set line characteristics.  Check for valid line type first.
C
	IF  ( iltyp .gt. 0 )  THEN
	    IF  ( iltyp .ne. mltyp )  new = .true.
	    jltyp = iltyp
	END IF
C
C*	Check hardware line type.
C
	IF  ( ilthw .eq. 1 )  THEN
	    jlthw = 1
C
C*	    In changing from hardware to software width, reset line 
C*	    width.
C
	    IF  ( mlthw .eq. 2 )  THEN
		CALL HSLTYP  ( 1, iret )
		new = .true.
	    END IF
	  ELSE IF  ( ilthw .eq. 2 )  THEN
	    IF  ( nlthw .eq. 1 )  THEN
	        jlthw = 2
	      ELSE
	        jlthw = 1
	    END IF
	END IF
C
C*	Check line width and line width hardware flag.
C
	IF  ( iwidth .gt. 0 )  jwidth = iwidth
	IF  ( ilwhw .eq. 1 )  THEN
	    jlwhw = 1
	    IF  ( mlwhw .eq. 2 )  CALL HSLWID ( 1, iret )
	  ELSE IF  ( ilwhw .eq. 2 )  THEN
	    IF  ( nwdhw .eq. 1 )  THEN
	        jlwhw = 2
	      ELSE
	        jlwhw = 1
	    END IF
	END IF
C
C*	Check for software lines.
C
	IF  ( jlthw .eq. 1 )  THEN
C
C*	    For software line type set active pattern.
C*	    Also, find last non-zero segment.
C
	    IF  ( jltyp .gt. 99 )  jltyp = 1
	    IF  ( new )  THEN
C
C*              Get pattern number ( ones place)
C
	        jpatno = mod ( jltyp, 10 ) + 1
C
C*              Get pattern size factor ( ones = 1, 10's = 0.5, 
C*              20's = 1, 30's = 2, 40's = 3, ... )
C
	        psfac = int ( jltyp / 10 ) - 1
	        IF ( psfac .eq. 0 ) psfac = 0.5
		IF ( psfac .lt. 0 ) psfac = 1.0
		DO  i = 1, 8
                    actpat (i) = lpat ( i, jpatno ) * psfac * lpscal
		    IF  ( actpat (i) .ne. 0. )  nseg = i
		END DO
		rmlen = actpat ( 1 )
	        IF ( rmlen .gt. 0 ) THEN
	             dtflg = .false.
		     lnflg = .true.
	        ELSE
C
C*                   Dot pattern
C
	             dtflg = .true.
                     lnflg = .false.
	             rmlen = (-rmlen) * 0.5
	        END IF
		mseg  = 1
	    END IF
C*
	  ELSE IF ( (jlthw .ne. mlthw) .or. (jltyp .ne. mltyp) ) THEN
	    CALL HSLTYP ( jltyp, iret )
	END IF
C
C*	Check for hardware line width characteristics.
C
	IF  ( ( jlwhw  .eq. 2 )  .and. 
     + 	      ( (jwidth .ne. mlwid) .or. (jlwhw .ne. mlwhw)) ) THEN
	    CALL HSLWID  ( jwidth, iret )
C
C*	    Check for change from hardware to software lines.
C
	  ELSE IF ( (jlwhw .eq. 1 ) .and. (mlwhw .eq. 2) ) THEN
	    CALL HSLWID  ( 1, iret )
	END IF
C
C*	Save characteristics in /DEVACT/.
C
	mltyp = jltyp
	mlthw = jlthw
	mlwid = jwidth
	mlwhw = jlwhw
C
C*	If the driver is VG, send the attributes to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HSLINE ( mltyp, mlthw, mlwid, mlwhw, iret )
	END IF
C*
	RETURN
	END
