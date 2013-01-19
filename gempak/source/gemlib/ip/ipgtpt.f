	SUBROUTINE IP_GTPT  ( pvar, iret )
C************************************************************************
C* IP_GTPT								*
C*									*
C* This subroutine gets the points from the button presses and sets	*
C* the requested variable.						*
C*									*
C* IP_GTPT  ( PVAR, IRET )						*
C*									*
C* Input parameters:							*
C*	PVAR		CHAR*		Input parameter name		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				   	  0 = normal return 		*
C*					-11 = Invalid parameter name	*
C*					-41 = Invalid cursor points	*
C**									*
C* Log:									*
C* S. Jacobs/EAI	 6/93						*
C* S. Jacobs/EAI	 9/93		Added ITYP to call for GGTPNT	*
C* S. Jacobs/NMC	 6/94		Changed values for parms to	*
C*					one decimal place		*
C* K. Tyle/GSC		 7/96		Renamed from NT_GTPT		*
C* K. Tyle/GSC		 7/96		Added check for mode		*
C* S. Maxwell/GSC        1/97           Added ST_LSTF, ST_LSTC, and     *
C*                                      GG_ZARE                         *
C* S. Jacobs/NCEP	 1/97		Fixed check for SAT projection;	*
C*					Removed unused variables	*
C* D.W.Plummer/NCEP	 6/97	Increased string lengths from 72 to 128	*
C* S. Jacobs/NCEP	 6/98		Removed NP from call to GGTPNT	*
C* T. Lee/GSC		 1/01		Added extended display area	*
C* M. Li/SAIC		12/07		Add CGAREA and CGAREAX		*
C************************************************************************
	INCLUDE		'ipcmn.cmn'
C*
	CHARACTER*(*)	pvar
C*
	REAL		x(2), y(2), rarr(2)
	CHARACTER	value*128, wname*128, garea*128
	CHARACTER	proj*128, str(2)*128
C-----------------------------------------------------------------------
	iret = 0
	CALL ST_LCUC ( pvar, pvar, ier )
C
C*	Get the integer from the input.
C
	IF  ( pvar .eq. 'GPOINT' )  THEN
	    ityp = 1
	ELSE IF  ( pvar .eq. 'CXSTNS' )  THEN
	    ityp = 2
	ELSE IF  ( pvar .eq. 'GAREA' )  THEN
	    ityp = 3
	ELSE IF  ( pvar .eq. 'GAREAX' )  THEN
	    ityp = 13
	    pvar = 'GAREA'
	ELSE IF  ( pvar .eq. 'CGAREA' )  THEN
            ityp = 4 
            pvar = 'GAREA'
	ELSE IF  ( pvar .eq. 'CGAREAX' )  THEN
            ityp = 14
            pvar = 'GAREA'
	ELSE
	    iret = -11
	    CALL ER_WMSG ( 'IP', iret, ' ', ier )
	    RETURN
	END IF
C
C*	Select and pop the current window.
C
	wname = ' '
	CALL GSLWIN ( wname, ier )
	CALL GEPLOT ( ier )
C
C*	Get the cursor point(s).
C
	CALL GGTPNT ( 'M', ityp, x, y, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Set the value of the parameter.
C
	IF  ( pvar .eq. 'GPOINT' )  THEN
       	    rarr(1) = x(1)
            rarr(2) = y(1)
            CALL ST_LSTF ( rarr, 2, ';', 1, value, iret )
	  ELSE IF  ( pvar .eq. 'CXSTNS' )  THEN
            rarr(1) = x(1)
            rarr(2) = y(1)
            CALL ST_LSTF ( rarr, 2, ';', 1, str(1), iret )
            rarr(1) = x(2)
            rarr(2) = y(2)
            CALL ST_LSTF ( rarr, 2, ';', 1, str(2), iret )
            CALL ST_LSTC ( str, 2, '>', value, iret )
  	  ELSE IF  ( pvar .eq. 'GAREA' )  THEN
C
C*	    Check for map mode.
C
	    CALL GQMODE ( mode, iret )
	    IF ( mode .eq. 1 ) THEN 
C
C*		Get new garea and proj.
C
		CALL GG_ZARE ( x, y, garea, proj, iret )
		value = garea
C
C*	   	Check value of PROJ.
C
	    	DO i = 1, ncparm
		    IF ( cparmn (i) .eq. 'PROJ' ) ilist = i 
	    	END DO	
C
	    	CALL ST_LCUC ( cparmv (ilist), cparmv (ilist), ier)
C
C*		Redefine value of PROJ, if it is not currently
C*		SAT or RAD.
C
	    	IF  ( ( cparmv (ilist) .ne. 'SAT' ) .and.
     +		      ( cparmv (ilist) .ne. 'RAD') ) THEN
		    cparmv (ilist) = proj 
	    	END IF
	    END IF
	END IF
C
C*	Check for parameter name in full list.
C
	DO  i = 1, ncparm
	    IF  ( pvar .eq. cparmn (i) )  ifound = i
	END DO
	cparmv ( ifound ) = value
C
C*	Flush and pop the current window.
C
	CALL GEPLOT ( ier )
C*
	RETURN
	END
