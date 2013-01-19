	SUBROUTINE LV_CORD  ( vcoord, vparm, ivert, iret )
C************************************************************************
C* LV_CORD								*
C*									*
C* This subroutine converts the input for VCOORD to upper-case and	*
C* translates it to a numeric value.					*
C*									*
C* LV_CORD  ( VCOORD, VPARM, IVERT, IRET )				*
C*									*
C* Input parameters:							*
C*	VCOORD		CHAR*		Vertical coordinate input	*
C*									*
C* Output parameters:							*
C*	VPARM		CHAR*		Upper-case coordinate 		*
C*	IVERT		INTEGER		Numeric vertical coordinate	*
C*					  0 = NONE			*
C*					  1 = PRES			*
C*					  2 = THTA			*
C*					  3 = HGHT			*
C*					  4 = SGMA			*
C*					  5 = DPTH			*
C*					  6 = HYBL			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -3 = invalid coordinate	*
C**									*
C* Log:									*
C* M. Goodman/RDS	11/84	Original source				*
C* M. desJardins/GSFC	 5/86	Changed from IP_VCORD to LV_CORD	*
C* M. desJardins/GSFC	 7/88	Documentation				*
C* S. Schotz/GSC	 8/90	Check for abbreviation of coordinate	*
C* K. Brill/NMC		 4/91	Added SGMA and DPTH			*
C* M. desJardins/NMC	10/91	Generalized vertical coordinate		*
C* M. desJardins/NMC	 4/93	Improve efficiency			*
C* K. Brill/NMC		 5/93	Use VP and remove CALL ST_LCUC		*
C* F. J. Yen/NCEP	 7/08	Added HYBL				*
C************************************************************************
	CHARACTER*(*)	 vcoord, vparm
C*
	LOGICAL		abrflg, check
	CHARACTER	v*1, vp*4
C*
	PARAMETER	( NVERT= 7 )
	CHARACTER	vert (NVERT)*4
C*
	DATA 		vert  / 'NONE', 'PRES', 'THTA', 'HGHT',
     +				'SGMA', 'DPTH', 'HYBL'/
C------------------------------------------------------------------------
	vp = vcoord
	iret  = 0
	ivert = -1
	vparm = ' '
C
C*	Check for standard vertical coordinates.
C
	v = vp ( 4:4 )
	IF  ( v .eq. ' ' )  THEN
	    check = .true.
	  ELSE
	    check = .false.
	END IF
	i = 1
	DO WHILE  ( ( ivert .eq. -1 ) .and. ( i .le. NVERT ) )
	    IF  ( check )  THEN
		CALL ST_ABBR ( vert (i), vp, abrflg, ier )
	      ELSE
		IF  ( vp .eq. vert (i) )  THEN
		    abrflg = .true.
		  ELSE
		    abrflg = .false.
		END IF
	    END IF
	    IF  ( abrflg )  THEN
		ivert = i - 1
		vparm = vert (i)
	    END IF
	    i = i + 1
	END DO
C
C*	Check for valid alphanumeric parameter name.
C
	IF  ( ivert .lt. 0 )  THEN
	    ivert = 0
	    DO  i = 1, 4
		v = vp (i:i)
		IF  ( ( ( v .lt. 'A' ) .or. ( v .gt. 'Z' ) ) .and.
     +		      ( ( v .lt. '0' ) .or. ( v .gt. '9' ) ) )  THEN
		    ivert = -1
		END IF
	    END DO
	    IF  ( ivert .eq. 0 )  THEN
		CALL ST_CTOI  ( vp, 1, ivert, ier )
		IF  ( ier .ne. 0 )  ivert = -1
	    END IF
	END IF
C
C*	Return error message if coordinate is not recognized.
C
	IF  ( ivert .lt. 0 ) iret = -3
C*
	RETURN
	END
