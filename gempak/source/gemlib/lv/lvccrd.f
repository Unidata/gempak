	SUBROUTINE LV_CCRD  ( ivcord, vcoord, iret )
C************************************************************************
C* LV_CCRD								*
C*									*
C* This subroutine translates a numeric value for IVCORD into its 	*
C* character value in VCOORD.						*
C*									*
C* LV_CCRD  ( IVCORD, VCOORD, IRET )					*
C*									*
C* Input parameters:							*
C*	IVCORD		INTEGER		Numeric vertical coordinate	*
C*									*
C* Output parameters:							*
C*	VCOORD		CHAR*		Vertical coordinate 		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -3 = invalid coordinate	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/89	From LV_CORD				*
C* K. Brill/NMC		 4/91	Added SGMA and DPTH			*
C* M. desJardins/NMC	10/91	Generalize vertical coordinate		*
C* F. J. Yen/NCEP 	 7/08	Added HYBL				*
C************************************************************************
	CHARACTER*(*)	 vcoord
C*
	CHARACTER	v*1
C*
	PARAMETER	( NVERT= 7 )
	CHARACTER	vert (NVERT)*4
C*
	DATA 		vert  / 'NONE', 'PRES', 'THTA', 'HGHT',
     +				'SGMA', 'DPTH', 'HYBL' /
C------------------------------------------------------------------------
	iret  = 0
C
C*	Translate known vertical coordinates or look for parameter name.
C
	ier = 0
C
C*	Check for numeric vertical coordinates.
C
	IF  ( ( ivcord .ge. 0 ) .and. ( ivcord .le. NVERT - 1 ) )  THEN
	    vcoord = vert ( ivcord + 1 )
C
C*	    Check for character name as vertical coordinate.  Check that
C*	    each character is an alphanumeric character.
C
	  ELSE IF  ( ivcord .gt. 100 )  THEN
	    CALL ST_ITOC  ( ivcord, 1, vcoord, ier )
	    IF  ( ier .eq. 0 )  THEN
		DO  i = 1, 4
		    v = vcoord (i:i)
		    IF  ( ( ( v .lt. 'A' ) .or. ( v .gt. 'Z' ) ) .and.
     +			  ( ( v .lt. '0' ) .or. ( v .gt. '9' ) ) )  THEN
			ier = -1
		    END IF
		END DO
	    END IF
	  ELSE
	    ier = -1
	END IF
C
C*	Check for errors.
C
	IF  ( ier .ne. 0 )  THEN
	    vcoord = ' '
	    iret   = -3
	END IF
C*
	RETURN
	END
