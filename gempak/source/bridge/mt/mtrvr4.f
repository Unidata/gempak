	SUBROUTINE MT_RVR4 ( strrvr, irvr, idecd, iret )
C************************************************************************
C* MT_RVR4                                                              *
C*                                                                      *
C* This subroutine will decode up to four runway visual range groups.	*
C* The values are stored in common.					*
C*									*
C* MT_RVR4 ( STRRVR, IRVR, IDECD, IRET )                                *
C*								        *
C* Input parameters: 						        *
C*      STRRVR		CHAR*		Runway visual range field	*
C*	IRVR		INTEGER		Relative number of RVR group    *
C*								        *
C* Output parameters:						        *
C*	CIVALS(ICRWID)	CHAR*		Runway identifier               *
C*	RIVALS(IRV1RI)  REAL		First runway visual range (m)   *
C*	RIVALS(IRV2RI)  REAL		Second runway visual range (m)  *
C*	RIVALS(IRV1RF)  REAL		First runway visual range flag  *
C*	RIVALS(IRV2RF)  REAL		Second runway visual range flag *
C*	RIVALS(IRRWYT)  REAL		Runway visual range tendency    *
C*	RIVALS(IRNRWY)  REAL		Number of runway groups         *
C*	IDECD		INTEGER		Decode decision flag            *
C*					  1 = field decoded		*
C*					  0 = field not decoded    	*
C*	IRET		INTEGER		Return code                     *
C*	      		    		  0 = normal return       	*
C*	                                 10 = miscoded field	 	*
C*					 -1 = field not found		*
C*					                                *
C**								        *
C* Log:									*
C* D. Kidwell/NCEP 	 4/95                                           *
C* D. Kidwell/NCEP    	 3/96   Modified to handle Canadian format      *
C* D. Kidwell/NCEP    	 9/96   Changed tendency vals per new WMO table *
C* K. Tyle/GSC		 2/97	Changed error processing		*
C* D. Kidwell/NCEP    	 6/97   Replaced ST_LSTR with INDEX             *
C* D. Kidwell/NCEP    	 6/97   Added check for string of length .ge. 40*
C* D. Kidwell/NCEP    	 4/98   New interface                           *
C* D. Kidwell/NCEP    	 1/99   Fixed bug for bad substring reference   *
C* D. Kidwell/NCEP    	 9/02   Replaced MT_VALD with BR_VALD           *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'mtcmn.cmn'
C*
	CHARACTER*(*)	strrvr
C------------------------------------------------------------------------
	iret   = 0
	idecd  = 0
	irvrun = IMISSD
C
	CALL BR_VALD ( strrvr, 1, 1, 'R', 0, iloc, iret)
	IF ( iret .eq. 0 ) THEN
	    IF ( INDEX ( strrvr, '/' ) .eq. 0 ) iret = -1
	END IF
	IF ( iret .eq. 0 ) THEN
C
C*	    Found keys 'R' and '/'.
C* 	    Decode runway visual range group.
C
	    kret  = 0
	    idecd = 1
C
	    lens = INDEX ( strrvr, ' ' )  - 1
	    IF ( lens .lt. 0 ) lens = LEN ( strrvr )
	    IF ( strrvr ( lens - 1:lens ) .eq. 'FT' ) THEN
C
C*	    Have U.S. or Canadian practice - 'FT'.
C
		irvrun = 0     
		iloc   = lens - 2
              ELSE IF ( ( contry .eq. 'CN  ' ) .and.
     +                ( strrvr ( lens-3:lens-1 ) .eq. 'FT/' ) ) THEN
C
C*              Have Canadian practice 'FT/i'.
C
                irvrun = 0
                iloc   = lens - 4
              ELSE
C
C*              Not U.S. or Canadian.
C
                irvrun = 1
                iloc   = lens - 1
            END IF
C
            IF ( iloc .ne. ( lens - 2 ) ) THEN
C
C*		Check for tendency.
C
		IF ( strrvr ( lens:lens ) .eq. 'U' ) THEN
		    rivals ( irrwyt ( irvr ) ) = 0.
		  ELSE IF ( strrvr ( lens:lens ) .eq. 'D' ) THEN
		    rivals ( irrwyt ( irvr ) ) = 1.
		  ELSE IF ( strrvr ( lens:lens ) .eq. 'N' ) THEN
		    rivals ( irrwyt ( irvr ) ) = 2.
		  ELSE
		    iloc = lens
		END IF
	    END IF
C
C*	    Get runway number.
C
	    ilocsl = INDEX ( strrvr, '/' )
	    IF ( ( ilocsl .lt. 4 ) .or. ( ilocsl .gt. 6 ) ) THEN
	        kret = 1
	      ELSE
	        civals ( icrwid ( irvr ) ) = strrvr ( 2:ilocsl-1 )
	    END IF
C
C*          Check to see if we have a range of visibilities (2).
C
	    ilocv = INDEX ( strrvr, 'V' )
	    IF ( ( ilocv .eq. 0 ) .or. ( ilocv .lt. ilocsl ) ) THEN
	        iloce = iloc
		ilocv = 0
	      ELSE
	        iloce = ilocv - 1
	    END IF
	    ilocb = ilocsl + 1
	    i     = 1
C
C*	    Decode visiblitie(s).
C
	    DO WHILE ( i .lt. 3 )
C
		IF ( i .eq. 1 ) THEN
		    iri = irv1ri ( irvr )
		    irf = irv1rf ( irvr )
		  ELSE
		    iri = irv2ri ( irvr )
		    irf = irv2rf ( irvr )
		END IF
C		    
C*		Look for 'P' or 'M' and set flag if found.
C
	        IF ( strrvr ( ilocb:ilocb ) .eq. 'M' ) THEN
	            rivals ( irf ) = 0.
	            ilocb = ilocb + 1
	          ELSE IF ( strrvr ( ilocb:ilocb ) .eq. 'P' ) THEN
	            rivals ( irf ) = 4.
	            ilocb = ilocb + 1
		  ELSE
		    rivals ( irf ) = 2.
	        END IF
C
C*		Get runway visual range value.
C
		IF ( iloce .ge. ilocb ) THEN
	            CALL ST_INTG ( strrvr (ilocb:iloce), irange, jret )
		    IF ( jret .eq. 0 ) THEN
		        rivals ( iri ) = FLOAT ( irange )
		        IF ( irvrun .eq. 0 )
     +			     rivals ( iri ) = PR_HGFM ( rivals ( iri ) )
		      ELSE
			kret = 1
		    END IF
		  ELSE
		    kret = 1
		END IF
	        IF ( ilocv .ne. 0) THEN
C
C*		    We have a second visiblity to decode.
C
	            ilocb = ilocv + 1
	            iloce = iloc
	            i     = i + 1
	          ELSE
C
C*		    No second visbility.
C
	            i = 3
	        END IF
	    END DO
C
	    iret = kret
	    rivals ( irnrwy ) = irvr
C
          ELSE IF ( iret .eq. 1 ) THEN
C
C*	    Found 'R' not in first position - not a runway.
C
	    iret = -1
	  ELSE
C
C*          No runway visual range group found.
C
	END IF
	IF ( iret .gt. 0 ) iret = 10
C*
	RETURN
	END
