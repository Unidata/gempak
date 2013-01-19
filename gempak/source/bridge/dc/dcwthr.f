	SUBROUTINE DC_WTHR ( string, lensin, wxfnd, outstr, iret ) 
C************************************************************************
C* DC_WTHR                                                              *
C*                                                                      *
C* This subroutine will validate a weather phenomena group.	        *
C* 								        *
C* DC_WTHR ( STRING, LENSIN, WXFND, OUTSTR, IRET )                      *
C*								        *
C* Input parameters: 						        *
C*      STRING		CHAR*		Possible weather phenom field	*
C*	LENSIN		INTEGER		Length of weather string        *
C*								        *
C* Output parameters:						        *
C*	WXFND           LOGICAL         .T. if weather group identified *
C*      OUTSTR          CHAR*           Modified string                 *
C*	IRET		INTEGER		Return code                     *
C*	      		    		  0 = normal return       	*
C*	                                 32 = unrecognized WX code 	*
C**								        *
C* Log:									*
C* A. Hardy/GSC          3/98   					*
C* A. Hardy/GSC          5/98	   Solo wx descriptors are ignored;     *
C*                                 Added PL as second designator for PE *
C* D. Kidwell/NCEP       7/98	   Replace PE with PL                   *
C* D. Kidwell/NCEP       9/98	   Allow TS as a solo wx descriptor     *
C* D. Kidwell/NCEP       7/99	   Check for miscoded string HAZE       *
C************************************************************************
	INCLUDE 	'dccmn.cmn'
C*
	CHARACTER*(*) 	string, outstr
	LOGICAL		wxfnd
C*
	PARAMETER	( NUMWX = 31 )
        CHARACTER     	wpart (NUMWX)*2, str*2, tmpstr*20, t2*2
	LOGICAL 	alpha, search, bad
C
C*      The following table includes all valid two-letter METAR
C*      present weather codes.
C
        DATA            wpart / 'TS','SH','RA','DZ','SN','SG',
     +                          'IC','PL','GR','GS','UP','FZ',
     +                          'BR','FG','FU','VA','DU','SA',
     +                          'HZ','PY','BL','VC','PO','SQ',
     +                          'FC','SS','DS','MI','PR','BC','DR'/
C------------------------------------------------------------------------
 	iret  = 0
	ibeg  = 0
	wxfnd = .false.
C
C*	Check to see if this could be weather phenomena.
C
        tmpstr = string
	lens   = lensin
C
C*      First check for special cases of miscoded weather.
C
	CALL ST_RPST ( tmpstr, 'RAIN', 'RA', ipos1, tmpstr, jret )
	CALL ST_RPST ( tmpstr, 'SNOW', 'SN', ipos2, tmpstr, jret )
	CALL ST_RPST ( tmpstr, 'SNW',  'SN', ipos3, tmpstr, jret )
	CALL ST_RPST ( tmpstr, 'HAZY', 'HZ', ipos4, tmpstr, jret )
	CALL ST_RPST ( tmpstr, 'HZY',  'HZ', ipos5, tmpstr, jret )
	CALL ST_RPST ( tmpstr, 'HAZE', 'HZ', ipos6, tmpstr, jret )
        IF ( ( ipos1 + ipos2 + ipos3 + ipos4 + ipos5 + ipos6 ) .gt. 0 ) 
     +	   THEN
            lens = INDEX ( tmpstr, ' ' ) - 1
	END IF
C
C*      Replace ice pellet string 'PE' with 'PL'.
C
	CALL ST_RPST ( tmpstr, 'PE', 'PL', ipos, tmpstr, jret )
C
	IF ( ( lens .eq. 3 ) .or. ( lens .eq. 5 ) .or. ( lens .eq. 7 ) 
     +       .or. (lens .eq. 9 ) ) THEN
	    IF ( ( tmpstr ( 1:1 ) .eq. '+' ) .or. 
     +		 ( tmpstr ( 1:1 ) .eq. '-' ) ) ibeg = 2
	  ELSE IF ( ( lens .eq. 2 ) .or. ( lens .eq. 4 ) .or.
     +	          ( lens .eq. 6 ) .or. ( lens .eq. 8 ) ) THEN
	    ibeg = 1
	END IF
C
C*	Check for alphabetic string.
C 
	alpha = .true.
	IF ( ibeg .eq. 0 ) alpha = .false.
	i = ibeg
	DO WHILE ( alpha )
	    CALL ST_ALNM ( tmpstr ( i:i ), ityp, jret )
	    IF ( ityp .ne. 2 ) THEN
		alpha = .false.
	      ELSE
		i = i + 1
		IF ( i .gt. lens ) alpha = .false.
	    END IF
	END DO
	IF ( i .gt. lens ) THEN
C
C*	    String is alpha, of length 2, 4, 6 or 8, with possible
C*	    + or - preceding.
C*	    Look for valid weather phenomena.
C 
	    bad = .false.
            i   = ibeg
C
C*          If only a weather descriptor was reported, then weather
C*          validation will not occur.  An exception is TS
C*	    (thunderstorm), which is allowed to appear by itself per
C*	    Weather Service Observing Handbook 7 (NWS-OH7).
C
            IF ( lens .le. 3 ) THEN
	        t2 = tmpstr( i:i+1 )
	        IF  ( ( t2.eq. 'BC' ) .or. ( t2.eq. 'BL' )
     +            .or.  ( t2.eq. 'DR' ) .or. ( t2.eq. 'FZ' )
     +            .or.  ( t2.eq. 'MI' ) .or. ( t2.eq. 'PR' )
     +            .or.  ( t2.eq. 'SH' ) )      bad = .true.
	    END IF
            DO WHILE ( ( .not. bad ) .and. ( i .lt. lens ) )
                search = .true.
                j      = 1
                str    = tmpstr ( i:i + 1 )
                DO WHILE ( search )
                    IF ( str .eq. wpart ( j ) ) THEN
                        search = .false.
			wxfnd  = .true.
                      ELSE
                        j = j + 1
                        IF ( j .gt. NUMWX ) THEN
                            search = .false.
			    bad    = .true.
			    iret   = 32
                        END IF
                    END IF
                END DO
                i = i + 2
            END DO
C
            IF ( bad ) wxfnd = .false.
	END IF
	outstr = tmpstr
C*
	RETURN
	END
