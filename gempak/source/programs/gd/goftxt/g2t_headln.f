	SUBROUTINE G2T_HEADLN ( ntype, dayw, lunt, iret )
C************************************************************************
C* G2T_HEADLN								*
C*									*
C* This subroutine creates the headline message for OFF text.		*
C*									*
C* G2T_HEADLN ( NTYPE, DAYW, LUNT, IRET )				*
C*									*
C* Input parameters:							*
C*	NTYPE		INTEGER		Type of the storm (not used)	*
C*					  1: Extratropical		*
C*					  2. Tropical			*
C*	DAYW(*)		CHAR*		Day of the week			*
C*	LUNT		INTEGER		LUN for offshore text file	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		 9/06						*
C* T. Lee/SAIC		12/06	Rewritten based on headline document	*
C* T. Lee/SAIC		12/06	Added G2T_WRN1, G2T_WRN2		*
C* T. Lee/SAIC		 2/07	Added G2T_FILL for time trend		*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	CHARACTER*(*)	dayw(*)
	LOGICAL		ftwoln, apendh, apends, apendg
	CHARACTER*128	ooo, warn(2) 
	CHARACTER*40	headln(8)
	CHARACTER*2	eol, spac
	INTEGER		nt(9), nh(8)
	DATA		nt / 1, 2, 3, 4, 5, 6, 7, 8, 9 /
	DATA		headln / '...HURRICANE FORCE WIND WARNING...',
     +				 '...STORM WARNING...',
     +				 '...GALE WARNING...',
     +				 '...HURRICANE FORCE WINDS EXPECTED',
     +				 '...STORM FORCE WINDS EXPECTED',
     +				 '...GALE FORCE WINDS EXPECTED',
     +				 '...HURRICANE WARNING...',
     +				 '...TROPICAL STORM WARNING...'/
C-----------------------------------------------------------------------
	iret = 0
	apendh = .false.
	apends = .false.
	apendg = .false.
C
C*	Fill the non-synoptic flags to synoptic time. (comment out for
C*	now to compare with snap shot!
C
	CALL G2T_FILL ( iret )
C	
	DO iw = 1, 2
	    warn ( iw ) = ' '
	END DO
C
C*	For any forecast period, retain the higher category and set the
C*	lower categories to missing.
C
	DO ii = 1, ngrdtm
	    IF  ( fhurr ( ii ) )  THEN
		CALL G2T_MISS ( 2, ii, ii, iret )
		CALL G2T_MISS ( 3, ii, ii, iret )
	    END IF
C
	    IF  ( fstorm ( ii ) )  THEN
		CALL G2T_MISS ( 3, ii, ii, iret )
	    END IF
	END DO
C
C*	For the 1-4 period (nt = 1, 3, 5, 7), using the following table.
C*
C*	 First 2P/24h  3 or 4P        Headline
C*	 ---------------------------------------------------------------
C*	  Nothing	 Gale	    Gale Force Winds Expected 3P (and 4P)
C*	  Gale	         Gale	    Gale Warning 
C*	  Gale	         Storm	    Gale Warning	
C*				    Storm Force Winds Expected
C*	  Gale to Storm  Gale/Storm Storm Warning
C*	  Storm	         Storm	    Storm Warning
C*	  Storm	         Hurricane  Storm Warning
C*				    Hurricane Force Winds Exp.
C
	ftwoln = .false.
C
C*	If the 1st and/or 2nd period is set for hurricane warning, only
C*	retain the warning to the 1st period and set the rest to flags
C*	to missing.  If 3rd and/or 4th is set for hurricane warning, set
C*	the 5th and the rest periods to missing. 
C
	IF  ( fhurr ( 1 ) .or. fhurr ( 3 ) )  THEN
	    CALL G2T_MISS ( 1, nt ( 1 ), ngrdtm, iret )
	    CALL G2T_MISS ( 2, nt ( 1 ), ngrdtm, iret )
	    CALL G2T_MISS ( 3, nt ( 1 ), ngrdtm, iret )
	    fhurr ( 1 ) = .true.
	  ELSE IF ( fhurr ( 5 ) .or. fhurr ( 7 ) )  THEN
            CALL G2T_MISS ( 1, nt ( 9 ), ngrdtm, iret )
            CALL G2T_MISS ( 2, nt ( 9 ), ngrdtm, iret ) 
            CALL G2T_MISS ( 3, nt ( 9 ), ngrdtm, iret )
	END IF
C
C*	If the 1st and/or 2nd period (nt = 1 and/or 3 ) is set for storm 
C*	warning, only retain the warning to the 1st period and set the 
C*	rest storm and gale flags to missing.  Check the rest period if 
C*	any hurricane flag is set.  If it does, two line warnings are 
C*	needed and the 5th period and beyond may need to be added to the 
C*	headline if necessary.
C*
C*	If 3rd and/or 4th period ( nt = 5 and/or 7 ) is set for storm 
C*	warning, set the 5th (nt = 9 ) and the rest to missing. 
C
C
	IF ( fstorm ( 1 ) .or. fstorm ( 3 ) )  THEN
	    CALL G2T_MISS ( 2, nt ( 1 ), ngrdtm, iret )
	    CALL G2T_MISS ( 3, nt ( 1 ), ngrdtm, iret )
	    fstorm ( 1 ) = .true.
C
C*	    Two liners needed for hurricane force winds.
C
	    DO ii = 5, ngrdtm, 2
		IF ( fhurr ( ii ) )  THEN
		    ftwoln = .true.
		    IF ( ii .ge. nt ( 9 ) )  apendh = .true.
		    CALL G2T_WRNX ( 1, iret )
		END IF
	    END DO
	  ELSE IF ( fstorm ( 5 ) .or. fstorm ( 7 ) )  THEN
            CALL G2T_MISS ( 2, nt ( 9 ), ngrdtm, iret )
            CALL G2T_MISS ( 3, nt ( 9 ), ngrdtm, iret )
	    DO ii = 9, ngrdtm, 2
		ftwoln = .true.
		IF ( fhurr ( ii ) )  apendh = .true.
		CALL G2T_WRNX ( 1, iret )
	    END DO
	END IF
C
C*	Same methodology is applied to GALE warning.
C
	IF ( fgale ( 1 ) .or. fgale ( 3 ) )  THEN
	    CALL G2T_MISS ( 3, nt ( 1 ), ngrdtm, iret )
	    fgale ( 1 ) = .true.
C
C*	    Two liners needed for hurricane and storm force winds.
C*	    Also check 5th period and beyond.
C
	    DO ii = 5, ngrdtm, 2
		IF ( fhurr ( ii ) .or. fstorm ( ii ) )  THEN
		    ftwoln = .true.
		    IF ( ii .ge. nt ( 9 ) ) THEN
			IF ( fhurr ( ii ) )  THEN
			    apendh = .true.
			    CALL G2T_WRNX ( 1, iret )
			  ELSE
			    apends = .true.
			    CALL G2T_WRNX ( 2, iret )
			END IF
		    END IF
		END IF
	    END DO
	  ELSE IF ( .not. fgale ( 5 ) .and. .not. fgale ( 7 ) )  THEN
C
C*	    In the case that no warning for the first four periods.  
C*	    Check if appending needed.
C
	    DO ii = 9, ngrdtm, 2
		IF ( fhurr ( ii )  )  THEN
		    apendh = .true.
		    CALL G2T_WRNX ( 1, iret )
		  ELSE IF ( fstorm ( ii )  )  THEN
		    apends = .true.
		    CALL G2T_WRNX ( 2, iret )
		  ELSE IF ( fgale ( ii )  )  THEN
		    apendg = .true.
		    CALL G2T_WRNX ( 3, iret )
		END IF
	    END DO
	  ELSE  IF ( fgale ( 5 ) .or. fgale ( 7 ) )  THEN
            CALL G2T_MISS ( 3, nt ( 9 ), ngrdtm, iret )
	    DO ii = 9, ngrdtm, 2
		IF ( fhurr ( ii )  )  THEN
		    apendh = .true.
		    ftwoln = .true.
		    CALL G2T_WRNX ( 1, iret )
		  ELSE IF ( fstorm ( ii )  )  THEN
		    apends = .true.
		    ftwoln = .true.
		    CALL G2T_WRNX ( 2, iret )
		END IF
	    END DO
	END IF	
C
	DO ii = 1, 6
	    CALL ST_LSTR ( headln ( ii ), nh ( ii ), ier )
	END DO
C
C	Create and write out storm warning headline.
C
	IF ( ftwoln )  THEN
	    IF ( fgale ( 1 ) .or. fgale ( 5 ) .or. fgale ( 7 ) )  THEN
		IF  ( fgale ( 1 ) )  THEN
		    warn ( 1 ) = headln ( 3 ) ( : nh ( 5 ) )
		  ELSE IF ( fgale ( 5 ) .or. fgale ( 7 ) )  THEN
		    CALL G2T_WRN1 ( fgale  ( 5 ), fgale ( 7 ),
     +				    dayw   ( 5 ), dayw  ( 7 ),
     +				    headln ( 6 ), warn ( 1 ), ier )
		END IF
C
		IF  ( fhurr ( 5 ) .or. fhurr ( 7 ) )  THEN
		    CALL G2T_WRN1 ( fhurr  ( 5 ), fhurr ( 7 ), 
     +				    dayw   ( 5 ), dayw  ( 7 ),
     +				    headln ( 4 ), warn  ( 2 ), ier )
		  ELSE IF ( fstorm ( 5 ) .or. fstorm ( 7 ) ) THEN
			CALL G2T_WRN1 ( fstorm ( 5 ), fstorm ( 7 ),
     +                              dayw   ( 5 ), dayw  ( 7 ),
     +                              headln ( 5 ), warn  ( 2 ), ier )
		END IF
	      ELSE IF ( fstorm (1) .or. fstorm (5) .or. fstorm (7) ) THEN
		IF  ( fstorm ( 1 ) )  THEN
		    warn ( 1 ) = headln ( 2 ) ( : nh ( 2 ) )
		  ELSE IF ( fstorm ( 5 ) .or. fstorm ( 7 ) ) THEN
		    CALL G2T_WRN1 ( fstorm ( 5 ), fstorm ( 7 ),
     +				    dayw   ( 5 ), dayw  ( 7 ),
     +				    headln ( 5 ), warn  ( 1 ), ier )
		END IF
C
		IF  ( fhurr ( 5 ) .or. fhurr ( 7 ) )  THEN
		    CALL G2T_WRN1 ( fhurr  ( 5 ), fhurr ( 7 ), 
     +				    dayw   ( 5 ), dayw  ( 7 ),
     +				    headln ( 4 ), warn  ( 2 ), ier )
		END IF
	    END IF
	  ELSE
	    IF ( fgale ( 1 ) .or. fgale ( 5 ) .or. fgale ( 7 ) ) THEN
		IF ( fgale ( 1 ) )  THEN
		    warn ( 2 ) = headln ( 3 ) ( : nh ( 3 ) )
		  ELSE IF  ( fgale ( 5 ) .or. fgale ( 7 ) )  THEN
		    CALL G2T_WRN1 ( fgale  ( 5 ), fgale ( 7 ), 
     +				    dayw   ( 5 ), dayw  ( 7 ),
     +				    headln ( 6 ), warn  ( 2 ), ier )
		END IF
	      ELSE IF ( fstorm (1) .or. fstorm (5) .or. fstorm(7) ) THEN
		IF  ( fstorm ( 1 ) )  THEN
		    warn ( 2 ) = headln ( 2 ) ( : nh ( 2 ) )
		  ELSE IF ( fstorm (5) .or. fstorm ( 7 ) )  THEN
		    CALL G2T_WRN1 ( fstorm ( 5 ), fstorm ( 7 ), 
     +				    dayw   ( 5 ), dayw   ( 7 ),
     +				    headln ( 5 ), warn   ( 2 ), ier )
		END IF
	      ELSE IF ( fhurr (1) .or. fhurr (5) .or. fhurr (7) )  THEN
		IF ( fhurr ( 1 ) )  THEN
		    warn ( 2 ) = headln ( 1 ) ( : nh ( 1 ) )
		  ELSE IF  ( fhurr ( 5 ) .or. fhurr ( 7 ) )  THEN
		    CALL G2T_WRN1 ( fhurr  ( 5 ), fhurr ( 7 ), 
     +				    dayw   ( 5 ), dayw  ( 7 ),
     +				    headln ( 4 ), warn  ( 2 ), ier )
		END IF
	    END IF
	END IF
C
C*	Added the 5th period and beyond if necessary.		
C
	IF ( apendh )  THEN
	    CALL G2T_WRN2 ( 1, dayw, headln ( 4 ), warn ( 2 ), ier )
	  ELSE IF ( apends )  THEN
	    CALL G2T_WRN2 ( 2, dayw, headln ( 5 ), warn ( 2 ), ier )
	  ELSE IF ( apendg )  THEN
	    CALL G2T_WRN2 ( 3, dayw, headln ( 6 ), warn ( 2 ), ier )
	END IF
C
C*	Write out headline.
C
        CALL ST_LSTR  ( warn ( 2 ), len, ier )
	IF  ( len .gt. MXCHAR )  THEN
	    eol = CHLF // CHNULL
	    spac = ' ' // CHNULL
	    CALL ST_NULL ( warn ( 2 ),  warn ( 2 ), len, ier )
	    CALL CST_WRAP ( warn ( 2 ), spac, MXCHAR, eol, CHNULL, 
     +			    warn ( 2 ), ier )
	END IF
C*
	DO ii = 1, 2
	    IF  ( warn ( ii ) .ne. ' ' )  THEN
		ooo = warn ( ii )
		CALL ST_LSTR ( ooo, looo, ier )
		WRITE ( lunt, 10 ) warn ( ii ) ( : looo )
	    END IF
	END DO
10	FORMAT ( A )
C*
	RETURN
	END
