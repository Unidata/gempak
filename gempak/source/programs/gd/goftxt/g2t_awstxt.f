	SUBROUTINE G2T_AWSTXT ( ktype, lunb, nt, end, iret )
C************************************************************************
C* G2T_AWSTXT								*
C*									*
C* This subroutine adds wave/wind text to OFFTXT based on the data in 	*
C* the two trending buffers.  For example, for Period A to RbEc, period *
C* A will be stored in the first trending buffer while RbEc will be	*
C* stored in the 2nd.  The selection of final wording is determined by 	*
C* data type, KTYPE, and magnitude of the RANGE and EXCEPT parts.	*
C*									*
C* G2T_AWSTXT ( KTYPE, LUNB, NT, END, IRET )				*
C*									*
C* Input parameters:							*
C*	KTYPE		INTEGER		Data type			*
C*					 1 = Wave			*
C*					 2 = Wind			*
C*	LUNB		INTEGER		LUN for G2T_TXT.TBL		*
C*	NT		INTEGER		Nth time step			*
C*	END		INTEGER		Appending text, EARLY/LATE	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		06/07	Created					*
C* T. Lee/SAIC		08/07	Defined text for "BECOMING"		*
C* T. Lee/SAIC		11/07	Added NT for combining period		*
C* T. Lee/SAIC		11/07	Added spdflg for trending; Improved	*
C*				"shifting" algorithm; Combined periods	*
C* T. Lee/SAC		06/08	Use 1 FT or less for 0 FT wave		*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	CHARACTER*(*)	end
	CHARACTER	ooo*72, woo*72, str(2)*5, wd_1(2)*2, wd_2(2)*2
	INTEGER		kd_1(2), kd_2(2)
	LOGICAL		E1, E2, dirflg, spdflg, shift, spread
	INCLUDE		'EQUAL.FNC'
C------------------------------------------------------------------------
	iret = 0
	dirflg = .false.
	spdflg = .false.
	shift = .false.
C
	ooo = ' '	
	DO ii = 1, 2
	    str ( ii ) = ' '
	    kd_1 (ii ) = IMISSD
	    kd_2 (ii ) = IMISSD
	END DO
C
	min_1  = mxn_d  ( ktype, 1, 1 )
	max_1  = mxn_d  ( ktype, 1, 2 )
	mine_1 = mxn_de ( ktype, 1, 1 )
	maxe_1 = mxn_de ( ktype, 1, 2 )
	E1 = eflag_d ( ktype , 1 )
C
	min_2  = mxn_d  ( ktype, 2, 1 )
	max_2  = mxn_d  ( ktype, 2, 2 )
	mine_2 = mxn_de ( ktype, 2, 1 )
	maxe_2 = mxn_de ( ktype, 2, 2 )
	E2 = eflag_d ( ktype , 2 )
C
C*	Append text from the 1st scratch buffer.
C
	itrnd = 1
	CALL G2T_RANGX ( ktype, lunb, nt, itrnd, ier )
	CALL G2T_APPEX ( ktype, lunb, nt, itrnd, ' ', ier )
C
	IF ( ktype .eq. 2 )  THEN
	    spdflg = ( max_1 .ne. max_2 ) .or. ( min_1 .ne. min_2 )
C
C*	    Convert wind directions to eight cardinal directions.
C
	    DO ii = 1, 2
		wd_1 ( ii ) =  wdir_d ( ii, 1 )
		wd_2 ( ii ) =  wdir_d ( ii, 2 )
		CALL G2T_COMPASS ( wd_1 ( ii ), kd_1 ( ii ), ier )
		CALL G2T_COMPASS ( wd_2 ( ii ), kd_2 ( ii ), ier )
	    END DO
C
C*	    Get the mean cardinal wind direction.
C
	    CALL G2T_MWDIR ( kd_1, rd1, ier )
	    CALL G2T_MWDIR ( kd_2, rd2, ier )
C
C*	    Set directional flag.
C
	    dirflg = ( ( max_1 .le. LGHTS ) .or.
     +		   ( CNE ( wd_2 ( 1 ), wd_1 ( 1 ) ) .and.
     +		     CNE ( wd_2 ( 1 ), wd_1 ( 2 ) ) ) .or.
     +		   ( CNE ( wd_2 ( 2 ), wd_1 ( 1 ) ) .and.
     +		     CNE ( wd_2 ( 2 ), wd_1 ( 2 ) ) ) )
C
C*	    Set shifting flag: If wind max exceed or equal to 15 for 
C*	    both periods, and wind directions change more than 45 
C*	    degrees.
C
	    IF ( .not. dirflg )  THEN
C
	      ELSE
		IF ( max_ 1 .ge. 15 .and. max_2 .ge. 15 )  THEN
		  kdif1 = ABS ( kd_1 ( 1 ) - kd_1 ( 2 ) )
		  kdif2 = ABS ( kd_2 ( 1 ) - kd_2 ( 2 ) )
		  IF  ( ( kdif1 .le. 1 .or. kdif1 .ge. 7 ) .and.
     +			( kdif2 .le. 1 .or. kdif2 .ge. 7 ) )  THEN
		      shift = ( ABS ( rd1 - rd2 ) .ge. 2. ) .and.
     +			      ( ABS ( rd1 - rd2 ) .le. 6. )
		    ELSE 
C
		      IF ( kdif1 .le. 1 .or. kdif1 .ge. 7 ) THEN
			  shift = ( ABS ( rd1 - kd_2(1) ) .ge. 2. .and.
     +				    ABS ( rd1 - kd_2(1) ) .le. 6. )
			ELSE IF ( kdif2 .le. 1 .or. kdif2 .ge. 7 ) THEN
			  shift = ( ABS ( rd2 - kd_1(1) ) .ge. 2. .and.
     +				    ABS ( rd2 - kd_1(1) ) .le. 6. )
			ELSE
			  shift = ABS ( kd_1 (1) - kd_2 (1) ) .ge. 2.
     +				  .and. 
     +				  ABS ( kd_1 (1) - kd_2 (1) ) .le. 6.
		      END IF
		  END IF
	       END IF
	    END IF
C
C*	    Handle wind spread for the range part of the 2nd period.
C*	    The exception part will be taken care by G2T_APPEX module.
C
	    CALL G2T_WSPREAD ( wd_2, spread, ier )
	END IF
C
C*	Retrieve appropriate trending text from the table based on RANGE
C*	and EXCEPTION.
C
	IF ( .not. IRMISS ( max_2 ) )  THEN
	    itrnd = 2
	    CALL ST_INCH ( min_2, str ( 1 ), ier )
	    CALL ST_INCH ( max_2, str ( 2 ), ier )
C	      
	    IF ( E1 .and. E2 )  THEN
		IF  ( ( max_2  .gt. max_1 ) .and.
     +		      ( min_2  .gt. min_1 ) .and.
     +		      ( maxe_2 .gt. maxe_1 ) .and.
     +		      ( mine_2 .gt. mine_1 ) )  THEN
		    IF ( ktype .eq. 1 )  THEN
			CALL G2T_GTEXT ( lunb, 40, str, ooo, ier )
		      ELSE
C
C*			Light and variable winds. 
C
			IF ( max_2 .le. LGHTS )  THEN
			    str ( 1 ) = ' '
			    CALL ST_INCH ( LGHTS, str ( 2 ), ier )
			    CALL G2T_GTEXT ( lunb, 30, str, ooo, ier )
			  ELSE
			    IF  ( dirflg )  THEN 
			        IF ( shift )  THEN
			            CALL G2T_GTEXT(lunb,19,wd_2,woo,ier)
			          ELSE
			            CALL G2T_GTEXT(lunb,16,wd_2,woo,ier)
			        END IF
C
				IF ( spdflg ) THEN
			          CALL G2T_GTEXT ( lunb,11,str,ooo,ier )
				 ELSE
				  ooo = '.'
				END IF
C
			        CALL ST_LSTR ( woo, iwoo, ier )
			        CALL ST_LSTR ( ooo, iooo, ier )
			        ooo = woo ( : iwoo ) // ooo ( : iooo )
			      ELSE
			       CALL G2T_GTEXT( lunb, 20, str, ooo, ier )
			    END IF
			END IF
		    END IF
		  ELSE IF ( ( max_2 .lt. max_1 ) .and.
     +			    ( min_2 .lt. min_1 ) .and.
     +		      	    ( maxe_2 .lt. maxe_1 ) .and.
     +			    ( mine_2 .lt. mine_1 ) )  THEN
		    IF ( ktype .eq. 1 )  THEN
			IF ( min_2 .eq. 0 .or. max_2 .eq. 0 )  THEN
			    CALL G2T_GTEXT ( lunb, 56, str, ooo, ier )
			  ELSE
			    CALL G2T_GTEXT ( lunb, 45, str, ooo, ier )
			END IF
		      ELSE
C
C*			Light and variable winds. 
C
			IF ( max_2 .le. LGHTS )  THEN
			    str ( 1 ) = ' '
			    CALL ST_INCH ( LGHTS, str ( 2 ), ier )
			    IF ( max_1 .le. LGHTS )  THEN
				CALL G2T_GTEXT (lunb, 30, str, ooo, ier)
			      ELSE
				CALL G2T_GTEXT (lunb, 28, str, ooo, ier)
			    END IF
			  ELSE
			    IF  ( dirflg )  THEN 
				IF ( shift )  THEN
			          CALL G2T_GTEXT (lunb,19,wd_2,woo,ier)
				ELSE
				  CALL G2T_GTEXT (lunb,17,wd_2,woo,ier)
				END IF
C
				IF ( spdflg ) THEN
			          CALL G2T_GTEXT ( lunb,11,str,ooo,ier )
				 ELSE
				  ooo = '.'
				END IF
C
				CALL ST_LSTR ( woo, iwoo, ier )
				CALL ST_LSTR ( ooo, iooo, ier )
				ooo = woo ( : iwoo ) // ooo ( : iooo )
			      ELSE
				CALL G2T_GTEXT (lunb, 25, str, ooo, ier)
			    END IF
			END IF
		    END IF
		  ELSE
		    IF ( ktype .eq. 1 )  THEN
			CALL G2T_GTEXT ( lunb, 48, str, ooo, ier )
		      ELSE
			IF ( max_2 .le. LGHTS )  THEN
			    str ( 1 ) = ' '
			    CALL ST_INCH ( LGHTS, str ( 2 ), ier )
			    CALL G2T_GTEXT ( lunb, 30, str, ooo, ier )
			  ELSE
			    IF  ( dirflg )  THEN 
				IF ( shift )  THEN
			          CALL G2T_GTEXT (lunb,19,wd_2,woo,ier)
				ELSE
				  CALL G2T_GTEXT (lunb,18,wd_2,woo,ier)
				END IF
C
				IF ( spdflg ) THEN
			          CALL G2T_GTEXT ( lunb,11,str,ooo,ier )
				 ELSE
				  ooo = '.'
				END IF
C
				CALL ST_LSTR ( woo, iwoo, ier )
				CALL ST_LSTR ( ooo, iooo, ier )
				ooo = woo ( : iwoo ) // ooo ( : iooo )
			      ELSE
				CALL G2T_GTEXT (lunb, 32, str, ooo, ier)
			    END IF
			END IF
		    END IF
		END IF
	      ELSE IF ( E1 .and. .not. E2 )  THEN
		IF  ( max_2 .gt. maxe_1 .and. min_2 .gt. mine_1 .and.
     +		      min_2 .gt. min_1 )  THEN
		    IF ( ktype .eq. 1 )  THEN
			CALL G2T_GTEXT ( lunb, 41, str, ooo, ier )
		      ELSE
			IF  ( dirflg )  THEN 
			    IF ( shift )  THEN
			        CALL G2T_GTEXT(lunb, 19, wd_2, woo, ier)
			      ELSE
			        CALL G2T_GTEXT(lunb, 16, wd_2, woo, ier)
			    END IF
C
			    IF ( spdflg )  THEN
				CALL G2T_GTEXT ( lunb,12,str,ooo,ier )
			      ELSE
				ooo = ' THROUGHOUT.'
			    END IF
C
			    CALL ST_LSTR ( woo, iwoo, ier )
			    CALL ST_LSTR ( ooo, iooo, ier )
			    ooo = woo ( : iwoo ) // ooo ( : iooo )
			  ELSE
			   CALL G2T_GTEXT ( lunb, 21, str, ooo, ier )
			END IF
		    END IF
		  ELSE IF ( max_2 .lt. max_1 .and. min_2 .lt. min_1 
     +			    .and.  min_2 .lt. mine_1 ) THEN
		    IF ( ktype .eq. 1 )  THEN
			IF ( min_2 .eq. 0 .or. max_2 .eq. 0 )  THEN
			    CALL G2T_GTEXT ( lunb, 58, str, ooo, ier )
			  ELSE
			    CALL G2T_GTEXT ( lunb, 47, str, ooo, ier )
			END IF
		      ELSE
			IF ( max_2 .le. LGHTS )  THEN
			    str ( 1 ) = ' '
			    CALL ST_INCH ( LGHTS, str ( 2 ), ier )
			    CALL G2T_GTEXT ( lunb, 29, str, ooo, ier )
			  ELSE
			    IF  ( dirflg )  THEN 
				IF ( shift )  THEN
			          CALL G2T_GTEXT (lunb,19,wd_2,woo,ier)
				ELSE
				  CALL G2T_GTEXT (lunb,17,wd_2,woo,ier)
				END IF
C
				IF ( spdflg )  THEN
				  CALL G2T_GTEXT ( lunb,12,str,ooo,ier )
				 ELSE
				  ooo = ' THROUGHOUT.'
				END IF
C
				CALL ST_LSTR ( woo, iwoo, ier )
				CALL ST_LSTR ( ooo, iooo, ier )
				ooo = woo ( : iwoo ) // ooo ( : iooo )
			      ELSE
				CALL G2T_GTEXT (lunb, 27, str, ooo, ier)
			    END IF
			END IF
		    END IF
		  ELSE
		    IF ( ktype .eq. 1 )  THEN
			CALL G2T_GTEXT ( lunb, 49, str, ooo, ier )
		      ELSE
			IF ( max_2 .le. LGHTS )  THEN
			    str ( 1 ) = ' '
			    CALL ST_INCH ( LGHTS, str ( 2 ), ier )
			    CALL G2T_GTEXT ( lunb, 31, str, ooo, ier )
			  ELSE
			    IF  ( dirflg )  THEN 
				IF ( shift )  THEN
				    CALL G2T_GTEXT(lunb,19,wd_2,woo,ier)
			          ELSE
				    CALL G2T_GTEXT(lunb,18,wd_2,woo,ier)
				END IF
C
				IF ( spdflg )  THEN
				  CALL G2T_GTEXT ( lunb,12,str,ooo,ier )
				 ELSE
				  ooo = ' THROUGHOUT.'
				END IF
C
				CALL ST_LSTR ( woo, iwoo, ier )
				CALL ST_LSTR ( ooo, iooo, ier )
				ooo = woo ( : iwoo ) // ooo ( : iooo )
			      ELSE
				CALL G2T_GTEXT (lunb, 33, str, ooo, ier)
			    END IF
			END IF
		    END IF
		END IF
	      ELSE IF ( .not. E1 .and. E2 )  THEN
		IF  ( max_2 .gt. max_1 .and. min_2 .gt. min_1 .and.
     +		      mine_2 .gt. min_1 )  THEN
		    IF ( ktype .eq. 1 )  THEN
			CALL G2T_GTEXT ( lunb, 40, str, ooo, ier )
		      ELSE
			IF  ( dirflg )  THEN 
			    IF ( shift )  THEN
			        CALL G2T_GTEXT(lunb, 19, wd_2, woo, ier)
			      ELSE
			        CALL G2T_GTEXT(lunb, 16, wd_2, woo, ier)
			    END IF
C
			    IF ( spdflg ) THEN
				CALL G2T_GTEXT ( lunb,11,str,ooo,ier )
			      ELSE
				ooo = '.'
			    END IF
C
			    CALL ST_LSTR ( woo, iwoo, ier )
			    CALL ST_LSTR ( ooo, iooo, ier )
			    ooo = woo ( : iwoo ) // ooo ( : iooo )
			  ELSE
			   CALL G2T_GTEXT ( lunb, 20, str, ooo, ier )
			END IF
		    END IF
		  ELSE IF ( maxe_2 .lt. max_1 .and. min_2 .lt. min_1
     +			    .and. mine_2 .lt. min_1 )  THEN
		    IF ( ktype .eq. 1 )  THEN
			IF ( min_2 .eq. 0 .or. max_2 .eq. 0 )  THEN
			    CALL G2T_GTEXT ( lunb, 56, str, ooo, ier )
			  ELSE
			    CALL G2T_GTEXT ( lunb, 45, str, ooo, ier )
			END IF
		      ELSE
			IF ( max_2 .le. LGHTS )  THEN
			    str ( 1 ) = ' '
			    CALL ST_INCH ( LGHTS, str ( 2 ), ier )
			    IF ( max_1 .le. LGHTS )  THEN
				CALL G2T_GTEXT (lunb, 30, str, ooo, ier)
			      ELSE
				CALL G2T_GTEXT (lunb, 28, str, ooo, ier)
			    END IF
			  ELSE
			    IF  ( dirflg )  THEN 
				IF ( shift )  THEN
			          CALL G2T_GTEXT (lunb,19,wd_2,woo,ier)
				ELSE
				  CALL G2T_GTEXT (lunb,17,wd_2,woo,ier)
				END IF
C
				IF ( spdflg ) THEN
			          CALL G2T_GTEXT ( lunb,11,str,ooo,ier )
				 ELSE
				  ooo = '.'
				END IF
C
				CALL ST_LSTR ( woo, iwoo, ier )
				CALL ST_LSTR ( ooo, iooo, ier )
				ooo = woo ( : iwoo ) // ooo ( : iooo )
			      ELSE
				CALL G2T_GTEXT (lunb, 25, str, ooo, ier)
			    END IF
			END IF
		    END IF
		  ELSE
		    IF ( ktype .eq. 1 )  THEN
			CALL G2T_GTEXT ( lunb, 48, str, ooo, ier )
		      ELSE
			IF ( max_2 .le. LGHTS )  THEN
			    str ( 1 ) = ' '
			    CALL ST_INCH ( LGHTS, str ( 2 ), ier )
			    CALL G2T_GTEXT ( lunb, 30, str, ooo, ier )
			  ELSE
			    IF  ( dirflg )  THEN 
				IF ( shift )  THEN
				    CALL G2T_GTEXT(lunb,19,wd_2,woo,ier)
			          ELSE
				    CALL G2T_GTEXT(lunb,18,wd_2,woo,ier)
				END IF
C
				IF ( spdflg ) THEN
			          CALL G2T_GTEXT ( lunb,11,str,ooo,ier )
				 ELSE
				  ooo = '.'
				END IF
C
				CALL ST_LSTR ( woo, iwoo, ier )
				CALL ST_LSTR ( ooo, iooo, ier )
				ooo = woo ( : iwoo ) // ooo ( : iooo )
			      ELSE
				CALL G2T_GTEXT (lunb, 32, str, ooo, ier)
			    END IF
			END IF
		    END IF
		END IF
	      ELSE IF ( .not. E1 .and. .not. E2 )  THEN
		IF  ( max_2 .gt. max_1 .and. min_2 .gt. min_1  )  THEN
		    IF ( ktype .eq. 1 )  THEN
			CALL G2T_GTEXT ( lunb, 40, str, ooo, ier )
		      ELSE
			IF  ( dirflg )  THEN 
			    IF ( shift )  THEN
			        CALL G2T_GTEXT(lunb, 19, wd_2, woo, ier)
			      ELSE
			        CALL G2T_GTEXT(lunb, 16, wd_2, woo, ier)
			    END IF
C
			    IF ( spdflg ) THEN
				CALL G2T_GTEXT ( lunb,11,str,ooo,ier )
			      ELSE
				ooo = '.'
			    END IF
C
			    CALL ST_LSTR ( woo, iwoo, ier )
			    CALL ST_LSTR ( ooo, iooo, ier )
			    ooo = woo ( : iwoo ) // ooo ( : iooo )
			  ELSE
			   CALL G2T_GTEXT ( lunb, 20, str, ooo, ier )
			END IF
		    END IF
		  ELSE IF ( max_2  .lt. max_1 .and. min_2 .lt. min_1 ) 
     +			    THEN
		    IF ( ktype .eq. 1 )  THEN
			IF ( min_2 .eq. 0 .or. max_2 .eq. 0 )  THEN
			    CALL G2T_GTEXT ( lunb, 56, str, ooo, ier )
			  ELSE
			    CALL G2T_GTEXT ( lunb, 45, str, ooo, ier )
			END IF
		      ELSE
			IF ( max_2 .le. LGHTS )  THEN
			    str ( 1 ) = ' '
			    CALL ST_INCH ( LGHTS, str ( 2 ), ier )
			    IF ( max_1 .le. LGHTS )  THEN
				CALL ST_INCH ( LGHTS, str ( 2 ), ier )
				CALL G2T_GTEXT (lunb, 30, str, ooo, ier)
			      ELSE
				CALL G2T_GTEXT (lunb, 28, str, ooo, ier)
			    END IF
			  ELSE
			    IF  ( dirflg )  THEN 
				IF ( shift )  THEN
			          CALL G2T_GTEXT (lunb,19,wd_2,woo,ier)
				ELSE
				  CALL G2T_GTEXT (lunb,17,wd_2,woo,ier)
				END IF
C
				IF ( spdflg ) THEN
			          CALL G2T_GTEXT ( lunb,11,str,ooo,ier )
				 ELSE
				  ooo = '.'
				END IF
C
				CALL ST_LSTR ( woo, iwoo, ier )
				CALL ST_LSTR ( ooo, iooo, ier )
				ooo = woo ( : iwoo ) // ooo ( : iooo )
			      ELSE
				CALL G2T_GTEXT (lunb, 25, str, ooo, ier)
			    END IF
			END IF
		    END IF
		  ELSE
		    IF ( ktype .eq. 1 )  THEN
			CALL G2T_GTEXT ( lunb, 48, str, ooo, ier )
		      ELSE
			IF ( max_2 .le. LGHTS )  THEN
			    str ( 1 ) = ' '
			    CALL ST_INCH ( LGHTS, str ( 2 ), ier )
			    CALL G2T_GTEXT ( lunb, 30, str, ooo, ier )
			  ELSE
			    IF  ( dirflg )  THEN 
				IF ( shift )  THEN
				    CALL G2T_GTEXT(lunb,19,wd_2,woo,ier)
			          ELSE
				    CALL G2T_GTEXT(lunb,18,wd_2,woo,ier)
				END IF
C
				IF ( spdflg ) THEN
			          CALL G2T_GTEXT ( lunb,11,str,ooo,ier )
				 ELSE
				  ooo = '.'
				END IF
C
				CALL ST_LSTR ( woo, iwoo, ier )
				CALL ST_LSTR ( ooo, iooo, ier )
				ooo = woo ( : iwoo ) // ooo ( : iooo )
			      ELSE
				CALL G2T_GTEXT (lunb, 32, str, ooo, ier)
			    END IF
			END IF
		    END IF
		END IF
	    END IF
C
	    IF ( end .ne. ' ' .and. .not. eflag_d ( ktype, 2 ) )  THEN
		CALL ST_LSTR ( ooo, lo, ier )
		CALL ST_LSTR ( end, len, ier )
		ooo = ooo ( : lo - 1 ) // end ( : len )
	    END IF
	    CALL G2T_APPTXT ( ktype, nt, ooo, ier )
	    CALL G2T_APPEX ( ktype, lunb, nt, itrnd, end, ier )
	END IF
C*
	RETURN
	END 
