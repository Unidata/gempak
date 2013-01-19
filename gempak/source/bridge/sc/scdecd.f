	SUBROUTINE SC_DECD ( report, lenr, irpnt, iret )
C************************************************************************
C* SC_DECD                                                              *
C*                                                                      *
C* This subroutine decodes a single Supplemental Climatological Data    *
C* report (SCD).						        *
C*                                                                      *
C* Except for cloud base height, code values for SCD cloud data as      *
C* defined in NWS OH-7 are the same as the WMO code table values        *
C* referenced below.  For cloud base height scdbas, SCD code table 3-10 *
C* given in NWS OH-7 uses units of feet, while WMO code table 1600      *
C* referenced by GEMPAK parameter CBAS uses units of meters.  Code      *
C* values are roughly the same for both tables.				*
C* 								        *
C* SC_DECD  ( REPORT, LENR, IRPNT, IRET )			        *
C*								        *
C* Input parameters:						        *
C*	REPORT		CHAR*		SCD report  		        *
C*	LENR		INTEGER		Report length		        *
C*	IRPNT		INTEGER		Pointer after time field        *
C*                                                                      *
C* Output parameters:						        *
C*	RIVALS(IRTDXC)	REAL	24 hour maximum temperature (degrees C) *
C*	RIVALS(IRTDMC)	REAL	24 hour minimum temperature (degrees C) *
C*	RIVALS(IRP06I)	REAL	6 hour precipitation (inches)           *
C*	RIVALS(IRP24I)	REAL	24 hour precipitation (inches)          *
C*	RIVALS(IRSNOW)	REAL	Snow depth on the ground (inches)       *
C*	RIVALS(IRSNEW)	REAL	Amount of fresh snow (inches)           *
C*	RIVALS(IRWEQS)	REAL	Water eqv. of snow on ground (inches)   *
C*	RIVALS(IRMSUN)  REAL	Duration of sunshine (minutes)          *
C*	RIVALS(IRCTYL)	REAL	Low cloud type (WMO Code 0513)          *
C*	RIVALS(IRCTYM)	REAL	Middle cloud type (WMO Code 0515)       *
C*	RIVALS(IRCTYH)	REAL	High cloud type (WMO Code 0509)         *
C*	RIVALS(IRCFRT)	REAL	Total cloud amount (WMO Code 2700)      *
C*	RIVALS(IRCFRL)	REAL	Low or middle cloud amount (WMO 2700)   *
C*	RIVALS(IRCBAS)	REAL	Base height of lowest cloud (WMO 1600)  *
C*	CIVALS(ICWCOD)	CHAR*	Present weather string array            *
C*	IRET		INTEGER		Return code                     *
C*					  0 = Normal return             *
C*					 -1 = Zero length - no report   *
C*					 -2 = No decodable fields       *
C*								        *
C**								        *
C* Log:								        *
C* D. Kidwell/NCEP	 3/97	Based on MT_DECD                        *
C* A. Hardy/GSC         12/97   Added interface variables               *
C* D. Kidwell/NCEP	 6/98	Added CIVALS to prologue; cleaned up    *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE         'sccmn.cmn'
C*
	CHARACTER*(*) 	report
C*
	CHARACTER*40	strarr ( 50 ), scdgp
	CHARACTER*9	strwea ( 3 ), clouds
	CHARACTER	stcltp*3, stchr*1, fauxwx*2
	REAL 		cltp ( 3 )
	LOGICAL  	errflg
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for zero length report.
C
	IF ( irpnt .gt. lenr ) THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Initialize decoded variables to missing.
C
	DO i = 1, 3
	    strwea ( i ) = ' '
	    cltp   ( i ) = RMISSD
	END DO
C
	snowdp = RMISSD
	snownu = RMISSD
	tmx24  = RMISSD
	tmn24  = RMISSD
	sunshn = RMISSD
	prec24 = RMISSD 
	prec6  = RMISSD 
	h2oeqv = RMISSD
	scdfrt = RMISSD
	scdfrl = RMISSD
	scdbas = RMISSD
	cltp (1) = RMISSD
	cltp (2) = RMISSD
	cltp (3) = RMISSD
C
C*	Break down the report into an array; SCD groups are separated
C*	by blanks in the report.
C
 	nexp = 50
	CALL ST_CLST ( report ( irpnt:lenr ), ' ', ' ', nexp, strarr,
     +	       	       num, kret)
C			   
C*	There are 'num' substrings to decode here.
C
	IF ( num .le. 0 ) THEN
	    iret = -1
	    RETURN
	END IF
C
C*	First check for 'NIL'.
C
	IF ( strarr ( 1 ) ( 1:3 ) .eq. 'NIL' ) THEN
	    iret = -2
	    RETURN
	END IF
C
C*	Get count of "weather" and/or "obstructions to vision" groups.
C*	These groups should be terminated by a '/'.  At most 3 such
C*	groups will be decoded.
C
	maxwea = 0
	numck = MIN ( num, 3 ) 
	stchr = strarr ( 1 ) ( 1:1 )
	CALL ST_ALNM ( stchr, ityp, kret ) 
	IF ( ( ityp .eq. 2 ) .or. ( stchr .eq. '-' ) .or.
     +	     ( stchr .eq. '+' ) ) THEN
C
C*	    The first group may be a weather group.
C
	    i = 1
	    DO WHILE ( i .le. numck ) 
	        CALL ST_LSTR ( strarr ( i ), lens, kret )
	        IF  ( strarr ( i ) ( lens:lens ) .eq. '/' ) THEN 
		    IF ( lens .gt. 1 )  THEN
		        CALL ST_ALNM ( strarr ( i ) ( lens-1:lens-1 ), 
     +				       ityp, kret )
		        IF ( ityp .eq. 2 ) maxwea = i
		      ELSE IF ( i .gt. 1 ) THEN
		        CALL ST_LSTR ( strarr ( i - 1 ), lens, kret )
			CALL ST_ALNM ( strarr ( i - 1 ) ( lens:lens ),
     +				       ityp, kret )
			IF ( ityp .eq. 2 ) maxwea = i - 1
		    END IF
		    i = numck + 1
		  ELSE
		    i = i + 1
		END IF
	    END DO
C
	    IF ( maxwea .eq. 0 ) THEN
C
C*	    	No '/' was found, so recheck first groups.
C
		i = 1
		DO WHILE ( i .le. numck )
		    CALL ST_LSTR ( strarr ( i ) , lens, kret )
		    ityp = 0
		    IF ( lens .gt. 1 ) CALL ST_ALNM ( strarr (i) (2:2), 
     +					              ityp, kret ) 
		    IF ( ityp .eq. 2 ) THEN
			maxwea = i 
			i = i + 1
		      ELSE
			i = numck + 1
		    END IF
		END DO
C
		IF ( maxwea .gt. 0 ) THEN
C
C*                  If the weather conditions and cloud reports are 
C* 		    not separated by a blank, they will be split up 
C*                  into new arrays. Otherwise, the cloud report is
C*                  not printed. If there are other groups to be
C*		    decoded, they are moved down one in the strarr
C*		    array.
C
		    CALL ST_LSTR ( strarr ( maxwea ), lens, kret )
		    DO j = 1, lens
		        IF( ( strarr ( maxwea ) ( j:j ) .eq. '/' ) .and.
     +	                ( strarr( maxwea ) ( j+1:j+1 ) .eq. '8' ) ) THEN
	                    clouds = strarr ( maxwea ) ( j+1:lens )
                            IF ( num .ge. ( maxwea + 1 ) ) THEN
		                DO i = num, maxwea + 1, -1
		                    strarr ( i + 1 ) = strarr ( i )
		                    IF ( i .eq. ( maxwea + 1 ) ) THEN
	                                strarr ( maxwea + 1 ) = clouds
		 	                num = num + 1
	                            END IF
                                END DO
		              ELSE
	                        strarr ( maxwea + 1 ) = clouds
			        num = num + 1
	                    END IF
                            strarr ( maxwea ) = 
     +					  strarr ( maxwea ) ( 1:j-1 )
		        END IF
		    END DO
C
C*		    Append a '/' to the last weather group.
C
		    CALL ST_LSTR ( strarr ( maxwea ), lens, kret )
		    strarr ( maxwea ) ( lens+1:lens+2 ) = '/   '
		END IF
	    END IF
C
C*          These statements allow the last weather group
C*	    to be stored if there is a space between the
C*          weather group and its' terminating '/'.
C
	    CALL ST_LSTR ( strarr ( maxwea ), lens, kret )
	    IF ( strarr ( maxwea ) ( lens:lens ) .ne. '/ ' )
     +           strarr ( maxwea ) ( lens+1:lens+2 ) = '/ ' 
	END IF
C
C*	Loop on SCD groups.
C
	icnt = 1
	errflg = .false.
C
	DO WHILE ( icnt .le. num )
C
	    ier = 0
	    scdgp = strarr ( icnt )
C
	    CALL ST_LSTR ( scdgp, lengp, kret )
C
C*	    First process weather group(s), if any.
C
	    IF ( icnt .le. maxwea ) THEN
                IF ( icnt .eq. maxwea ) lengp = lengp -1
      	        CALL SC_WEA3 ( scdgp ( :lengp ), icnt, strwea, ier )
C
C*	        Store weather group decoded data.
C
	        DO  i = 1, maxwea 
	            civals ( icwcod ( i ) ) = strwea ( i )
	        END DO
C
		IF ( ier .gt. 0 ) THEN
		    fauxwx = scdgp ( 1:2 )
C
C*		    Check for non-weather field RMK, RMKS, COR or NIL.
C
		    IF ( ( fauxwx .eq. 'CO' ) .or. ( fauxwx .eq. 'RM')
     +                   .or. ( fauxwx .eq. 'NI' ) ) ier = 0
		END IF
	      ELSE
C
C*	        Next decode remarks.
C
	        IF ( lengp .eq. 5 ) THEN
C
		    IF ( scdgp ( 1:2 ) .eq. '4/' ) THEN
C
C*		        Decode snow depth on ground.
C
		        IF ( ERMISS ( snowdp ) ) THEN 
     	                    CALL ST_CRNM ( scdgp ( 3:5 ), snowdp, ier )
                            IF ( ier .eq. 0 ) THEN
		                 rivals ( irsnow ) = snowdp
			      ELSE
			         ier = 20
			    END IF
                        END IF 
C
		      ELSE IF ( scdgp ( 1:1 ) .eq. '7' ) THEN
C
C*		        Decode and store 24-hour precipitation amount.
C
		        IF ( ERMISS ( prec24 ) ) THEN
     	                    CALL SC_PRRM ( scdgp, prec24, ier )
                            IF ( ier .eq. 0 ) THEN 
			        rivals ( irp24i ) = prec24
			      ELSE
				ier = 23
			    END IF
                        END IF
C
		      ELSE IF ( scdgp ( 1:1 ) .eq. '6' ) THEN
C
C*		        Decode and store 6-hour precipitation amount.
C
		        IF ( ERMISS ( prec6 ) ) THEN 
     	                    CALL SC_PRRM ( scdgp, prec6, ier )
                            IF ( ier .eq. 0 ) THEN
				rivals ( irp06i )= prec6
			      ELSE
				ier = 19
			    END IF
                        END IF

C
	  	      ELSE IF ( scdgp ( 1:2 ) .eq. '98' ) THEN
C
C*		        Decode and store duration of sunshine.
C
	 	        IF ( ERMISS ( sunshn ) ) THEN
         	            CALL ST_CRNM ( scdgp ( 3:5 ), sunshn, ier )
                            IF ( ier .eq. 0 )  THEN
    				IF ( scdgp ( 3:5 ) .eq. '///' ) THEN
                                    ier = 25
                                  ELSE
                                    ier = 0
				    rivals ( irmsun ) = sunshn
				END IF
                            END IF
			END IF
C
		    END IF
C
	          ELSE IF ( lengp .eq. 6 ) THEN
C
		    IF ( scdgp ( 1:3 ) .eq. '933' ) THEN
C
C*		        Decode and store water equivalent of snow 
C*			on ground.
C
		        IF ( ERMISS ( h2oeqv ) ) THEN
		            CALL ST_CRNM ( scdgp ( 4:6 ), h2oeqv, ier )
                            IF ( ier .eq. 0 ) THEN
                                h2oeqv = h2oeqv * .1
			        rivals ( irweqs ) = h2oeqv
                              ELSE
                                ier = 28
                            END IF
		        END IF
C 
	  	      ELSE IF ( scdgp ( 1:3 ) .eq. '931' ) THEN
C
C*		        Decode and store depth of new snow; 0 is 
C*			a trace.
C
		        IF ( ERMISS ( snownu ) ) THEN
		            CALL ST_CRNM ( scdgp ( 4:6 ), snownu, ier )
		            IF ( ier .eq. 0 ) THEN
			        snownu = snownu * .1
			        rivals ( irsnew ) = snownu
			      ELSE
				ier = 29
                            END IF
		        END IF
		    END IF
C
	          ELSE IF ( lengp .eq. 7 ) THEN
C
	   	    IF ( scdgp ( 1:1 ) .eq. '8' ) THEN
C
C*		        Decode and store cloud group. 
C
		 	IF ( ERMISS ( scdfrt ) ) THEN
			    IF  ( scdgp ( 2:2 ) .eq. '9' ) THEN
			        scdfrt = 9.
                                rivals ( ircfrt ) = scdfrt
			      ELSE IF ( scdgp ( 2:2 ) .ne. '/' ) THEN
				CALL ST_CRNM ( scdgp ( 2:2 ), 
     +                                         scdfrt, ier1)
				IF ( scdgp ( 3:3 ) .ne. '/' ) THEN
				    CALL ST_CRNM ( scdgp ( 3:3 ),
     +					  	   scdfrl, ier2 )
				  ELSE
				    ier2 = 0
				END IF
C
				IF ( scdgp ( 5:5 ) .ne. '/' ) THEN
				    CALL ST_CRNM ( scdgp ( 5:5 ),
     +						   scdbas, ier3 )
				  ELSE
				    ier3 = 0
				END IF
C
				stcltp = scdgp ( 4:4 ) // scdgp ( 6:7 )
				CALL SC_CLTP ( stcltp, cltp, ier )
				IF ( ( ier1+ier2+ier3 ) .lt. 0 ) THEN
				    ier = 14
				  ELSE
				    rivals ( irctyl ) = cltp (1) 
				    rivals ( irctym ) = cltp (2) 
				    rivals ( irctyh ) = cltp (3) 
				END IF
C
C*				If we have only high clouds, change
C*				cloud base height code from 0 to 9.
C
				IF ( ( scdgp ( 3:6 ) .eq. '0000' ) .and.
     +				     ( cltp ( 3 ) .gt. 0 ) .and.
     +				     ( scdfrt .gt. 0 ) )  THEN
				    scdbas = 9.
				END IF
			        rivals ( ircfrt ) = scdfrt
			        rivals ( ircfrl ) = scdfrl
			        rivals ( ircbas ) = scdbas
			    END IF
		        END IF
		    END IF
C
	          ELSE IF ( lengp .eq. 9 ) THEN
C
      		    IF ( scdgp ( 1:1 ) .eq. '4' ) THEN
		        IF ( ( ( scdgp ( 2:2 ) .eq. '0' ) .or.
     +			       ( scdgp ( 2:2 ) .eq. '1' ) ) .and.
     +			     ( ( scdgp ( 6:6 ) .eq. '0' ) .or.
     +			       ( scdgp ( 6:6 ) .eq. '1' ) ) ) THEN
C
C*		            Decode and store 24-hour max and min 
C*			    temperatures.
C
			    IF ( ERMISS ( tmx24 ) ) THEN
      			        CALL SC_TPRM ( scdgp (2:5), tmx24, ier1)
			        rivals ( irtdxc ) = tmx24
			    END IF
			    IF ( ERMISS ( tmn24 ) ) THEN 
     			        CALL SC_TPRM ( scdgp (6:9), tmn24, ier2)
			        rivals ( irtdnc ) = tmn24
			    END IF
                            ier = MAX ( ier1, ier2 )
		        END IF
		    END IF
	        END IF
	    END IF
C
	    IF ( ier .ne. 0 ) THEN
C
C*		An error was found while decoding this group.
C
		errflg = .true.
		CALL DC_WLOG ( 2, 'SC', ier, scdgp ( :lengp ), ierr )
	    END IF
	    icnt = icnt + 1
	END DO
C
C*	We are done with this report, with all valid data decoded.
C*	Write out record if error found.
C
	IF ( errflg ) 
     +       CALL DC_WLOG ( 1, 'SC', 15, report ( :lenr ), ierr )
C*
	RETURN
	END
