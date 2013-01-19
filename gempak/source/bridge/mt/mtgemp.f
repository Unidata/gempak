        SUBROUTINE MT_GEMP ( lunf, ihhmm, nparm, parms, cprms, imnem,
     +			     numprm, saoflg, iret )
C************************************************************************
C* MT_GEMP                                                              *
C*                                                                      *
C* This subroutine retrievs interface-stored data, converts it into     *
C* GEMPAK output, and then writes the GEMPAK output to the GEMPAK       *
C* output file.                                                         *
C* 								        *
C* MT_GEMP ( LUNF, IHHMM, NPARM, PARMS, CPRMS, IMNEM, NUMPRM, SAOFLG,   *
C*           IRET )                                                     *
C*								        *
C* Input parameters:                                                    *
C*      LUNF           INTEGER        GEMPAK file unit number           *
C*	IHHMM	       INTEGER	      Station time (HHMM)               *
C*      NPARM          INTEGER        Number of parms in packing table  *
C*      PARMS (*)      CHAR*          List of parms in packing table    *
C*	CPRMS (*)      CHAR*          GEMPAK parms chosen for output    *
C*      IMNEM (*)      INTEGER        Subscript mappings, data to GEMPAK*
C*      NUMPRM         INTEGER        Count of chosen GEMPAK parameters *
C*	SAOFLG	       LOGICAL	      Flag for SAOs                     *
C*	RIVALS (*)     REAL	      Interface array for real values   *
C*	CIVALS (*)     CHAR*	      Interface array for char. values  *
C*								        *
C* Output parameters:						        *
C*      IRET           INTEGER        Return code                       *
C*                                      0 = normal return               *
C*                                      1 = problems                    *
C*                                                                      *
C**                                                                     *
C* Log:							                *
C* D. Kidwell/NCEP	 3/96			           	        *
C* D. Kidwell/NCEP	 9/96	Deleted Fahrenheit temperatures         *
C* K. Tyle/GSC	 	 1/97	Add irtarr array as input; use new 	*
C*				packing table; bypass IF library	*
C* D. Kidwell/NCEP	 4/98	New interface; include SAOs             *
C* D. Kidwell/NCEP	 5/98	Added sky cover prioritization, ceiling *
C* D. Kidwell/NCEP	 1/99	Added check for 1-hour precip           *
C* D. Kidwell/NCEP	 9/02	MT_GMSK -> BR_GMSK (with added args.)   *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
        INCLUDE 	'mtcmn.cmn'
C*
        CHARACTER*(*)  	parms (*), cprms (*)
	INTEGER		imnem (*)
	LOGICAL 	saoflg
C*
	REAL		rdata (MMPARM), sky (3), MT_CEIL, cmtn (6)
	CHARACTER	wchr*32
	LOGICAL		skycvr
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
        iret = 0
C
C*      Initialize GEMPAK output array.
C
        DO i = 1, MMPARM
            rdata ( i ) = RMISSD
        END DO
	ceil = RMISSD
	skycvr = .not. ( ERMISS ( rivals ( irnsky ) ) ) 
C
        DO  k = 1, numprm
C
C*          Check for the parameter in the list.
C
            CALL ST_FIND  ( cprms ( k ), parms, nparm, ilc, ier )
C
            IF ( ilc .eq. 0 )  THEN
                CALL DC_WLOG  ( 2, 'DCMETR', -3, cprms(k), ierr )
              ELSE   
C
C*              Retrieve the requested data and store it into the
C*		GEMPAK array.
C
                IF ( ( imnem ( k ) .ne. 0 ) .and.
     +		     ( cprms ( k ) .ne. 'WNUM' ) ) THEN
C
C*		    Store real value from interface to GEMPAK.
C
                    rdata ( ilc ) = rivals ( imnem ( k ) )
C
C*                  Check for parameters which require special handling.
C
		    IF ( cprms ( k ) .eq. 'P03C' ) THEN
C
C*                      Convert pressure tendency information to 
C*		        pressure change in mb.
C
			rdata ( ilc ) = PR_P03C ( rivals ( irp03d ) )
		      ELSE IF ( cprms ( k ) .eq. 'CEIL' ) THEN
			rdata ( ilc ) = ceil
		    END IF
C
C*		    The following parameter checks apply to METAR
C*		    reports only.
C
		    IF ( .not. saoflg ) THEN
                        IF ( cprms ( k ) .eq. 'DRCT' ) THEN
C
C*			    Replace variable wind direction interface
C*			    value with missing.
C
                            IF ( rdata ( ilc ) .eq. -99. )
     +                           rdata ( ilc ) = RMISSD
		          ELSE IF ( cprms ( k ) .eq. 'ALTI' ) THEN
C
C*			    Convert altimeter in mb to altimeter in 
C*			    inches if necessary.
C
			    IF ( ERMISS ( rdata ( ilc ) ) )
     +			     rdata ( ilc ) = PR_ALTI ( rivals (iraltm) )
	   	          ELSE IF ( cprms ( k ) .eq. 'SKNT' ) THEN
C
C*		            Convert wind speed units from m/sec to knots
C*			    if necessary.
C
			    IF ( ERMISS ( rdata ( ilc ) ) ) 
     +			     rdata ( ilc ) = PR_MSKN ( rivals (irsped) )
C
	   	          ELSE IF ( cprms ( k ) .eq. 'GUST' ) THEN
			    IF ( ERMISS ( rdata ( ilc ) ) ) 
     +			     rdata ( ilc ) = PR_MSKN ( rivals (irgums) )
		          ELSE IF ( cprms ( k ) .eq. 'VSBY' ) THEN
C
C*		            Convert units of visibility from kilometers
C*			    to statute miles if necessary.
C
			    IF ( ERMISS ( rdata ( ilc ) ) ) THEN
			        v1 = PR_HGKM ( rivals ( irvsbk (1) ) )
			        v2 = PR_HGMF ( v1 )
			        rdata ( ilc ) = PR_HGFS ( v2 )
			      ELSE IF ( rdata ( ilc ) .lt. .26 ) THEN
C
C*			        Store visibility 'M1/4SM' as zero.
C
			        IF ( rivals ( irvsfl ( 1 ) ) .eq. 0. )
     +				     rdata ( ilc ) = 0.
			    END IF
                          ELSE IF ( ( cprms ( k ) .eq. 'P03I' ) .or.
     +			            ( cprms ( k ) .eq. 'P06I' ) .or.
     +			            ( cprms ( k ) .eq. 'P01I' ) .or.
     +		                    ( cprms ( k ) .eq. 'P24I' ) ) THEN
C
C*                          Replace trace or indeterminable precip
C*			    interface value with correct GEMPAK value.
C
                            IF ( rdata ( ilc ) .eq. -1. ) THEN
                                rdata ( ilc ) = 0.
			      ELSE IF ( rdata ( ilc ) .eq. -99. ) THEN
                                rdata ( ilc ) = RMISSD
			    END IF
			  ELSE IF ( ( cprms ( k ) (1:3) .eq. 'CHC' ) 
     +                              .and. skycvr ) THEN
C
C*			    Process sky condition parameters.
C
			    CALL ST_INTG ( cprms (k) (4:4), layer, ier )
			    sky ( layer ) = rdata ( ilc )
			    IF ( layer .eq. 3 ) THEN
C
C*			        Choose 3 priority layers if more than 
C*				three layers were reported.
C
				nsky = NINT ( rivals ( irnsky ) )
				IF ( nsky .gt. 3 ) THEN
				    DO i = 1, nsky
					cmtn (i) = rivals ( ircmtn (i) )
				    END DO
				    CALL BR_GMSK ( nsky, cmtn, sky, ier)
				END IF
C
				isky = ilc - 3
				DO i = 1, 3
				    isky = isky + 1
C
C*			            Remove convective cloud flag value 
C*			            if it is present.
C
			            IF ( sky ( i ) .gt. 30000. ) THEN
			                sky ( i ) = sky ( i ) - 30000.
			              ELSE IF ( sky(i) .gt. 20000.) THEN
			                sky ( i ) = sky ( i ) - 20000.
			            END IF
C
C*				    Remove '-' flag for zero cloud hght.
C
				    IF ( .not. ERMISS ( sky (i) ) ) THEN
				        rdata ( isky ) = ABS ( sky (i) )
				      ELSE
					rdata ( isky ) = RMISSD 
				    END IF
				END DO
C
C*				Get ceiling for METAR report.
C
				ceil = MT_CEIL ( sky ( 1 ), sky ( 2 ),
     +						 sky ( 3 ) )
			    END IF
			END IF
		      ELSE
			IF ( cprms ( k ) .eq. 'CHC3' ) THEN
C
C*			    Get ceiling for SAO report.
C
			    ceil = MT_CEIL ( rdata ( ilc - 2 ),
     +				   rdata ( ilc - 1 ), rdata ( ilc ) ) 
			END IF
                    END IF
		  ELSE IF ( imnem ( k ) .ne. 0 ) THEN
C
C*		    Build the weather code from the three weather
C*		    codes.
C
		    IF ( rivals ( irnpwx ) .gt. 0. ) THEN
		        nwea = INT ( rivals ( irnpwx ) )
		        CALL ST_LSTC ( civals ( icwcod (1) ), nwea,
     +				       ' ', wchr, ier ) 
		        CALL ST_RMBL ( wchr, wchr, len, ier )
		        IF ( wchr .ne. ' ' ) THEN
		            IF ( .not. saoflg ) THEN
      		                rdata ( ilc ) = PT_WNMT ( wchr )
		              ELSE
			        rdata ( ilc ) = PT_WNUM ( wchr )
			    END IF
		        END IF
		    END IF
                END IF
            END IF
C
        END DO
C
C*	Write the decoded report data to GEMPAK land surface data file.
C
        CALL SF_WDAT ( lunf, ihhmm, rdata, ier )
C
        IF ( ier .ne. 0 ) THEN
C
C*          Write an error message if there was a file write error.
C
            CALL DC_WLOG ( 2, 'SF', ier, ' ', ierr )
            iret = 1
            RETURN
        END IF
C*
	RETURN
	END
