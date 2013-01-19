	SUBROUTINE GH_SWLN ( ise, ine, inw, isw, alat, alon, nstrm, 
     +                       hvhur, iret)
C************************************************************************
C* GH_SWLN								*
C*									*
C* This subroutine calculates and plots the radii of the tropical       *
C* storm quadrants from one quadrant to the next for tropical storm     *
C* (34KT) and hurricane (64KT) force winds.				*
C*									*
C* GH_SWLN ( ISE, INE, INW, ISW, ALAT, ALON, NSTRM, HVHUR, IRET) 	*
C*									*
C* Input parameters:							*
C*	ISE (2, nstrm)	INTEGER		Southeast wind distance (nm)	*
C*	INE (2, nstrm)	INTEGER		Northeast wind distance (nm)	*
C*	INW (2, nstrm)	INTEGER		Northwest wind distance (nm)	*
C*	ISW (2, nstrm)	INTEGER		Southwest wind distance (nm)	*
C*      ALAT (nstrm)	REAL		Array of initial latitudes	*
C*      ALON (nstrm)	REAL		Array of initial longitudes	*
C*	NSTRM		INTEGER		Total number of files read	*
C*	HVHUR		LOGICAL		One report of hurricane winds	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 2/01   					*
C* A. Hardy/GSC		 6/01 	Added color tag parameter		*
C* D. Kidwell/NCEP	 9/01	Rewrote for interpoln & smooth; cleanup *
C* D. Kidwell/NCEP	 2/02	Removed unused variable idrop           *
C* D. Kidwell/NCEP	 4/02	Tapered down to 0 at end                *
C* D. Kidwell/NCEP	 5/03	Added argument iptsm to GG_TCSM call    *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( MXINTP = 1000 )
C*
 	INTEGER         ise(2,*), ine(2,*), inw(2,*), isw(2,*)
        REAL		alat(*), alon(*)
        LOGICAL         hvhur
C*
        INTEGER		istart (20), iend (20)
        REAL		blat (364),
     +			blon (364), 
     +			dlat (MXINTP), dlon (MXINTP), angle (4),
     +          	dist (5), dse (2,MXINTP), dne (2,MXINTP),
     +		  	dnw (2,MXINTP), dsw (2,MXINTP)
        LOGICAL		blob
        CHARACTER       coltag*33
C-----------------------------------------------------------------------
        iret  = 0
        iang1 = 0
        iang2 = 90
        angle (1) = 45
        angle (2) = 135
        angle (3) = 225
        angle (4) = 315
C
    	nsmth = 50
    	iptsm = 5
C
C*      Set loop for tropical storm and/or hurricane.
C
        IF ( hvhur ) THEN
            ifini = 2
          ELSE
            ifini = 1
        END IF
C
        DO kk = 1, ifini
            IF ( kk .eq. 1 ) THEN
                icol   = 18
                coltag = 's_ts_wind'
		iradif = 9
            END IF
            IF ( kk .eq. 2 ) THEN
                icol   = 2
                coltag = 's_hur_wind'
		iradif = 6
            END IF
C
C*          Determine the starting and end points for each "blob".
C
            DO mb = 1, 20
                istart ( mb ) = IMISSD
                iend   ( mb ) = IMISSD
            END DO
C
            iblob = 1
            blob  = .false.
            DO ik = 1, nstrm
                isum = ise ( kk, ik ) +  ine ( kk, ik ) +
     +                 inw ( kk, ik ) +  isw ( kk, ik )
                IF ( ( isum .ne. 0 ) .and. ( .not. blob ) ) THEN
		    istart (iblob) = ik
                    blob = .true.
                END IF
                IF ( ( isum .eq. 0 ) .and. blob ) THEN
                    iend ( iblob ) = ik
                    blob  = .false.
                    iblob = iblob + 1
                END IF
            END DO
C
            IF ( ( istart ( iblob ) .ne. IMISSD ) .and.
     +           ( iend ( iblob ) .eq. IMISSD ) ) THEN
	        iend ( iblob ) = nstrm
	      ELSE
                iblob = iblob - 1
            END IF
C
C*	    Loop over the discrete "blobs" in the swath.
C
            DO mm = 1, iblob
                ib  = istart (mm)
                ie  = iend (mm)
C
C*		Interpolate track points and radii for a smooth plot.
C
		ipt = 1
		DO ip = ib, ie
		    dlat ( ipt ) = alat ( ip )
		    dlon ( ipt ) = alon ( ip )
		    dse ( kk, ipt ) = FLOAT ( ise ( kk, ip ) )
		    dne ( kk, ipt ) = FLOAT ( ine ( kk, ip ) )
		    dnw ( kk, ipt ) = FLOAT ( inw ( kk, ip ) )
		    dsw ( kk, ipt ) = FLOAT ( isw ( kk, ip ) )
		    IF ( ip .lt. ie ) THEN
	                CALL CLO_DIST ( alat ( ip ), alon ( ip ), 1,
     +				       alat ( ip + 1 ), alon ( ip + 1 ),
     +			               dd, ier )
		        IF ( dd .gt. 5000. ) THEN
                            radmin = AMIN0 ( ise (kk,ip), ine (kk,ip),
     +                                       inw (kk,ip), isw (kk,ip),
     +                                       ise(kk,ip+1), ine(kk,ip+1),
     +                                       inw(kk,ip+1), isw(kk,ip+1))
			    radavg = NINT ( FLOAT ( ise (kk,ip) + 
     +				     ine (kk,ip) +
     +				     inw (kk,ip) + isw (kk,ip) + 
     +				     ise(kk,ip+1) + ine(kk,ip+1) +
     +				     inw(kk,ip+1) + isw(kk,ip+1) )  
     +				     / 8. )
			    IF ( radavg .lt. 6. ) radavg = 6.
			    space = PR_HGNM ( radavg )
	                    intrp = NINT ( dd / space + .6 )
			    incr  = NINT ( radavg - radmin ) / iradif
			    intrp = intrp + incr
			    intvl = intrp + 1
		            delta = dd / FLOAT ( intvl )
		            CALL CLO_DIRECT ( alat (ip+1), alon (ip+1),
     +				           alat ( ip ), alon ( ip ),
     +					   ddir, ier )
			    ddist = delta
			    DO ii = 1, intrp
			        ipt = ipt + 1
			        CALL CLO_DLTLN ( alat (ip), alon (ip),
     +					        ddist, ddir, dlat (ipt),
     +					        dlon (ipt), ier )
			        wt2 = FLOAT ( ii ) / FLOAT ( intvl ) 
			        wt1 = 1. - wt2
				dse ( kk, ipt ) = ise ( kk,ip ) * wt1
     +					        + ise ( kk, ip+1 ) * wt2
				dne ( kk, ipt ) = ine ( kk,ip ) * wt1
     +					        + ine ( kk, ip+1 ) * wt2
				dnw ( kk, ipt ) = inw ( kk,ip ) * wt1
     +					        + inw ( kk, ip+1 ) * wt2
				dsw ( kk, ipt ) = isw ( kk,ip ) * wt1
     +					        + isw ( kk, ip+1 ) * wt2
   			        ddist = ddist + delta
			    END DO
			END IF
		        ipt = ipt + 1    
		    END IF
		END DO
C
C*              Calculate tropical storm and hurricane force wind zones
C*		within a "blob".
C
		rdmin = 0.
                DO ip = 1, ipt
C
                    dist (1) = AMAX1 ( dse ( kk, ip ), rdmin )
                    dist (2) = AMAX1 ( dne ( kk, ip ), rdmin )
                    dist (3) = AMAX1 ( dnw ( kk, ip ), rdmin )
                    dist (4) = AMAX1 ( dsw ( kk, ip ), rdmin )
                    dist (5) = dist (1)
C
C*	 	    Loop through each quadrant of the polygon.  The
C*                  radii are currently calculated to generate a
C*                  smooth contour when sweeping from one quadrant
C*                  to the next in 1 degree azimuthal increments.  
C
                    icnt  = 0
                    DO ii = 1, 4
                        DO iangle  = iang1, iang2
                            icnt   = icnt + 1
			    theta  = FLOAT ( iangle )
			    rtheta = DTR * theta
                            rad    = COS ( rtheta ) * COS ( rtheta )
     +                               * dist ( ii ) 
     +                               + SIN ( rtheta ) * SIN ( rtheta )
     +                               * dist ( ii + 1 )
C
			    rtheta = DTR * ( theta + 270.0 + angle(ii) )
                            y1     = rad * SIN ( rtheta ) / 60.0
                            x1     = (-rad) * COS ( rtheta ) / 60.0
     +                               / COS ( DTR * dlat ( ip ) )
C
                            blat (icnt) = dlat (ip) + y1
                            blon (icnt) = dlon (ip) - x1
                        END DO
                    END DO
C
		    CALL GG_TCSM ( nsmth, icnt, iptsm, blat, blon, ier )
C
C*		    Draw and fill the polygon.
C
 		    CALL GH_SWDF ( coltag, icol, icnt, blat, blon, ier )
		END DO
            END DO
        END DO
C*
        RETURN
        END
