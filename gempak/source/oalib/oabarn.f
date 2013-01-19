	SUBROUTINE OA_BARN  ( ngrid, weight, srad, kexy, nstn,
     +			      data, slat, slon, gelat, gelon, coslat,
     +			      cosslt, extrap, gsflag, gpinb, stninb,
     +                        isn, grid, iret )
C************************************************************************
C* OA_BARN								*
C*									*
C* This subroutine performs a single pass of a Barnes analysis.  The	*
C* weighting function used is						*
C*									*
C*	WF = [ EXP ( DIST ** 2 / WEIGHT ) ]				*
C*									*
C* where DIST is the distance from the station to the grid point.	*
C* Locations and distances in this subroutine are now defined in	*
C* latitude/longitude.  Distance between a grid point and a station	*
C* is computed as							*
C*									*
C* DIST ** 2 = [ ( lat (grid) - lat (stn) ) ** 2 +			*
C*	       ( ( lon (grid) - lon (stn) ) * avgcos ) ** 2 ],		*
C*									*
C* where AVGCOS is the average cosine of latitude between grid point	*
C* and station.	If the grid point latitude is too close to a pole,	*
C* a more exact formula is used:					*
C*									*
C* DIST ** 2 = [ ( lat (grid) - lat (stn) ) ** 2 +			*
C*	       2 * ( 1 - cos ( lon (grid) - lon (stn) )			*
C*		 * cos ( lat (grid) ) * cos ( lat (stn) ) ].		*
C*									*
C* ISN is the number of stations used for each grid computed.  Only	*
C* data within the distance [ SQRT (SRAD) ] of a grid point is		*
C* included in the analysis.						*
C*									*
C* EXTRAP controls whether the ordinary Barnes analysis is used.	*
C* If EXTRAP = .FALSE., a check is made to ensure that data is		*
C* absent in no more than three consecutive octals about a grid point.	*
C* If four or more consecutive octals are devoid of data, as might be	*
C* the case at an offshore grid point, the grid point value is returned	*
C* as missing.								*
C*									*
C* OA_BARN ( NGRID, WEIGHT, SRAD, KEXY, NSTN, DATA, SLAT, SLON,		*
C*	     GELAT, GELON, COSLAT, COSSLT, EXTRAP, GSFLAG, GPINB	*
C*	     STNINB, ISN, GRID, IRET )					*
C*									*
C* Input parameters:							*
C*	NGRID		INTEGER		Number of grids			*
C*	WEIGHT		REAL		Weighting factor		*
C*	SRAD		REAL		Search radius in grid coords	*
C*	KEXY		INTEGER		# of points in extend grid	*
C*	NSTN		INTEGER		Number of stations		*
C*	DATA		REAL		Station data			*
C*	 (NGRID,NSTN)							*
C*	SLAT   (NSTN)	REAL		Station latitudes		*
C*	SLON   (NSTN)	REAL		Station longitudes		*
C*	GELAT  (KEXY)	REAL		Grid point latitudes		*
C*	GELON  (KEXY)	REAL		Grid point longitudes		*
C*	COSLAT (KEXY)	REAL		COS (lat) at grid pts		*
C*	COSSLT (NSTN)	REAL		COS (lat) at stations		*
C*	EXTRAP		LOGICAL		Allow extrapolation		*
C*	GSFLAG		LOGICAL		Flag for first guess		*
C*	GPINB  (KEXY)	LOGICAL		Flags for inbound grid points	*
C*	STNINB (NSTN)	LOGICAL		Flag for inbound stations	*
C*									*
C* Output parameters:							*
C*	ISN   (NGRID)	INTEGER		# stations used for grid	*
C*	GRID		REAL		Grid data			*
C*	 (NGRID,KEXY)							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/86						*
C* M. desJardins/GSFC	10/88	Documentation				*
C* K. Brill/GSC		 4/90	Put missing value check in grid update	*
C* J. Nielsen/SUNYA	12/90	Included exact formula for distance,	*
C*				ignored if approximation valid or FAST	*
C* J. Nielsen/SUNYA	12/90	Included option for no extrapolation	*
C* K. Brill/NMC		06/91	Removed FAST option and cleaned up;	*
C*				Improved approximate calculation;	*
C*				Use different Cartesian frame for data	*
C*				extrapolation check			*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C* S. Jacobs/NCEP	 6/98	Added guess flag calling sequence	*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C* J. Wu/SAIC           04/05   Added bounds blocking			*  
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( MINSTN = 3 )
	PARAMETER	( LLDTMX = LLSTFL * MMFILE )
	REAL		data ( NGRID, NSTN )
	REAL		slat (*), slon (*), gelat ( * ), gelon (*),
     +			coslat ( * ), cosslt ( * ), grid ( NGRID, KEXY )
	LOGICAL		extrap, gsflag, gpinb ( * ), stninb ( * )
	INTEGER		isn ( * )
C*
	REAL		sum (LLOAGD), swt (LLOAGD) 
	REAL		gnmlat (LLMXGD), gnmlon (LLMXGD)
	REAL		snmlat (LLDTMX), snmlon (LLDTMX)
	INTEGER		mod8 (10)
	LOGICAL		hit ( 8, LLOAGD )
	LOGICAL		incmpl, intersect
	INCLUDE		'ERMISS.FNC'
	DATA		mod8 /2,3,4,5,6,7,8,1,2,3/
C*
C------------------------------------------------------------------------
	iret = 0
	rtd2 = RTD * RTD
	sr = sqrt(srad)
	isect = 1
C
C*	Check the value entered for the weight.  Compute the reciprocal
C*	to use multiplication instead of division.
C
	IF  ( weight .le. 0. )  THEN
	    www   = 5.051457 * 2. / PI
	    wtfac = 1. /www
	  ELSE
	    wtfac = 1. / weight
	END IF
C
C*      Transform to normalized coordinates for bounds checking.
C	    
        CALL GTRANS ( 'M', 'N', kexy, gelat, gelon, 
     +                          gnmlat, gnmlon, iret )
        CALL GTRANS ( 'M', 'N', nstn, slat, slon, 
     +                          snmlat, snmlon, iret )
C
C*	Loop through all grid points.
C
	DO  ixy = 1, kexy
	    glat   = gelat  ( ixy )
	    glon   = gelon  ( ixy )
	    gnlat   = gnmlat  ( ixy )
	    gnlon   = gnmlon  ( ixy )	    
C
C*          Check if the grid point should be blocked.
C	    
	    IF  ( gpinb ( ixy ) )  THEN
	        DO  igrid = 1, ngrid
	            grid ( igrid, ixy ) = RMISSD
                END DO
	    END IF
C	    
	    IF  ( gpinb ( ixy ) )  THEN
		GO TO 222	    
	    END IF
C
C*	    Initialize the weighted data arrays and the hit array.
C
	    DO  igrid = 1, ngrid
		sum ( igrid ) = 0.
		swt ( igrid ) = 0.
		isn ( igrid ) = 0
	    END DO
	    IF  ( .not. extrap )  THEN
		DO  igrid = 1, ngrid
		    DO  isec = 1, 8
			hit ( isec, igrid ) = .false.
		    END DO
		END DO
	    END IF
C
C*	    Loop through each station.
C
	    DO  is = 1, nstn	        
C
C*              Skip the station if it is inside of the blocking bounds
C	    
		IF  ( stninb ( is ) )  GO TO 111
C
C*		Compute distance and check that it is less than the
C*		search radius.
C
		at = slat (is)
		on = slon (is)
C
C*		Do points span dateline?
C
		IF ( on - glon .gt. 180. ) THEN
		    on = on - 360.
		ELSE IF ( on - glon .lt. -180. ) THEN
		    on = on + 360.
		END IF
C*
		diflon = ABS ( on - glon )
C*
		IF ( ( glat .gt. 90. - sr  .or. glat .lt. sr - 90. )
     +					.and.
     +		   ( diflon .gt. sr .or. diflon .gt. 60. ) ) THEN
C
C*		   Within search radius of pole; need to use exact
C*		   formula.
C
		   dsq = ( at - glat ) ** 2
		   IF ( dsq .le. srad ) dsq = dsq +
     +		           2. * ( 1. - COS ( diflon * DTR ) ) *
     +			   cosslt ( is ) * coslat ( ixy ) * rtd2
C*
		   IF ( ( .not. extrap ) .and. (dsq .le. srad) ) THEN
C
C*			Express station position relative to the grid
C*			point using a coordinate system with (0,0) at
C*			the NP with the grid point at (0, - colatitude).
C*			The location of the station is computed in
C*			this coordinate system and then translated in
C*			+y direction by an amount equal to the
C*			colatitude of the grid point.
C
			IF ( glat .lt. 0 ) THEN
			    rg = 90 + glat
			ELSE
			    rg = 90 - glat
			END IF
C*
			IF ( at .lt. 0 ) THEN
			    rr = 90 + at
			ELSE
			    rr = 90 - at
			END IF
			theta = ( ( on - glon ) - 90. ) * DTR
			on = rr * COS ( theta )
			at = rr * SIN ( theta ) + rg
	 	   END IF
C*
		ELSE
C
C*		     Use the approximate formula.
C
		     avgcos = .5 * ( coslat (ixy) + cosslt (is) )
		     dsq = ( at - glat ) **2 + ( diflon * avgcos ) **2
C
C*		     Adjust lon to approximate Cartesian frame with the
C*		     origin at the grid point.
C
		     IF  ( ( .not. extrap ) .and. ( dsq .le. srad ) )
     +			THEN
			on = ( on - glon ) * avgcos
		   	at = ( at - glat )
		     END IF
		END IF
C
C*               Skip the station if the segment ( station to grid point)
C*               intersects with the blocking boundaries.
C
		IF  ( dsq .le. srad )  THEN
		    CALL OA_BNDCHK ( gnlat, gnlon, snmlat(is), 
     + 			             snmlon(is), intersect, iret )		
                    IF ( intersect )  GO TO 111  
                END IF
C		
		IF  ( dsq .le. srad )  THEN
C
C*		    If no extrapolation is allowed; set the position
C*		    of the station relative to the grid point by
C*		    octant number.  (ON is x; AT is y.)
C
                    IF  ( at .gt. 0.0 .and. .not. extrap ) THEN
                        IF  ( on .gt. 0.0 ) THEN
                       	    IF  ( on .gt. at ) THEN
                                isect = 1
                            ELSE
                                isect = 2
                            END IF
                        ELSE
                            IF ( at .gt. -on ) THEN
                                isect = 3
                            ELSE
                                isect = 4
                            END IF
                        END IF
                    ELSE IF ( .not. extrap ) THEN
                        IF  ( on .gt. 0.0 ) THEN
                       	    IF  ( -at .gt. on ) THEN
                            	isect = 7
                            ELSE
                                isect = 8
                            END IF
                        ELSE
                            IF  ( on .lt. at ) THEN
                            	isect = 5
                            ELSE
                           	isect = 6
                            END IF
                        END IF
		    END IF
C
C*		    Compute weight to use and apply over levels and
C*		    parameters.
C
		    wt = EXP ( (-dsq) * wtfac )
		    DO  igrid = 1, ngrid
			sdata = data ( igrid, is )
			IF  ( .not. ERMISS (sdata) )  THEN
			    sum (igrid) = sum (igrid) + wt*sdata
			    swt (igrid) = swt (igrid) + wt
			    isn (igrid) = isn (igrid) + 1
			    hit ( isect, igrid ) = .true.
			END IF
		    END DO
		END IF
111	      CONTINUE
	    END DO
C
C*	    Calculate grid point values.
C
	    IF  ( extrap )  THEN
	      DO  igrid = 1, ngrid
		gv = grid ( igrid, ixy )
		IF  ( isn (igrid) .ge. MINSTN .and. .not. ERMISS ( gv )
     +               )  THEN
		    grid ( igrid, ixy ) = grid ( igrid, ixy ) +
     +					  sum (igrid) / swt (igrid)
		ELSE
		    IF  ( .not. gsflag )  THEN
			grid ( igrid, ixy ) = RMISSD
		    END IF
	        END IF
	      END DO
	    ELSE
	      DO  igrid = 1, ngrid
		gv = grid ( igrid, ixy )
		IF  ( isn (igrid) .ge. MINSTN .and. .not. ERMISS ( gv )
     +               )  THEN
		  isec = 1
		  incmpl = .false.
C
C*		  Make sure no more than three consecutive octals did
C*		  not have data.
C
		  DO WHILE  ( ( isec .le. 8 ) .and. .not. incmpl )
		     IF  ( ( .not. hit ( isec  , igrid ) ) .and.
     +		 	 ( .not. hit ( mod8 (isec), igrid ) ) .and.
     +			 ( .not. hit ( mod8 (isec+1), igrid ) ) .and.
     +			 ( .not. hit ( mod8 (isec+2), igrid ) ) ) THEN
			 incmpl = .true.
		     ELSE
			 isec = isec + 1
		     END IF
		  END DO
C*
		  IF  ( incmpl )  THEN
		     IF  ( .not. gsflag )  THEN
			grid ( igrid, ixy ) = RMISSD
		     END IF
		  ELSE
		     grid ( igrid, ixy ) = grid ( igrid, ixy ) +
     +					   sum (igrid) / swt (igrid)
		  END IF
		ELSE
		  IF  ( .not. gsflag )  THEN
		      grid ( igrid, ixy ) = RMISSD
		  END IF
		END IF
	      END DO
	    END IF
C
222         CONTINUE	  
C
	END DO
C*
	RETURN
	END
