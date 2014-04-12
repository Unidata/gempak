	SUBROUTINE D2PFIL ( lunin, lunout, catflg, otlktyp, iret )
C************************************************************************
C* D2PFIL								*
C*									*
C* This subroutine reads the points information from the given input	*
C* file and writes the processed information to the output file.	*
C*									*
C* D2PFIL ( LUNIN, LUNOUT, CATFLG, OTLKTYP, IRET )			*
C*									*
C* Input parameters:							*
C*	LUNIN		INTEGER		Input file unit number		*
C*	LUNOUT		INTEGER		Output file unit number		*
C*	CATFLG		LOGICAL		Categorical flag		*
C*      OTLKTYP         CHAR*           Type of outlook, C=Convective   *
C*                                      F=Fire Weather                  *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 2/02	Initial coding				*
C* S. Jacobs/NCEP	 4/02	Added extra check for the end of file	*
C* F. J. Yen/NCEP	11/04	Enhanced with station relative output 	*
C* G. Grosshans         01/06   Updated to include fire weather outlooks* 
C* F. J. Yen/NCEP	10/07	Updated for extended fire wx outlooks	*
C* S. Jacobs/NCEP	 7/13	Added new fire wx categories		*
C* S. Jacobs/NCEP	11/13	Added MRGL and ENH categories		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
     	LOGICAL		catflg
C*
	PARAMETER	(MXLENB = 68)
	INTEGER		nxstnr(LLMXLN)
	CHARACTER	label*(LLMXLN), cval*4, ltln(LLMXPT)*8,
     +			info(5)*12, buffer*(LLMXLN), stnrel(3*LLMXPT)*10
	CHARACTER	acval(LLMXLN)*4, outbf*(MXLENB), stnrlp*10
        CHARACTER       otlktyp*1
	CHARACTER	carr(20)*30, dattyp*43
     	LOGICAL		done, dryt, hddryt
	DATA	dattyp / 'DRY THUNDERSTORM CRITICAL FIRE WEATHER AREA' /
C------------------------------------------------------------------------
	iret = 0
C
C*	Read the remainder of the input file.
C
	ncval = 0
	nstnrl = 0
	iostat = 0
	cval = ' '
	hddryt = .false.
	DO WHILE  ( iostat .eq. 0 )
C
C*	    Read the label for the next outlook line.
C
	    READ ( lunin, 2000, IOSTAT = iostat ) label
2000	    FORMAT ( A )
	    IF  ( ( iostat .eq. 0 ) .and.
     +		  ( label .ne. ' ' ) .and. 
     +		  ( label .ne. '$$' ) )  THEN
C
C*		Determine if the label is a percentage.
C
		CALL ST_ILST ( label, '%', 0, 1, ival, num, ier )
		IF  ( ival .ne. 0 )  THEN
C
C*		    Convert the percentage to a real number.
C
		    rval = FLOAT (ival) / 100.0
		    CALL ST_RLCH ( rval, 2, cval, ier )
		  ELSE
C
C*		    Otherwise, the label is a string; check if
C*		    a "day" label.
C
		    CALL ST_ALNM ( label(2:2), ityp, ier )
		    IF ( label(1:1) .eq. 'D' .and. ityp .eq. 1 ) THEN
C
C*			It is a 'day' label; store it in cval. 
C
		        CALL ST_CLST ( label, ' ', ' ', 20, carr,
     +				num, ier )
			cval = carr (1)
C
C*			Check if 'day' label has string 'DRYTSTM'
C
			IF ( num .eq. 2 .and.
     +				  carr (2) .eq. 'DRYTSTM' ) THEN
		    	    dryt = .true.
		    	    IF ( .not. hddryt ) THEN
C
C*			      Data type '...' header line for dry
C*			      tstm not written yet.  Write '...' header
C*			      line for dry thunderstorm.
C
			      hddryt = .true.
      	    	              WRITE ( lunout, 1400 )
C
C*              	      Write the data type to the output file.
C
                	      WRITE ( lunout, 1100 ) dattyp
1100            	      FORMAT ( /, '... ', A, ' ...', / )

		            END IF
		          ELSE
		            dryt = .false.
		        END IF    
			
		      ELSE
C
C*		        Not a "day" label, so convert it to the
C*			abbreviated value and store it in cval.
C
		        IF  ( label .eq. 'SIGNIFICANT SEVERE' )  THEN
			  cval = 'SIGN'
		         ELSE IF  ( label .eq. 'MARGINAL RISK' )  THEN
			  cval = 'MRGL'
		         ELSE IF  ( label .eq. 'SLIGHT RISK' )  THEN
			  cval = 'SLGT'
		         ELSE IF  ( label .eq. 'ENHANCED RISK' )  THEN
			  cval = 'ENH '
		         ELSE IF  ( label .eq. 'MODERATE RISK' )  THEN
			  cval = 'MDT '
		         ELSE IF  ( label .eq. 'HIGH RISK' )  THEN
			  cval = 'HIGH'
                         ELSE IF  ( label .eq. 'ISOLATED DRY TSTM ' //
     +				    'AREA FIRE WEATHER' )  THEN
                          cval = 'IDRT'
                         ELSE IF  ( label .eq. 'SCATTERED DRY TSTM ' //
     +				    'AREA FIRE WEATHER' )  THEN
                          cval = 'SDRT'
                         ELSE IF  ( label .eq. 'DRY TSTM AREA ' //
     +				    'FIRE WEATHER' )  THEN
                          cval = 'DTSM'
                         ELSE IF  ( label .eq. 'EXTREMELY CRITICAL ' // 
     +				    'AREA FIRE WEATHER' )  THEN
                          cval = 'EXTM'
                         ELSE IF  ( label .eq. 'CRITICAL AREA ' //
     +				    'FIRE WEATHER' )  THEN
                          cval = 'CRIT'
                         ELSE IF  ( label .eq. 'ELEVATED AREA ' //
     +				    'FIRE WEATHER' )  THEN
                          cval = 'ELEV'
                         ELSE
			  cval = 'TSTM'
			END IF
		    END IF
		END IF
C
C*		The next line is the location of the label, skip it.
C
		iostat = 0
		READ ( lunin, 2000, IOSTAT = iostat ) buffer
		IF  ( iostat .ne. 0 )  THEN
		    done = .true.
		  ELSE
		    done = .false.
		END IF
C
C*		Read the outlook line points until the end of record,
C*		$$, is encountered.
C
		lin = 0
		DO WHILE  ( .not. done )
		    READ ( lunin, 2000, IOSTAT = iostat ) buffer
		    IF  ( ( buffer .eq. '$$' ) .or.
     +		          ( iostat .ne. 0 ) )  THEN
			done = .true.
		      ELSE
			lin   = lin + 1
C
C*			Check for a continuation marker.
C
			icont = INDEX ( buffer, 'CONT' )
			IF  ( icont .eq. 0 )  THEN 
C
C*			    If not CONT, then parse the info to get
C*			    the latitude and longitude.
C
			    CALL ST_CLST ( buffer, ' ', ' ', 5,
     +					   info, num, ier )
			    CALL ST_CRNM ( info(1), rlat, ier )
			    CALL ST_CRNM ( info(2), rlon, ier )
C
C*			    Convert the lat and lon to a single
C*			    8-character string.
C
			    ilat = NINT ( 100.0 * rlat )
			    ilon = MOD ( NINT ( (-100.0) * rlon ),
     +					10000 )
			    WRITE ( ltln(lin), 1200 ) ilat, ilon
1200		    	    FORMAT ( 2I4.4 )
			    IF ( catflg .and. cval .ne. 'SIGN' ) THEN
C
C*				Since categorical, save station relative
C*				information to be output at the end
C
				IF ( info(3) .ne. '0' ) THEN
				    ninfo = 3
				    iskip = 2
				  ELSE
C
C*				    Only station ID, since
C*				    distance is 0.
C
				    ninfo = 1
				    iskip = 4
				END IF
				DO ij = 1, ninfo
				    nstnrl = nstnrl + 1
				    stnrel(nstnrl) = info(ij + iskip)
				END DO
				
C
C*				Save station relative information
C*				for output at the end of outlook
C*				report since categorical.
C
			    END IF
			  ELSE
C
C*			    Set the string to the continuation value.
C
			    ltln(lin) = '99999999'
			    IF ( catflg .and. cval .ne. 'SIGN' ) THEN
C
C*			        Save '...CONT...' marker for station
C*			        relative information to be output at end
C
			        nstnrl = nstnrl + 1
			        stnrel(nstnrl) = '...CONT...'
			    END IF
			END IF
		    END IF
		END DO
		IF ( catflg .and. cval .ne. 'SIGN' .and.
     +				lin .gt. 0 ) THEN
C
C*		    Set element of nxstnr to the last index # into
C*		    stnrel and store the label cval in array acval.
C
		    ncval = ncval + 1
      		    nxstnr(ncval) = nstnrl		
		    acval(ncval) = cval
		END IF
C
C*		If there is some data, write it to the output file. The
C*		first line includes the label value. Subsequent lines
C*		are indented.
C
		IF  ( lin .gt. 0 )  THEN
		    is = 1
		    ie = MIN ( 6, lin )
		    WRITE ( lunout, 1300 ) cval, ( ltln(k), k = is, ie )
1300		    FORMAT ( A, 2x, 6 ( 1X, A ) )
C
		    is = ie + 1
		    ie = MIN ( is + 5, lin )
		    DO  WHILE  ( is .le. lin )
			WRITE ( lunout, 1310 ) (ltln(k), k = is, ie)
1310		    	FORMAT ( 6x, 6 ( 1X, A ) )
			is = ie + 1
			ie = MIN ( is + 5, lin )
		    END DO
		END IF
	    END IF
	END DO
	IF ( ncval .le. 0 ) THEN
	    WRITE ( lunout, 1400 )
1400	    FORMAT ( '&&' )
	  ELSE
	    WRITE ( lunout, 1410 )
1410	    FORMAT ( /, '&&' )
C
C*	    Write out categorical station relative points for each
C*	    category label.
C
	    ibgr = 1
	    DO i = 1, ncval
		IF ( acval(i) .eq. 'TSTM' ) THEN
		    outbf( 1:19 ) = 'GEN TSTMS ARE FCST'
		    ip = 18
		  ELSE
		    outbf(1:11) = 'THERE IS A '
		    CALL ST_LSTR ( acval(i), len, ier )
		    ipt = 12 + len - 1
		    outbf(12:ipt) = acval(i)
		    ip = ipt + 18
		    outbf(ipt + 1:ip) = ' RISK OF SVR TSTMS'
		END IF
		outbf(ip + 1:ip + 28) = ' TO THE RIGHT OF A LINE FROM'
		ip = ip + 29
C
C*	        For each label, prepare the output buffer with the
C*		station relative information.  When the buffer
C*		is full, write it out.
C
		DO j = ibgr, nxstnr(i)
		    CALL ST_LSTR ( stnrel(j), len, ier )
		    IF ( j .eq. nxstnr (i) ) THEN
			stnrlp = stnrel(j)(:len) // '.'
			len = len + 1
		      ELSE
			stnrlp = stnrel(j)(:len)
		    END IF
		    IF ( ip + len .gt. MXLENB  ) THEN
			WRITE ( lunout, 1420 ) outbf(1:ip-1)
1420			FORMAT ( A )
			ip = 1
		    END IF
		    IF ( ip .eq. 1) THEN
		        outbf(ip:ip+len) =  stnrlp(1:len)
			ip = ip + len
		      ELSE
		        outbf(ip:ip+len) = ' ' // stnrlp(1:len)
		        ip = ip + len + 1
		    END IF
		END DO
		WRITE ( lunout, 1430 ) outbf(1:ip-1)
1430		FORMAT ( A, / )
		ibgr = nxstnr(i) + 1
	    END DO
	END IF    
C*
	RETURN
		END
