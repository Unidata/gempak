	SUBROUTINE RD_DCPM ( segmnt, lens, numnam, cpnam, lnpnm, nxtyp,
     +			     kpshr, jftmst, jftmen, iprms, mapprm,
     +			     lfchr, idtar, oristn, ispnt, rdata, more,
     +			     iret )
C************************************************************************
C* RD_DCPM								*
C*									*
C* This subroutine decodes the parameter lines.				*
C*									*
C* RD_DCPM  ( SEGMNT, LENS, NUMNAM, CPNAM, LNPNM, NXTYP, KPSHR, JFTMST,	*
C*	      JFTMEN, IPRMS, MAPPRM, LFCHR, IDTAR, ORISTN, ISPNT, RDATA,*
C*	      MORE, IRET )						*
C*									*
C* Input parameters:							*
C*      SEGMNT		CHAR*		Bulletin segment		*
C*      LENS		INTEGER		Length of segment		*
C*      NUMNAM		INTEGER		Number of fcst variable names	*
C*	CPNAM (*)	CHAR*		Array of fcst variable names	*
C*	LNPNM (*)	INTEGER		Lengths CPNAM			*
C*	NXTYP (*)	INTEGER		First index of type in CPNAM	*
C*      KPSHR (*)	INTEGER		Positions of forecast hours	*
C*      JFTMST		INTEGER		Index of 1st valid fcst hour	*
C*      JFTMEN		INTEGER		Index of last fcst hour		*
C*	IPRMS (*)	INTEGER		Position of parameter in output	*
C*	MAPPRM (*)	INTEGER		Mapping of cpnam to cprms	*
C*	LFCHR (*)	INTEGER		Local forecast hour		*
C*	IDTAR (*)	INTEGER		First integer date array	*
C*      ORISTN		CHAR*		Originating station		*
C*									*
C* Input and output parameters:						*
C*	ISPNT		INTEGER		Segment pointer			*
C*									*
C* Output parameters:							*
C*      RDATA		REAL		Forecast data			* 
C*	  ( MMPARM, * )							*
C*	MORE		LOGICAL		Flag for extended section	*
C*	IRET		INTEGER		Return code			*
C*					 -1 = bad extended section	*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 9/02						*
C* F. J. Yen/NCEP	11/02	Added UTC, 3HRLY and 6HRLY to time line.*
C*				Cleaned up; Added new parameters & msg.	*
C* S. Chiswell/Unidata	 3/06	Fixed sorted insertion "nn" bug		*
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
C*
	CHARACTER*(*)	segmnt, cpnam (*), oristn
	INTEGER		kpshr (*), lnpnm (*), nxtyp (*), iprms (*)
	INTEGER		mapprm (*), lfchr (*), idtar (*)
    	REAL		rdata ( MMPARM, * )
	LOGICAL		more
C*
C
C*			NUMWX is the number of weather phenomena
C
	PARAMETER	( NUMWX = 11 )
	CHARACTER	cline (22)*3, idir (8)*3, clud (10)*2,
     +			obvs (8)*3, pprb (10)*2
	INTEGER		line (22), mxmnln (22), ips12 (22)
	INTEGER		idtar2 (5), i3prm (3)
	INTEGER		iwxp (NUMWX,22), i3num (3), wnm (NUMWX), iadd(5)
	REAL		drct (8), cfrt (10), ovis (8)
	REAL	 	rlinu (22), rlinl (22), qrn (7)
	REAL		snrn (5), ctsn (5)
	CHARACTER	clin*200, tznlst (9)*4, errstr*80
	LOGICAL		prmfnd, mxmnfl, mxmndt, cont
	DATA		tznlst / 'EDT ', 'EST ', 'CDT ', 'CST ',
     +			         'MDT ', 'MST ', 'PDT ', 'PST ',
     +				 'UTC ' /
	DATA		idir / '  N', ' NE', '  E', ' SE',
     +			       '  S', ' SW', '  W', ' NW' /
	DATA		drct /   0.0,  45.0,  90.0, 135.0,
     +				 180.0, 225.0, 270.0, 315.0 /
	DATA		clud / 'CL', 'FE', 'FW', 'SC', 'PC',
     +			       'B1', 'B2', 'BK', 'MC', 'OV' /
	DATA		cfrt /   0.,   2.,   2.,   3.,   3.,
     +				 5.,   6.,   6.,   6.,   8. /
	DATA		obvs / ' F+', 'PF+', '  H', '  K',
     +			       '  F', ' PF', ' BD', ' BS' /
 	DATA		ovis /    1.,    1.,    2.,    2.,
     +				  3.,    3.,    4.,    4. /
	DATA		pprb / ' S', ' C', ' L', ' O', ' D', 
     +			       'IS', 'WS', 'SC', 'NM', 'WP' /
C*			wnm has codes for drzl, lt rain, mod rain, snow,
C*			rain shwr, lt snow shwr (flurries), snow shwr,
C*			frz, drzl, frz rain, ice pellet (sleet),
C*			thunder shwr (tstms)
	DATA		wnm /    2., 13.,  1.,  3., 16., 55.,
     +				22., 19., 15., 23., 66. /
	DATA		iadd /  0,  20,  40,  60,  80 /
C*			Use .004 in place of .01 for the start of
C*			second range, since "T" has been reported
	DATA		qrn  /  .00, .004, .10, .25, .50, 1., 2. /
	DATA		snrn /  0.,  .004, 2., 4., 6. /  
	DATA		ctsn /  0., 1.,    2., 4., 6. /
C------------------------------------------------------------------------
	iret = 0
C*
	mxmndt = .false.
	DO k = 1, 22
	    ips12 ( k ) = 0
	    DO j = 1, NUMWX
		iwxp (j,k) = 0
	    END DO
	END DO
	more = .false.
	ierlin = 0
	DO WHILE ( ierlin .eq. 0 )
	    ispnto = ispnt
	    CALL RD_GLIN ( segmnt, lens, ispnt, clin, lenl, ierlin )
	    IF ( ierlin .eq. 0 ) THEN
C
C*		    Process range data
C
	        prmfnd = .false.
		jj = nxtyp (1) 
	        DO WHILE ( jj .lt. nxtyp (2) .and. .not. prmfnd ) 
		    IF ( clin (1:lnpnm(jj)) .eq.
     +		            cpnam (jj) (1:lnpnm(jj)) ) THEN 

  		        CALL RD_RNGD ( clin, lenl, lnpnm(jj), kpshr,
     +				jftmst, jftmen, rlinl, rlinu, ier )
		        prmfnd = .true.
			IF ( ier .eq. -1 ) THEN
			    CALL ST_UNPR ( clin (:72), 72, errstr,
     +				    len1, ierr)
			    errstr = oristn(:4) // ':  '
			    CALL DC_WLOG ( 2, 'DCRDF', ier, errstr,
     +					    ierr )
		          ELSE
		            mp = iprms ( mapprm (jj) )
		            IF ( jj .eq. 1 .or. jj .eq. 2 ) THEN
C
C*			        QPF 12HR --> QP12
C*			        MAX QPF  --> QPX2
C
			        DO ii = jftmst, jftmen
			            IF ( rlinu ( ii ) .ne. RMISSD ) THEN
				        cont = .true.
				        jk = 1
				        DO WHILE ( jk .le. 7 .and.
     +						cont )
				            IF ( rlinu (ii) .le.
     +						    qrn (jk) ) THEN
					        cont = .false.
					        IF ( ( rlinu ( ii ) .eq.
     +						    rlinl ( ii ) ) .and.
     +						    ( rlinl (ii) .eq.
     +						    qrn (jk) ) ) 
     +							   THEN 
					            rdata (mp,ii) = jk - 1
					          ELSE
					            rdata (mp,ii) = jk - 2
					        END IF
				              ELSE
				                jk = jk + 1
				            END IF
				        END DO
				        IF ( cont ) rdata (mp,ii) = 6.
C
C*				        ips12 ( ii ) is set to 1 when a
C*				    	12-hr parm data exists.  It is 
C*				    	used for positioning MN/MX data.
C
				        ips12 ( ii ) = 1
			            END IF
			        END DO
		              ELSE IF ( jj .eq. 3 ) THEN
C
C*			        SNOW 12HR --> SN12
C
			        DO ii = jftmst, jftmen
			            IF ( rlinu ( ii ) .ne. RMISSD ) THEN
				        cont = .true.
				        jk = 1
				        DO WHILE ( jk .le. 5 .and.
     +						cont )
				            IF ( rlinu (ii) .le.
     +						     snrn (jk) ) THEN
					      cont = .false.
					      IF ( rlinu (ii) .eq.
     +						     rlinl (ii) .and.
     +						     rlinl (ii) .eq.
     +						     snrn(jk) ) THEN
					        rdata ( mp,ii ) =
     +							 ctsn (jk)
					       ELSE
					        rdata ( mp,ii ) =
     +							 ctsn (jk - 1)
					      END IF
					    END IF
					    jk = jk + 1
				        END DO
				        IF ( cont ) rdata ( mp,ii ) = 6.
C
C*				        ips12 ( ii ) is set to 1 when
C*				        a 12-hr parm data exists for use
C*				        in positioning MX/MN data.
C
				        ips12 ( ii ) = 1
				    END IF
			        END DO
			    END IF
			END IF
		    END IF
		    jj = jj + 1
	        END DO
C
C*	        Process single integer values
C
	        IF ( .not. prmfnd ) THEN
		    jj = nxtyp (2) 
	            DO WHILE ( jj .lt. nxtyp (3) .and. .not. prmfnd ) 
		    	IF ( clin (1:lnpnm(jj)) .eq.
     +				cpnam (jj) (1:lnpnm(jj)) ) THEN
			    mxmnfl = .false.
			    CALL RD_IDAT ( clin, lenl, lnpnm(jj),
     +				    kpshr, jftmst, jftmen, mxmnfl,
     +				    line, ier )
			
			    prmfnd = .true.
			    IF ( ier .eq. -1 ) THEN
				CALL ST_UNPR ( clin (:72), 72, errstr,
     +					len1, ierr)
			        errstr = oristn(:4) // ':  '
			        CALL DC_WLOG ( 2, 'DCRDF', ier, errstr,
     +					    ierr )
			      ELSE
			        mp = iprms ( mapprm (jj) )
			        DO ii = jftmst, jftmen
				    IF ( line ( ii ) .ne. IMISSD ) THEN
				        rdata ( mp, ii ) =
     +					    FLOAT ( line ( ii ) )
C
C*				        jj equals 4 for POP 12HR 
C*				        which is a 12-hr parm, so
C*				        ips12 is set to 1 if data
C*				        exists in that position
C
  				        IF ( jj .eq. 4 ) THEN
					    ips12 ( ii ) = 1
				        END IF
				    END IF
			        END DO
			    END IF
		        END IF
			jj = jj + 1

	    	    END DO
	        END IF
C
C*	        Process character data
C
	        IF ( .not. prmfnd ) THEN
		    jj = nxtyp (3) 
	            DO WHILE ( jj .lt. nxtyp (4) .and. .not. prmfnd ) 
		    	IF ( clin (1:lnpnm(jj)) .eq.
     +				cpnam (jj) (1:lnpnm(jj)) ) THEN
			    CALL RD_CDAT ( clin, lenl, lnpnm(jj),
     +				    kpshr, jftmst, jftmen, cline, ier )
			    prmfnd = .true.

			    IF ( ier .eq. -1 ) THEN
			        CALL ST_UNPR ( clin (:72), 72, errstr,
     +				 	       len1, ierr)
			        errstr = oristn(:4) // ':  '
			        CALL DC_WLOG ( 2, 'DCRDF', ier, errstr,
     +					    ierr )
			      ELSE IF ( jj .eq. 11 .or. jj .eq. 12 )
     +					THEN
C
C*			        WIND DIR --> DRCT
C
			        mp = iprms ( mapprm (jj) )
			        DO ii = jftmst, jftmen
				    CALL ST_FIND ( cline (ii), idir, 8,
     +						   ipos, ierr)
				    IF ( ipos .ne. 0 ) THEN 
      				        rdata ( mp, ii ) = drct ( ipos )
				    END IF 
			        END DO
			      ELSE IF ( jj .eq. 13 ) THEN
C
C*			        OBVIS --> OVIS
C
			        mp = iprms ( mapprm (jj) )
			        DO ii = jftmst, jftmen
				    CALL ST_FIND ( cline (ii), obvs,
     +						   10, ipos, ierr)
				    IF ( ipos .ne. 0 ) THEN 
      				        rdata ( mp, ii ) = ovis ( ipos )
				    END IF 
			        END DO
				
				
			      ELSE IF ( jj .eq. 14 .or. jj .eq. 15 )
     +					THEN
C
C*			        CLOUDS and AVG CLOUDS --> CFRT
C
			        mp = iprms ( mapprm (jj) )
			        DO ii = jftmst, jftmen
				    CALL ST_FIND ( cline (ii) (2:3),
     +						   clud, 10, ipos, ierr)
				    IF ( ipos .ne. 0 ) THEN 
      				        rdata ( mp, ii ) = cfrt ( ipos )
				    END IF 
			        END DO
			    END IF
		        END IF
			jj = jj + 1
	    	    END DO
	        END IF
C
C*	        Process weather probability or areal coverage data
C
	        IF ( .not. prmfnd ) THEN
		    jj = nxtyp (4) 
	            DO WHILE ( jj .lt. nxtyp (5) .and. .not. prmfnd ) 
		    	IF ( clin (1:lnpnm(jj)) .eq.
     +				cpnam (jj) (1:lnpnm(jj)) ) THEN
			    CALL RD_CDAT ( clin, lenl, lnpnm(jj), kpshr,
     +				    jftmst, jftmen, cline, ier )
			    prmfnd = .true.
			    IF ( ier .eq. -1 ) THEN
			      CALL ST_UNPR ( clin (:72), 72, errstr,
     +					     len1, ierr)
			      errstr = oristn(:4) // ':  '
			      CALL DC_WLOG ( 2, 'DCRDF', ier, errstr,
     +					     ierr )
			     ELSE
     			      mprb = iprms ( mapprm (jj) )
			      DO ii = jftmst, jftmen
				CALL ST_FIND ( cline (ii) (2:3), pprb,
     +					       10, iprb, ierr )
				IF ( iprb .ne. 0 ) THEN 
C
C*				  iadd is a (probability based) 
C*				  number added to the priority number
C*				  of the precip phenomenon (1 for drzl
C*				  having the lowest and up to NUMWX for
C*				  tstms having the highest priority).
C*				  This will produce a number iwxp.  The
C*				  highest 3 numbers in iwxp will
C*				  correspond to the 3 precip phenomena
C*				  to be used to calculate WNUM. (Thus,
C*				  WNUM will have the 3 parameters that
C*				  have the highest probability or areal
C*				  coverage.  In case there are more than
C*				  3 parameters having the highest
C*				  probability, the 3 parameters having
C*				  the highest priority will be selected.
C*				  iprb is the WXPB for the parameter.
C*				  nprm is the priority of the phenomena.
C
				  iprb = mod (iprb,5)
				  IF ( iprb .eq. 0 ) iprb = 5
				  IF ( jj .ne. nxtyp(4) ) THEN
				      nprm = nxtyp(5) - jj
				    ELSE
C
C*				      Set nprm to 7 for duplicate
C*				      SNOWSHWRS at postion nxtyp(4) 
C
				      nprm = 7
				  END IF
				  iwxp (nprm,ii) = nprm + iadd (iprb)
				END IF
			      END DO
			    END IF
			END IF
			jj = jj + 1		
		    END DO
		END IF
C
C*	        Process MX/MN or MN/MX data
C
	        IF ( .not. prmfnd ) THEN
		    jj = nxtyp (5) 
	            DO WHILE ( jj .lt. nxtyp (6) .and. .not. prmfnd ) 
		    	IF ( clin (1:lnpnm(jj)) .eq.
     +				cpnam (jj) (1:lnpnm(jj)) ) THEN
			    mxmnfl = .true.
			    CALL RD_IDAT ( clin, lenl, lnpnm(jj),
     +				    kpshr, jftmst, jftmen,
     +				    mxmnfl, mxmnln, mxmner )
			    IF ( mxmner .eq. 0 ) mxmndt = .true.
			    prmfnd = .true.
			END IF
  		        jj = jj + 1		
  		    END DO
		END IF
C
C*		Check if first date in date line for extended data is 
C*		same as first date in first section, then bad section.
C
	        IF ( .not. prmfnd ) THEN
		    ispn = ispnto - 2
   		    CALL RD_GDTE ( segmnt, ispn + 90, ispn,
     +                                  idtar2, idp, ier )
		    IF ( ier .eq. 0 ) THEN
			idate = 1
			DO nn = 1, 5
			    IF ( idtar2 (nn) .ne. idtar (nn) ) THEN
				idate = 0
			    END IF
			END DO
			IF ( idate .eq. 1 ) THEN
			    iret = -1
			    ierlin = -2
			    ispnt = ispnto + ispn
  			END IF
		    prmfnd = .true.
		    END IF 
		END IF
C
C*		Check if time line for extended data
C
	        IF ( .not. prmfnd ) THEN
		    jj = 1
		    jext = jj
		    DO WHILE ( jj .le. 9 .and.
     +			    clin (1:4) .ne. tznlst (jj) )
			jj = jj + 1
			jext = jj
		    END DO
		    IF ( jext .lt. 10 ) THEN
C
C*			Extended data; set pointer back for time line
C
			ispnt = ispnto - 1
			more = .true.
			ierlin = -2		
		    END IF
		END IF
	    END IF
	END DO
	IF ( mxmndt ) THEN
C
C*	    Continue processing MX/MN data to determine positioning
C
	    IF ( mxmner .eq. -1 ) THEN
	        CALL ST_UNPR ( clin (:72), 72, errstr, len1, ierr)
	        errstr = oristn(:4) // ':  '
	        CALL DC_WLOG ( 2, 'DCRDF', ier, errstr, ierr )
	      ELSE
	        i3 = 0
	        DO ii = jftmst, jftmen
		    ips = ii
		    IF ( mxmnln ( ii ) .ne. IMISSD .and.
     +				i3 .eq. 0 ) THEN
		        IF ( ii .lt. jftmen - 1 ) THEN
			    IF ( mxmnln (ii + 1) .ne. IMISSD .and.
     +				    mxmnln (ii + 2) .ne. IMISSD ) THEN
			        i3 = 2
C
C*			        Assume if the next 2 positions have
C*			        data, then it is a group of 3, so need
C*				to determine which value and position
C*				to use.  (Positions of other existing
C*				12-hr parameters must be set in ips12.)
C*				If MX/MN are in groups of 3, then the
C*				value in the same position will be
C*				used.  If there are no other 12-hr
C*				parms nearby, then the first value in
C*				the group will be used.

			        IF ( ips12 ( ii ) .eq. 1 ) THEN
				    ips = ii
			          ELSE IF (ips12 (ii + 1) .eq. 1) THEN
			            ips = ii + 1
			          ELSE IF (ips12 (ii + 2) .eq. 1) THEN
				    ips = ii + 2
			          ELSE
   	 		            ips = ii
			        END IF
		              ELSE
			        ips = ii
			    END IF
		        END IF
C
C*                      Determine whether the MX/MN or MN/MX temperature
C*		        is TNTF or TDYF.
C
		        IF ( lfchr (ips) .ge. 12 ) THEN
C
C*		            TDYF
C
			    CALL ST_FIND ( 'MN/MX', cpnam, NUMNAM,
     +				       ipos, ierr)
			    mp = iprms ( mapprm (ipos) )
			    rdata ( mp, ips ) = FLOAT ( mxmnln ( ips ) )
		          ELSE
C
C*			    TNTF
C
			    CALL ST_FIND ( 'MX/MN', cpnam, NUMNAM,
     +					ipos, ierr)
    			    mp = iprms ( mapprm (ipos) )
			    rdata ( mp, ips ) = FLOAT ( mxmnln ( ips ) )
		        END IF
		      ELSE
C
C*		        This skips over the group of 3
C
		        IF ( i3 .ne. 0 ) i3 = i3 - 1
	            END IF
	        END DO
	    END IF
	END IF
C
C*	Determine wnum from weather probability or areal coverage data.
C*	i3num will have the highest 3 iwxp number based on priority and
C*	probability.  i3prm will have the phenomenon priority
C*	(1 - NUMWX) of the corresponding i3num.
C
	DO j = 1, 3
    	    i3num (j) = 0
	    i3prm (j) = 0
	END DO
	DO k = jftmst, jftmen
	    np = 0
	    DO m = 1, NUMWX
		IF ( iwxp (m,k) .ne. 0 ) THEN
		    IF ( np .eq. 0 ) THEN
		        np = np + 1
			i3num ( np ) = iwxp (m,k)
			i3prm ( np ) = m
		      ELSE IF (np .eq. 1 ) THEN
		        np = np + 1
			IF ( iwxp (m,k) .gt. i3num (1) ) THEN
			    i3num ( 2 ) = i3num ( 1 )
			    i3num ( 1 ) = iwxp (m,k)
			    i3prm ( 2 ) = i3prm ( 1 )
			    i3prm ( 1 ) = m
			  ELSE
			    i3num ( 2 ) = iwxp (m,k)
			    i3prm ( 2 ) = m
			END IF
		      ELSE IF (np .eq. 2 ) THEN
		        np = np + 1
			IF ( iwxp (m,k) .gt. i3num (1) ) THEN
			    i3num ( 3 ) = i3num ( 2 )
			    i3num ( 2 ) = i3num ( 1 )
			    i3num ( 1 ) = iwxp (m,k)
			    i3prm ( 3 ) = i3prm ( 2 )
			    i3prm ( 2 ) = i3prm ( 1 )
			    i3prm ( 1 ) = m
			  ELSE IF ( iwxp (m,k) .gt. i3num (2) ) THEN
			    i3num ( 3 ) = i3num ( 2 )
			    i3num ( 2 ) = iwxp (m,k)
			    i3prm ( 3 ) = i3prm ( 2 )
			    i3prm ( 2 ) = m
			  ELSE
			    i3num ( 3 ) = iwxp (m,k)
			    i3prm ( 3 ) = m
			END IF
		      ELSE
			prmfnd = .false.
			n = 1
			DO WHILE ( .not. prmfnd .and. n .le. 3 )
			  IF ( iwxp (m,k) .gt. i3num (n) ) THEN
			    nn = 3
			    DO WHILE (nn .gt. n ) 
			      i3num (nn) = i3num (nn - 1)
			      i3prm (nn) = i3prm (nn - 1)
			      nn = nn - 1
			    END DO
			    i3num (n) = iwxp (m,k)
			    i3prm (n) = m
			    prmfnd = .true.
			  END IF
			  n = n + 1
			END DO
		    END IF
		END IF
	    END DO
	    IF (np .gt. 0 ) THEN
C
C*		Calculate wnum
C
	        rdata (mprb,k) = wnm (i3prm(1))
		IF ( np .ge. 2 ) THEN
		    rdata (mprb,k) = rdata (mprb,k) +
     +			         80 * wnm (i3prm(2))
		END IF
		IF ( np .eq. 3 ) THEN
		    rdata (mprb,k) = rdata (mprb,k) +
     +			         6400 * wnm (i3prm(3))
		END IF
C
C*		Determine WXPB from iadd.
C
		m = 5
		prmfnd = .false.
		DO WHILE ( .not. prmfnd .and. m .ge. 1)
		    IF ( i3num(1) .gt. iadd(m) ) THEN
			rdata (mprb+1,k) = float (m) 
			prmfnd = .true.
		    END IF
		    m = m - 1
		END DO
	    END IF
	END DO
C*
	RETURN
	END
