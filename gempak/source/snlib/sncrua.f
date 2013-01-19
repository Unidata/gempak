	SUBROUTINE SN_CRUA  ( filnam, iflsrc, iptype, maxstn, maxtim, 
     +			      pkflg,  stmflg, trpflg, isnfln, iret )
C************************************************************************
C* SN_CRUA								*
C*									*
C* This subroutine creates a sounding data file which has mandatory	*
C* and significant data stored separately.  If the packing flag,  	*
C* PKFLG, is set, data will be packed using standard packing values.	*
C* If the station time flag is set, a single word is allocated with 	*
C* each data report to store the report time (HHMM). This time should 	*
C* be sent to SN_WPRT.  TRPFLG is used to store tropopause and max wind *
C* data.  								*
C*									*
C* The data source values are parameters in GEMPRM.PRM.			*
C*									*
C* SN_CRUA  ( FILNAM, IFLSRC, IPTYPE, MAXSTN, MAXTIM, PKFLG, STMFLG,	*
C*            TRPFLG, ISNFLN, IRET )					*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		Sounding file name		*
C*	IFLSRC		INTEGER		Data source 			*
C*	IPTYPE		INTEGER		Data parts to be stored:	*
C*					  1 = man below 100 mb		*
C*					  2 = man & sig below 100 mb	*
C*					  3 = man & sig below & above	*
C*	MAXSTN		INTEGER		Maximum number of stations 	*
C*	MAXTIM		INTEGER  	Maximum number of times 	*
C*	PKFLG		LOGICAL		Packing flag			*
C*	STMFLG		LOGICAL		Station time flag		*
C*	TRPFLG		LOGICAL		Tropopause flag			*
C*									*
C* Output parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	IRET		INTEGER		Return code			*
C*				    	  0 = normal return		*
C*				   	 -1 = file not created		*
C*					-22 = invalid part type		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* K. Brill/NMC		 8/93	Changes for 8-char station ID		*
C* D. Kidwell/NCEP	 2/01	Added PPAA & PPCC, text, trop and mx wnd*
C* D. Kidwell/NCEP	 5/01 	Changed SPED scaling to preserve resoln *
C* S. Jacobs/NCEP	 1/04 	Changed SPED scale to get more resoln	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'sncmn.cmn'
C*
	CHARACTER*(*) 	filnam
	LOGICAL 	pkflg, stmflg, trpflg
C*
	PARAMETER	( MAXPRM = 17, MAXPRT = 17 )
C*
	LOGICAL		done, above
	CHARACTER	kcolsn (8)*4, fhdnam*4, krowtm (2)*4
	CHARACTER	prtnam (MAXPRT)*4
	INTEGER		iscale (MAXPRM,MAXPRT), iofset (MAXPRM,MAXPRT),
     +			ibits  (MAXPRM,MAXPRT)
	INTEGER		nparm (MAXPRT), lenhdr (MAXPRT), ityprt (MAXPRT)
	CHARACTER	parms  (MAXPRM,MAXPRT)*4
C*
	CHARACTER	mandpm (6)*4, sigtpm (3)*4, sigwpm (3)*4,
     +			troppm (5)*4, maxwpm (3)*4
	INTEGER		jscale (7), jofset (7), jbits (7)
C*
	DATA		krowtm /'DATE', 'TIME' /
	DATA		kcolsn /'STID', 'STNM', 'SLAT', 'SLON', 'SELV',
     +                          'STAT', 'COUN', 'STD2' /
C
C*	These scale and offset terms are used for data with the 
C*	following min, max and resolution values.
C*		PRES	100,  1100, 1.0  (below 100 mb)
C*		TEMP   -135,    65  0.1
C*		DWPT   -135,    65  0.1
C*		DRCT	  0,   360, 1.0
C*		SPED	  0,   126, 0.01
C*		HGHT  -1050, 64483, 1.0  (use -1050 because of typing
C*					  error in setting offset-MdJ)
C*		PRES	  0,   100, 0.1  (above 100 mb)
C*
 	DATA	jscale  /   0,    -1,    -1,  0, -2,     0,  -1 /
	DATA	jofset  / 100, -1350, -1350,  0,  0, -1050,   0 /
 	DATA	jbits   /  10,    11,    11,  9, 14,    16,  10 /
C
C*				original SPED scaling follows:
C	DATA	jscale  /   0,    -1,    -1,  0,  0,     0,  -1 /
C	DATA	jbits   /  10,    11,    11,  9,  7,    16,  10 /
C
C*				next vers of  SPED scaling follows:
C 	DATA	jscale  /   0,    -1,    -1,  0, -1,     0,  -1 /
C 	DATA	jbits   /  10,    11,    11,  9, 11,    16,  10 /
C------------------------------------------------------------------------
	iret = 0
C
C*	Set variables necessary for DM package.
C
	iftype = MFSN
	nfhdrs = 0
	fhdnam = ' '
	ifhlen = 0
	ifhtyp = 0
	nrow   = maxtim
	nrkeys = 2
	ncol   = maxstn
	nckeys = 8
C
C*	Read in parameters for each part.
C
	CALL SN_MSPM  ( mandpm, sigtpm, sigwpm, troppm, maxwpm, ier )
C
C*	Set up the part information for each part requested.
C
	IF  ( ( iptype .lt. 1 ) .or. ( iptype .gt. 3 ) ) THEN
	    iret = -22
	    RETURN
	END IF
C*
	nprt  = 0
	above = .false.
	done  = .false.
	IF ( iflsrc .eq. 100 ) done = .true.
C 
	DO WHILE ( .not. done )
C
C*	    Add mandatory data.
C
	    nprt = nprt + 1
	    IF  ( above )  THEN
		prtnam  ( nprt ) = 'TTCC'
	      ELSE
		prtnam  ( nprt ) = 'TTAA'
	    END IF
	    nparm  ( nprt ) = 6
	    DO  i = 1, 6
		parms ( i, nprt ) = mandpm ( i )
	    END DO
	    IF  ( pkflg )  THEN
		DO  i = 1, 6
		    iscale ( i, nprt ) = jscale ( i )
		    iofset ( i, nprt ) = jofset ( i )
		    ibits  ( i, nprt ) = jbits  ( i )
		END DO
		IF  ( prtnam ( nprt ) .eq. 'TTCC' )  THEN
		    iscale ( 1, nprt ) = jscale ( 7 )
		    iofset ( 1, nprt ) = jofset ( 7 )
		    ibits  ( 1, nprt ) = jbits  ( 7 )
		END IF
		ityprt ( nprt ) = MDRPCK
	      ELSE
		ityprt ( nprt ) = MDREAL
	    END IF
C
	    nprt = nprt + 1
	    IF  ( above )  THEN
		prtnam  ( nprt ) = 'PPCC'
	      ELSE
		prtnam  ( nprt ) = 'PPAA'
	    END IF
	    nparm  ( nprt ) = 3
	    DO  i = 1, 3
		ii = i + 2
		IF ( i .eq. 1 ) ii = i
		parms ( i, nprt ) = mandpm ( ii )
	    END DO
	    IF  ( pkflg )  THEN
		DO  i = 1, 3
		    ii = i + 2
		    IF ( i .eq. 1 ) ii = i
		    iscale ( i, nprt ) = jscale ( ii )
		    iofset ( i, nprt ) = jofset ( ii )
		    ibits  ( i, nprt ) = jbits  ( ii )
		END DO
		IF  ( prtnam ( nprt ) .eq. 'PPCC' )  THEN
		    iscale ( 1, nprt ) = jscale ( 7 )
		    iofset ( 1, nprt ) = jofset ( 7 )
		    ibits  ( 1, nprt ) = jbits  ( 7 )
		END IF
		ityprt ( nprt ) = MDRPCK
	      ELSE
		ityprt ( nprt ) = MDREAL
	    END IF
C
	    IF ( trpflg ) THEN
C
C*	        Add tropopause level and max wind level data.
C
	        nprt = nprt + 1
	        IF ( above ) THEN
		    prtnam ( nprt ) = 'TRPC'
	          ELSE
		    prtnam ( nprt ) = 'TRPA'
	        END IF
	        nparm ( nprt ) = 5
	        DO i = 1, 5
	            parms ( i, nprt ) = troppm ( i ) 
	        END DO
	        IF ( pkflg ) THEN
	            DO i = 1, 5
		        iscale ( i, nprt ) = jscale ( i )
		        iofset ( i, nprt ) = jofset ( i )
		        ibits ( i, nprt )  = jbits ( i )
	            END DO
		    IF  ( prtnam ( nprt ) .eq. 'TRPC' )  THEN
		        iscale ( 1, nprt ) = jscale ( 7 )
		        iofset ( 1, nprt ) = jofset ( 7 )
		        ibits  ( 1, nprt ) = jbits  ( 7 )
		    END IF
	            ityprt ( nprt ) = MDRPCK
	          ELSE
	            ityprt ( nprt ) = MDREAL
	        END IF
C
	        nprt = nprt + 1
	        IF ( above ) THEN
		    prtnam ( nprt ) = 'MXWC'
	          ELSE
		    prtnam ( nprt ) = 'MXWA'
	        END IF
	        nparm ( nprt ) = 3
	        DO i = 1, 3
	            parms ( i, nprt ) = maxwpm ( i ) 
	        END DO
	        IF ( pkflg ) THEN
	            DO i = 1, 3
		        ii = i + 2
		        IF ( i .eq. 1 ) ii = i
		        iscale ( i, nprt ) = jscale ( ii )
		        iofset ( i, nprt ) = jofset ( ii )
		        ibits ( i, nprt )  = jbits ( ii )
	            END DO
		    IF  ( prtnam ( nprt ) .eq. 'MXWC' )  THEN
		        iscale ( 1, nprt ) = jscale ( 7 )
		        iofset ( 1, nprt ) = jofset ( 7 )
		        ibits  ( 1, nprt ) = jbits  ( 7 )
		    END IF
	            ityprt ( nprt ) = MDRPCK
	          ELSE
	            ityprt ( nprt ) = MDREAL
	        END IF
	    END IF
C
C*	    Check significant level data.
C
	    IF  ( iptype .ne. 1 )  THEN
C
C*		Add significant temperature part.
C
		nprt = nprt + 1
		IF  ( above )  THEN
		    prtnam  ( nprt ) = 'TTDD'
		  ELSE
		    prtnam  ( nprt ) = 'TTBB'
		END IF
		nparm ( nprt ) = 3
		DO  i = 1, 3
		    parms ( i, nprt ) = sigtpm ( i )
		END DO
		IF  ( pkflg )  THEN
		    DO  i = 1, 3
			iscale  ( i, nprt ) = jscale ( i )
			iofset  ( i, nprt ) = jofset ( i )
			ibits   ( i, nprt ) = jbits  ( i )
		    END DO
		    ityprt  ( nprt ) = MDRPCK
	            IF (prtnam (nprt) .eq. 'TTDD') THEN
	                iscale (1, nprt) = jscale (7)
	                iofset (1, nprt) = jofset (7)
	                ibits  (1, nprt) = jbits  (7)
	            END IF
		  ELSE
		    ityprt  ( nprt ) = MDREAL
		END IF
C
C*		Add significant wind part.
C
		nprt = nprt + 1
		IF  ( above )  THEN
		    prtnam  ( nprt ) = 'PPDD'
		  ELSE
		    prtnam  ( nprt ) = 'PPBB'
		END IF
		nparm ( nprt ) = 3
		DO  i = 1, 3
		    parms ( i, nprt ) = sigwpm ( i )
		END DO
		IF  ( pkflg )  THEN
		    DO  i = 1, 3
			IF  ( i .eq. 1 )  THEN
			    ii = 6
			  ELSE
			    ii = i + 2
			END IF
			iscale  ( i, nprt ) = jscale ( ii )
			iofset  ( i, nprt ) = jofset ( ii )
			ibits   ( i, nprt ) = jbits  ( ii )
		    END DO
		    ityprt  ( nprt ) = MDRPCK
		  ELSE
		    ityprt  ( nprt ) = MDREAL
		END IF
	    END IF
C
C*	    Check to see if data above 100 mb is to be added.
C
	    IF  ( ( .not. above ) .and. ( iptype .eq. 3 ) )  THEN
		above = .true.
	      ELSE
		done  = .true.
	    END IF
	END DO
C
C*	Set up the part information for the text parts if requested.
C
	IF ( iflsrc .ge. 100 ) THEN
	    done  = .false.
	    above = .false.
C
	    DO WHILE ( .not. done )
C
C*	        Add text for mandatory data.
C
	        nprt = nprt + 1
	        IF  ( above )  THEN
		    prtnam  ( nprt ) = 'TXTC'
	          ELSE
		    prtnam  ( nprt ) = 'TXTA'
	        END IF
	        nparm  ( nprt ) = 1
		parms ( 1, nprt ) = 'TEXT'
		iscale ( 1, nprt ) = 0
		iofset ( 1, nprt ) = 0
		ibits  ( 1, nprt ) = 0
		ityprt ( nprt ) = MDCHAR
C
C*	        Add text for significant level data below 100 mb.
C
	        IF  ( ( iptype .ne. 1 ) .and. ( .not. above ) )  THEN
C
C*		    Add significant temperature text.
C
		    nprt = nprt + 1
		    prtnam  ( nprt ) = 'TXTB'
		    nparm ( nprt ) = 1
		    parms ( 1, nprt ) = 'TEXT'
		    iscale  ( 1, nprt ) = 0
		    iofset  ( 1, nprt ) = 0
		    ibits   ( 1, nprt ) = 0
		    ityprt  ( nprt ) = MDCHAR
C
C*		    Add significant wind text.
C
		    nprt = nprt + 1
		    prtnam  ( nprt ) = 'TXPB'
		    nparm ( nprt ) = 1
		    parms ( 1, nprt ) = 'TEXT'
		    iscale  ( 1, nprt ) = 0
		    iofset  ( 1, nprt ) = 0
		    ibits   ( 1, nprt ) = 0
		    ityprt  ( nprt ) = MDCHAR
		END IF
C
C*	        Check to see if text data above 100 mb is to be added.
C
	        IF  ( ( .not. above ) .and. ( iptype .eq. 3 ) )  THEN
		    above = .true.
	          ELSE
		    done  = .true.
	        END IF
	    END DO
	END IF
C
C*	Check to see if time is to be stored with each report.
C
	IF  (stmflg)  THEN
	    length = 1
	  ELSE
	    length = 0
	END IF
C*
	DO  i = 1, nprt
	    lenhdr ( i ) = length
	END DO
C
C*	Create the file.
C
	CALL DM_CRET (filnam, iftype, iflsrc, nfhdrs, fhdnam, ifhlen,
     +                ifhtyp, nrow,   nrkeys, krowtm, ncol,   nckeys, 
     +	              kcolsn, nprt,   prtnam, lenhdr, ityprt, nparm, 
     +	              MAXPRM, parms,  iscale, iofset, ibits,  isnfln,
     +                iret)
C
C*	Write out errors.
C
	IF (iret .ne. 0) THEN
	    CALL ER_WMSG ('DM', iret, filnam, ier)
	    iret = -1
	    CALL ER_WMSG ('SN', iret, filnam, ier)
	    RETURN
	END IF
C
C*	Load common areas.
C
	isndfn (isnfln) = isnfln
	dttype (isnfln) = 'ROW'
	sttype (isnfln) = 'COL'
	kdate  (isnfln) = 1
	ktime  (isnfln) = 2
	kstid  (isnfln) = 1
	kstnm  (isnfln) = 2
	kslat  (isnfln) = 3
	kslon  (isnfln) = 4
	kselv  (isnfln) = 5
	kstat  (isnfln) = 6
	kcoun  (isnfln) = 7
	kstd2  (isnfln) = 8
	kparm  (isnfln) = 6
	krow   (isnfln) = 0
	kcol   (isnfln) = 0
	curtim (isnfln) = ' '
	curstn (isnfln) = ' '
	icrstn (isnfln,1) = 0
	icrstn (isnfln,2) = 0
	timset (isnfln) = .false.
	stnset (isnfln) = .false.
	ftmset (isnfln) = .false.
	mrgtyp (isnfln) = .false.
C
C*	Set flags indicating type of data in common.
C
	taflg (isnfln) = .true.
	paflg (isnfln) = .true.
	IF  ( iptype .ne. 1 )  THEN
	    tbflg (isnfln) = .true.
	    pbflg (isnfln) = .true.
	  ELSE
	    tbflg (isnfln) = .false.
	    pbflg (isnfln) = .false.
	END IF
	IF  ( iptype .eq. 3 )  THEN
	    tcflg (isnfln) = .true.
	    pcflg (isnfln) = .true.
	    tdflg (isnfln) = .true.
	    pdflg (isnfln) = .true.
	  ELSE
	    tcflg (isnfln) = .false.
	    pcflg (isnfln) = .false.
	    tdflg (isnfln) = .false.
	    pdflg (isnfln) = .false.
	END IF
C*
	RETURN
	END
