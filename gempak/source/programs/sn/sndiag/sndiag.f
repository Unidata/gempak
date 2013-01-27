	PROGRAM SNDIAG
C************************************************************************
C* SNDIAG								*
C*									*
C* This program will read a sounding file, perform spline fitting and	*
C* filtering of the data, compute some special parameters, then		*
C* write the data to a new sounding file. Some output will also appear	*
C* on the Terminal or will be written to a File.			*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 4/92						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'sndiag.prm'
C*
	CHARACTER	snfile*72, snoutf*72, area*48, dattim*48,
     +			timstn*24, wavlen*24, wavspd*24, storm*24,
     +			crdrot*24, tropht*24, trpint*24, cldhgt*24,
     +			mxdpth*24, squall*24, delz*24, filtyp*24,
     +			output*48
C
	LOGICAL		spline
C*
	CHARACTER	snfcur*72, prmdst(MMPARM)*4, parms(NVAR)*4,
     +			arecur*48, stn*8, datcur*48, times(LLMXTM)*20,
     +			levels*48, vcoord*4, voutc*4, prmout(MMPARM)*4,
     +			vouth*4, snparm*72, idntyp*4, outdev(4)*1,
     +			snocur*72
C*
	REAL		vlevel(LLMXLV)
C*
	INTEGER		lun(4)
C*
	LOGICAL		respnd, done, proces, newfil, mrgflg, nwfile,
     +			rngchk, azmchk
C*
	DATA		parms / 'HGHT', 'PRES', 'TEMP', 'DWPT',
     +				'UWND', 'VWND', 'THTV',
     +				'BVIN', 'BVFQ', 'RICH',
     +				'SCOR', 'SCR1', 'SCR2',
     +				'UCVR', 'DUDZ', 'DVDZ',
     +				'FLXA', 'FLXM',
     +				'HELC', 
     +				'UABS', 'VABS', 'SHRD', 'SHRM',
     +				'RRCH', '    ', '    ' /
C*
	DATA		vcoord / 'HGHT' /
	DATA		levels / 'ALL'  /
	DATA		vouth  / 'HGHT' /
	DATA		snparm / 'HGHT;PRES;TEMP;DWPT;UWND;VWND;THTV' /
	DATA		idntyp / 'STID' /
C*
	DATA		iptype / 0 /
C------------------------------------------------------------------------
C*	Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	CALL IP_IDNT  ( 'SNDIAG', ier )
	IF  ( iperr .eq. 0 )  THEN
	    done = .false.
	ELSE
	    done = .true.
	END IF
C
C*	Loop through processing data.
C
	DO WHILE  ( .not. done )
C
C*	    Get user input.
C
	    CALL SNDINP  ( snfile, snoutf,   area, dattim, timstn,
     +			   wavlen, wavspd,  storm, crdrot, tropht,
     +			   trpint, cldhgt, mxdpth, squall,   delz,
     +			   filtyp, spline, output,  iperr )
	    IF  ( iperr .ne. 0 )  THEN
	        done   = .true.
		proces = .false.
	    ELSE
		proces = .true.
	    END IF
C
C*	    Open input sounding file.
C
	    IF  ( proces )  THEN
		CALL SNDFIL ( snfile, snfcur, isnfln, newfil, prmdst,
     +			      npmdst, ivert, mrgflg, iret )
		IF  ( iret .ne. 0 )  proces = .false.
	    END IF
C
C*	    IF RANG and AZIM are in the original data set then
C*	    compute the balloon location.
C
	    rngchk = .false.
	    azmchk = .false.
	    DO  j = 1, npmdst
		IF  ( prmdst(j) .eq. 'RANG' )  rngchk = .true.
		IF  ( prmdst(j) .eq. 'AZIM' )  azmchk = .true.
	    END DO
	    IF  ( rngchk .and. azmchk )  THEN
		parms(ILATI) = 'LATI'
		parms(ILONI) = 'LONI'
		nprms = NVAR
	    ELSE
		nprms = NVAR - 2
	    END IF
C
C*	    Open/create output sounding file.
C
	    IF  ( proces )  THEN
		CALL SNDOPN ( snoutf, snocur, timstn, parms, nprms,
     +			      isnout, nwfile, iret )
		IF  ( iret .ne. 0 )  proces = .false.
	    END IF
C
C*	    Get data from the input file.
C
	    IF  ( proces )  THEN
		CALL LC_UARE  ( area, newfil, isnfln, arecur,
     +				stn, iret )
		IF  ( iret .eq. 0 )  THEN
		    CALL SNDDTM ( isnfln, dattim, newfil, datcur,
     +				  ntime, times, iret )
		END IF
		IF  ( iret .ne. 0 )  proces = .false.
	    END IF
C
C*	    Get the levels and vertical coordinate.
C
	    IF  ( proces )  THEN
		CALL SNDLEV ( levels, vcoord, ivert, nlvl, vlevel,
     +			      levtyp, voutc, lvert, iret )
		IF  ( iret .ne. 0 )  proces = .false.
	    END IF
C
C*	    Get the paramter information.
C
	    IF  ( proces )  THEN
		CALL SNDPRM ( snparm, vouth, prmdst, npmdst, nwfile,
     +			      parms, nprms, prmout, npmout, iret )
		IF  ( iret .ne. 0 )  proces = .false.
	    END IF
C
C*	    Set the output devices.
C
	    IF  ( proces )  THEN
		CALL IN_OUTT ( output, 'SNDIAG', lun, nlun,
     +			       outdev, ier )
	    END IF
C
C*	    Get the data from the input file and put in output file
C*	    and write to output devices.
C
	    IF  ( proces )  THEN
		CALL SNDDTA ( isnfln, isnout, times, ntime, vlevel,
     +                        nlvl, levtyp, lvert, nprms, iptype,
     +                        idntyp, wavlen, wavspd, storm, crdrot,
     +			      tropht, trpint, cldhgt, mxdpth, squall,
     +			      delz, filtyp, spline, nlun, lun, parms,
     +			      iret )
		IF  ( iret .ne. 0 )  proces = .false.
	    END IF
C
C*	    Update global parameters.
C
	    IF  ( proces )  THEN
		CALL SNDUPD  ( snfile, snoutf,   area, dattim, timstn,
     +			       wavlen, wavspd,  storm, crdrot, tropht,
     +			       trpint, cldhgt, mxdpth, squall,   delz,
     +			       filtyp, spline, output,  iperr )
	    END IF
C
C*	    Call dynamic tutor.
C
	    IF  ( .not. done )  THEN
		CALL IP_DYNM  ( done, iret )
	    END IF
	END DO
C*
	IF  ( iperr .ne. 0 )  CALL ER_WMSG ( 'SNDIAG', iperr, ' ', ier )
C
C*	Close the files.
C
	CALL SN_CLOS  ( isnout, ier )
	CALL SN_CLOS  ( isnfln, ier )
C
	CALL IP_EXIT  ( iret )
C*
	END
