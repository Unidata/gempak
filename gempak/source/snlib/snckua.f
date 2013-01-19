	SUBROUTINE SN_CKUA  ( isnfln, nparm, parms, iret )
C************************************************************************
C* SN_CKUA								*
C* 									*
C* This subroutine checks the parts in a sounding data set for the	*
C* unmerged data types.  Flags are set in the common area to 		*
C* indicate type of data to be retrieved.				*
C* 									*
C* SN_CKUA  ( ISNFLN, NPARM, PARMS, IRET )				*
C* 									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C* 									*
C* Output parameters:							*
C*	NPARM		INTEGER		Number of parameters		*
C*	PARMS (NPARM)	CHAR*4		Parameter names			*
C*	IRET		INTEGER		Return code			*
C* 					  0 = normal return		*
C*					 -7 = file is not SN dataset	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* M. desJardins/GSFC	12/87	Fixed bug which rejected TTAA only	*
C* D. Kidwell/NCEP	 2/01	Added PPAA & PPCC, trop and mx wnd      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'
C*
	CHARACTER*(*)	parms (*)
	CHARACTER*4	mandpm (6), sigtpm (3), sigwpm (3), troppm (5),
     +			maxwpm (3)
	CHARACTER	part*4, prmnam (MMPARM)*4
	INTEGER		iscale (MMPARM), iofset (MMPARM), ibits (MMPARM)
	LOGICAL		above, done, found
C------------------------------------------------------------------------
	iret   = 0
	nfound = 0
C
C*	Enter default merge parameters in common.
C
	mrgtyp (isnfln) = .false.
	manflg (isnfln) = .false.
	imrtyp (isnfln) = 3
C
C*	Check for various parts.
C
	above = .false.
	done  = .false.
C
C*	Read in part parameter names.
C
	CALL SN_MSPM  ( mandpm, sigtpm, sigwpm, troppm, maxwpm, ier )
C*
	DO WHILE  ( .not. done )
C
C*	    Check mandatory data.
C
	    IF  ( above )  THEN
		part = 'TTCC'
	      ELSE
		part = 'TTAA'
	    END IF
C
C*	    Read in mandatory part information.
C
	    CALL DM_PART  ( isnfln, part, lenhdr, ityprt, np, 
     +			    prmnam, iscale, iofset, ibits,  iret )
	    IF  ( ( iret .eq. 0 ) .and. ( np .eq. 6 ) )  THEN
		found  = .true.
		DO  i = 1, 6
		    IF  ( prmnam (i) .ne. mandpm (i) )  found = .false.
		END DO
	      ELSE
		found = .false.
	    END IF
	    IF  ( found )  nfound = nfound + 1
C
C*	    Set proper mandatory flag.
C
	    IF  ( above )  THEN
		tcflg ( isnfln ) = found
	      ELSE
		taflg ( isnfln ) = found
	    END IF
C
C* 	    Check for tropopause data.
C
	    IF  ( above )  THEN
		part = 'TRPC'
	      ELSE
		part = 'TRPA'
	    END IF
C
C*	    Read in tropopause information.
C
	    CALL DM_PART  ( isnfln, part, lenhdr, ityprt, np, 
     +			    prmnam, iscale, iofset, ibits,  iret )
	    IF  ( ( iret .eq. 0 ) .and. ( np .eq. 5 ) )  THEN
		found  = .true.
		DO  i = 1, 5
		    IF  ( prmnam (i) .ne. troppm (i) )  found = .false.
		END DO
	      ELSE
		found = .false.
	    END IF
	    IF  ( found )  nfound = nfound + 1
C
C*	    Set proper mandatory flag.
C
	    IF  ( above )  THEN
		tcflg ( isnfln ) = tcflg ( isnfln ) .or. found
	      ELSE
		taflg ( isnfln ) = taflg ( isnfln ) .or. found
	    END IF
C
C* 	    Check for maximum wind data.
C
	    IF  ( above )  THEN
		part = 'MXWC'
	      ELSE
		part = 'MXWA'
	    END IF
C
C*	    Read in maximum wind information.
C
	    CALL DM_PART  ( isnfln, part, lenhdr, ityprt, np, 
     +			    prmnam, iscale, iofset, ibits,  iret )
	    IF  ( ( iret .eq. 0 ) .and. ( np .eq. 3 ) )  THEN
		found  = .true.
		DO  i = 1, 3
		    IF  ( prmnam (i) .ne. maxwpm (i) )  found = .false.
		END DO
	      ELSE
		found = .false.
	    END IF
	    IF  ( found )  nfound = nfound + 1
C
C*	    Set proper mandatory flag.
C
	    IF  ( above )  THEN
		tcflg ( isnfln ) = tcflg ( isnfln ) .or. found
	      ELSE
		taflg ( isnfln ) = taflg ( isnfln ) .or. found
	    END IF
C
C*	    Check mandatory wind data.
C
	    IF  ( above )  THEN
		part = 'PPCC'
	      ELSE
		part = 'PPAA'
	    END IF
C
C*	    Read in mandatory wind part information.
C
	    CALL DM_PART  ( isnfln, part, lenhdr, ityprt, np, 
     +			    prmnam, iscale, iofset, ibits,  iret )
	    IF  ( ( iret .eq. 0 ) .and. ( np .eq. 3 ) )  THEN
		found  = .true.
		DO  i = 1, 3
		    ii = i + 2
		    IF ( i . eq. 1 ) ii = i
		    IF ( prmnam (i) .ne. mandpm (ii) )  found = .false.
		END DO
	      ELSE
		found = .false.
	    END IF
	    IF  ( found )  nfound = nfound + 1
C
C*	    Set proper mandatory wind flag.
C
	    IF  ( above )  THEN
		pcflg ( isnfln ) = found
	      ELSE
		paflg ( isnfln ) = found
	    END IF
C
C*	    Check for significant temperature data.
C
	    IF  ( above )  THEN
		part = 'TTDD'
	      ELSE
		part = 'TTBB'
	    END IF
C
C*	    Read in sig temp part information.
C
	    CALL DM_PART  ( isnfln, part,   lenhdr, ityprt, np, 
     +			    prmnam, iscale, iofset, ibits,  iret )
	    IF  ( ( iret .eq. 0 ) .and. ( np .eq. 3 ) )  THEN
		found  = .true.
		DO  i = 1, 3
		    IF (prmnam (i) .ne. sigtpm (i)) found = .false.
		END DO
	      ELSE
		found = .false.
	    END IF
	    IF  ( found )  nfound = nfound + 1
C
C*	    Set proper flag.
C
	    IF  ( above )  THEN
		tdflg ( isnfln ) = found
	      ELSE
		tbflg ( isnfln ) = found
	    END IF
C
C*	    Set significant wind part.
C
	    IF  ( above )  THEN
		part = 'PPDD'
	      ELSE
		part = 'PPBB'
	    END IF
C
C*	    Read in part information.
C
	    CALL DM_PART  ( isnfln, part,   lenhdr, ityprt, np, 
     +			    prmnam, iscale, iofset, ibits,  iret )
	    IF  ( ( iret .eq. 0 ) .and. ( np .eq. 3 ) )  THEN
		found = .true.
		DO  i = 1, 3
		    IF (prmnam (i) .ne. sigwpm (i)) found = .false.
		END DO
	      ELSE
		found = .false.
	    END IF
	    IF  ( found )  nfound = nfound + 1
C
C*	    Set proper flag.
C
	    IF  ( above )  THEN
		pdflg ( isnfln ) = found
	      ELSE
		pbflg ( isnfln ) = found
	    END IF
C
C*	    Check whether two passes have been done.
C
	    IF  ( .not. above )  THEN
		above = .true.
	      ELSE
		done  = .true.
	    END IF
	END DO
C
C*	Check that there is at least some data present.
C
	IF  (  nfound .ge. 1 )  THEN
	    iret   = 0
C
C*	    Return parameter names.
C
	    nparm = 6
	    DO  i = 1, 6
		parms ( i ) = mandpm ( i )
	    END DO
	  ELSE
	    iret   = -7
	END IF
C*
	RETURN
	END
