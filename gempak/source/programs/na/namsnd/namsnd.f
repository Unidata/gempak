	PROGRAM NAMSND
C************************************************************************
C* PROGRAM NAMSND							*
C*									*
C* This programs decodes profile and surface data from NMC models and	*
C* creates both a surface and an upper-air GEMPAK sounding dataset.	*
C* The parameter packing files must be provided by the user.		*
C**									*
C* Log:									*
C* K. Brill/NMC		12/93						*
C* K. Brill/NMC		 7/94	Added SNBPRM				*
C* K. Brill/NMC		10/94	Changes for auxiliary sfc file		*
C* K. Brill/NMC		 1/95	Skip station for iret=2 in NAMRDD	*
C* K. Brill/NMC		 8/95	Add ASCII output option			*
C* K. Brill/EMC		 4/96	Add sfcfc2 for parms in aux file	*
C* K. Brill/EMC		 7/98	Changes for J. Woollen BUFR decoder	*
C* K. Brill/EMC		 9/98	Close BUFR file only if open		*
C* D. Kidwell/NCEP	11/98	snefil -> snbufr                        *
C* D. Kidwell/NCEP	12/98	SNMODL -> NAMSND, SNM... -> NAM...      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	snbufr*132, snoutf*132, sfoutf*132, snprmf*132,
     +			sfprmf*132, timstn*32
	CHARACTER	dattim*20, dtmlst*20, stid*8, sidlst*8,
     +			jensid*3
	CHARACTER*4	parmsn (MMPARM), parmsf (MMPARM),
     +			prmsf2 (MMPARM), snbprm (MMPARM)
	REAL		sndata (LLMXLV*MMPARM), sfdata (2*MMPARM)
	REAL		sfdat2 (MMPARM)
	REAL		sncfac (MMPARM), sfcfac (MMPARM),
     +			sfcfc2 (MMPARM)
	CHARACTER*8	tblde (MMPARM)
	INTEGER		lunsf (2)
	LOGICAL		degree, done, proces, clsbfr
C------------------------------------------------------------------------
C*	Initialize TAE.
C
	CALL IP_INIT  ( respnd, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -1
	    CALL ER_WMSG  ( 'NAMSND', iret, ' ', ier )
	    CALL SS_EXIT
	END IF
	CALL IP_IDNT  ( 'NAMSND', ier )
	done = .false.
	clsbfr = .false.
C
C*	Loop through program until user exits.
C
	DO WHILE  ( .not. done )
C
C*	    Get input.
C
	    CALL NAMINP  ( snbufr, snoutf, sfoutf, snprmf, sfprmf,
     +			   timstn, iret )
	    IF  ( iret .ne. 0 )  THEN
		CALL ER_WMSG  ( 'NAMSND', iret, ' ', ier )
		CALL SS_EXIT
	    END IF
	    proces = .true.
C
C*	    Open the model sounding file, process the parameter
C*	    files and open the output files.  Also read in the
C*	    conversion factors for the parameters.
C
	    CALL NAMOPN ( snbufr, snoutf, sfoutf, snprmf, sfprmf,
     +			  timstn, lunin, lunsn, lunsf,
     +			  jenlun, jenstn, jensid, 
     +			  nprmsn, nprmsf, sncfac, sfcfac, parmsn,
     +			  parmsf, snbprm, npbfr, nprms2, prmsf2, sfcfc2,
     +			  tblde, nde, degree, clsbfr, iret )
	    IF  ( iret .ne. 0 )  THEN
		CALL ER_WMSG  ( 'NAMSND', iret, snbufr, ier )
		proces = .false.
	    END IF
C
C*	    Read the input file; write to the output file.
C
	    dtmlst = ' '
	    sidlst = ' '
	    isnlst = 0
	    istbyt = 1
	    DO WHILE ( proces )
		CALL NAMRDD ( lunin, tblde, nde, degree,
     +			      sncfac, sfcfac, nprmsn,
     +			      nprmsf, parmsn, parmsf, snbprm, npbfr,
     +			      nprms2, prmsf2, sfcfc2,
     +			      istbyt, dattim, stid, istnm,
     +			      slat, slon, selv, sndata, sfdata,
     +			      sfdat2, iclass, nz, ifhr, iret )
		IF ( iret .lt. 0 .and. iret .ne. -27 ) THEN
		    proces = .false.
		    CALL ER_WMSG ( 'NAMSND', iret, ' ', ier )
		ELSE IF ( iret .eq. 1 ) THEN
		    proces = .false.
		    CALL ER_WMSG ( 'NAMSND', iret, ' ', ier )
		ELSE IF ( iret .eq. -27 ) THEN
C
C*		    Just skip this station.
C
		    CALL ER_WMSG ( 'NAMSND', iret, 'WARNING', ier )
		END IF
C*
		IF ( iret .eq. 0 .and. jenlun .ne. 0
     +			.and. istnm .eq. jenstn ) THEN
C
C*		    Add to the ASCII file.
C
		    CALL NAMJEN ( jenlun, nprmsn, nprmsf, parmsn,
     +				  parmsf, dattim, jensid, jenstn,
     +				  slat, slon, selv, sndata, sfdata,
     +				  nz, ifhr, iret )
		END IF
C*
		IF ( iret .eq. 0 .and. lunsn .ne. 0 ) THEN
C
C*		    Set station and time for writing.
C
		    CALL NAMSST ( lunsn, lunsf, stid, istnm,
     +			          slat, slon, selv, dattim,
     +				  dtmlst, sidlst, isnlst, ierx )
		    IF ( ierx .eq. 0 .and. lunsn .ne. 0 ) THEN
     			CALL SN_WDAT ( lunsn, ifhr, nz, sndata,
     +     			       ier )
			IF ( ier .ne. 0 ) 
     +			    CALL ER_WMSG ( 'SN', ier, ' ', ierr )
		    END IF
		    IF ( ierx .eq. 0 .and. lunsf (1) .ne. 0 ) THEN
			CALL SF_WDAT ( lunsf (1), ifhr, sfdata, ier )
		        IF ( ier .ne. 0 ) 
     +			    CALL ER_WMSG ( 'SF', ier, ' ', ierr )
		    END IF
		    IF ( ierx .eq. 0 .and. lunsf (2) .ne. 0 ) THEN
			CALL SF_WDAT ( lunsf (2), ifhr, sfdat2, ier )
		        IF ( ier .ne. 0 ) 
     +			    CALL ER_WMSG ( 'SF', ier, ' ', ierr )
		    END IF
		END IF
	    END DO
C
C*	    Close files.
C
	    IF ( clsbfr ) CALL JB_CLOS  ( lunin, ier )
	    CALL SN_CLOS  ( lunsn, ier )
	    CALL SF_CLOS  ( lunsf(1), ier )
	    CALL SF_CLOS  ( lunsf(2), ier )
C
C*	    Call dynamic tutor.
C
	    CALL IP_DYNM  ( done, ier )
C*
	END DO
C
C*	Exit.
C
	CALL IP_EXIT  ( ier )
C*
	END		
