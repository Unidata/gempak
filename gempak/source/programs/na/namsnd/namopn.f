            SUBROUTINE  NAMOPN ( snbufr, snoutf, sfoutf,
     +				 snprmf, sfprmf, timstn,
     +                    	 lunin, lunsn, lunsf,
     +			         jenlun, jenstn, jensid, 
     +				 nprmsn, nprmsf, sncfac, sfcfac,
     +				 parmsn, parmsf, snbprm, npbfr,
     +				 nprms2, prmsf2, sfcfc2,
     +				 tblde, nde, degree, bfropn, iret )
C************************************************************************
C* NAMOPN								*
C*									*
C* This subroutine opens the files needed by NAMSND.  The output files	*
C* are opened or created and the number of parameters is determined.	*
C* Conversion factors for the parameters are read from the parameter	*
C* packing file.  Then, the input file is opened.			*
C*									*
C* The name of the input BUFR file is given in snbufr.                  *
C*									*
C* If the surface file name is followed by +, then a second surface	*
C* file is opened.  Its name is that of the first with the suffix	*
C* _aux.  The packing table for this auxiliary file is expected to 	*
C* have the name of that specified in SFPRMF with the suffix _aux.	*
C* The auxiliary parameters are diagnosed parameters only.		*
C*									*
C* If the parameter file names (SNPRMF and SFPRMF) are blank, then	*
C* a dump of the parameter names from the BUFR table stored in the	*
C* first BUFR message is written to the file bufr_table.dump.		*
C*									*
C* NOTE ABOUT THE PARAMETER TABLE ENTRIES:				*
C*									*
C* Diagnosed or extra computed parameters are added at the end of the	*
C* parameter list for surface data.  They may be added anywhere for	*
C* profile data, but the last two columns must contain the actual	*
C* BUFR parameters in correct order.  The penultimate column is the	*
C* scaling factor, and the last column is the corresponding parameter	*
C* name expected by the conversion codes.  Note that the names in the	*
C* last column need not agree with the names in the first column for	*
C* sounding parameter files.  Surface parameter files do not have the	*
C* extra last column of names.						*
C*									*
C* An ASCII file for a particular station is requested by placing	*
C* the string |SSS=##### following the specification for SNBUFR.	*
C* SSS is the three character station ID and ##### is the station	*
C* number.  The ASCII file will be named prof.SSS.			*
C*									*
C* NAMOPN  ( SNBUFR, SNOUTF, SFOUTF, SNPRMF, SFPRMF, TIMSTN,		*
C*	     LUNIN, LUNSN, LUNSF, JENLUN, JENSTN, JENSID,		*
C*	     NPRMSN, NPRMSF, SNCFAC, SFCFAC,				*
C*	     PARMSN, PARMSF, SNBPRM, NPBFR, NPRMS2, PRMSF2, SFCFC2,	*
C*	     TBLDE, NDE, DEGREE, BFROPN, IRET )				*
C*									*
C* Input parameters:							*
C*	SNBUFR		CHAR*		Model sounding file name	*
C*	SNOUTF		CHAR*		Output sounding file name	*
C*	SFOUTF		CHAR*		Output surface file name	*
C*	SNPRMF		CHAR*		Sounding parm packing file name *
C*	SFPRMF		CHAR*		Surface parm packing file name  *
C*	TIMSTN		CHAR*		Maximum times / stations	*
C*									*
C* Output parameters:							*
C*	LUNIN		INTEGER		Model snd file unit number	*
C*	LUNSN		INTEGER		Output snd data file number	*
C*	LUNSF (2)	INTEGER		Output sfc data file numbers	*
C*	JENLUN		INTEGER		Unit number of ASCII file	*
C*	JENSTN		INTEGER		Station number for ASCII file	*
C*	JENSID		CHAR*3		Station ID for ASCII file	*
C*	NPRMSN		INTEGER		Number of sounding parms	*
C*	NPRMSF		INTEGER		Number of surface parms		*
C*	SNCFAC (NPRMSN) REAL		Conversion factors for snd data	*
C*	SFCFAC (NPRMSF) REAL		Conversion factors for sfc data *
C*	PARMSN (NPRMSN) CHAR*4		Sounding parm list		*
C*	PARMSF (NPRMSF) CHAR*4		Surface parm list		*
C*	SNBPRM (NPBFR)  CHAR*4		BUFR profile parm list		*
C*	NPBFR		INTEGER		Number in profile parm list	*
C*	NPRMS2		INTEGER		Number of auxiliary sfc parms	*
C*	PRMSF2 (NPRMS2) CHAR*4		Auxiliary surface parm list	*
C*	SFCFC2 (NPRMS2) REAL		Conversion factors for sfc data *
C*	TBLDE(*)	CHAR*		Table D entry list		*
C*	NDE		INTEGER		Number of Table D entries	*
C*	DEGREE		LOGICAL		Flag for stn lat/lon in degrees *
C*	BFROPN		LOGICAL		Flag that BUFR file is open	*
C*	IRET		INTEGER		Return code			*
C*					  3 = wrote bufr_table.dump	*
C*					  0 = normal			*
C*					 -1 = file not open		*
C*					 -3 = error opening SNBUFR      *
C*					 -4 = error opening SNPRMF	*
C*					 -5 = wrong # of snd parms	*
C*					 -6 = wrong sounding parms	*
C*					 -7 = cannot create snd file	*
C*					 -8 = error opening SFPRMF	*
C*					 -9 = wrong # of sfc parms	*
C*					-10 = wrong surface parms	*
C*					-11 = cannot create sfc file	*
C*					-13 = NAMCFC failed		*
C*					-30 = error opening aux file	*
C*					-31 = wrong # of aux parms	*
C*					-32 = wrong auxiliary parms	*
C*					-33 = cannot create aux file	*
C*					-55 = error opening SNOUTF      *
C*					-56 = error opening SFOUTF      *
C*					-57 = error opening aux SFOUTF  *
C**									*
C* Log:									*
C* K. Brill/NMC		12/93						*
C* K. Brill/NMC		05/94	Open BUFR file with DA_OPEN		*
C* K. Brill/NMC		 7/94	Added SNBPRM				*
C* K. Brill/NMC		10/94	Changes for auxiliary surface file	*
C* K. Brill/EMC		 4/96	Add SFCFC2				*
C* K. Brill/EMC		 7/98	Changes for using J. Woollen's decoder	*
C* K. Brill/EMC		 8/98	Add NPBFR, Cleanup/document error codes	*
C* K. Brill/EMC		 9/98	Added BFROPN in call			*
C* D. Kidwell/NCEP	11/98	SNEFIL -> SNBUFR; corrected prologue    *
C* K. Brill/EMC		11/98	Pass logical flag to NAMCFC		*
C* D. Kidwell/NCEP	12/98	SNM... -> NAM..., SNMODL -> NAMSND      *
C* D. Kidwell/NCEP	12/98	Check for file existence, allow ' +'    *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	snbufr, snoutf, sfoutf, snprmf, sfprmf, timstn
	CHARACTER*(*)   parmsn (*), parmsf (*), prmsf2 (*), snbprm (*)
	CHARACTER*(*)	jensid, tblde (*)
	REAL		sncfac (*), sfcfac (*), sfcfc2 (*)
	INTEGER		lunsf (2)
	LOGICAL		degree, bfropn
C*
        INTEGER         iscl (MMPARM), ioff (MMPARM), ibit (MMPARM)
	INTEGER		ilist (2)
        CHARACTER       reqprm (MMPARM)*4, filprm (MMPARM)*4
	CHARACTER	sfout*256, sfout2*256, sfprm2*256, newfil*256
	CHARACTER	snef(2)*132, bfrfl*132, csn (2)*16
	LOGICAL		pkflg, mrgdat, exist
C----------------------------------------------------------------------
	iret  = 0
	bfropn = .false.
C*
	nde = 0
	lunsn = 0
	lunsf (1) = 0
	lunsf (2) = 0
	nprmsn = 0
	nprmsf = 0
	nprms2 = 0
	jenlun = 0
	jenstn = 0
	jensid = ' '
	CALL ST_LSTR ( snprmf, lnsn, ier )
	CALL ST_LSTR ( sfprmf, lnsf, ier )
C
C*	Find out if an ASCII output file is needed.
C
	iq = INDEX ( snbufr, '|' )
	IF ( iq .ne. 0 ) THEN
	    CALL ST_CLST ( snbufr, '|', ' ', 2, snef, num, ier )
	    IF ( lnsf .eq. 0 .and. lnsn .eq. 0 ) THEN
		CALL NAMINV ( snef (1), iret )
		IF ( iret .ne. 0 ) THEN
		    CALL ER_WMSG ( 'NAMSND', iret, ' ', ier )
		END IF
		iret = 3
		RETURN
	    END IF
	    CALL ST_CLST ( snef (2), '=', ' ', 2, csn, num, ier )
	    jensid = csn (1) (1:3)
	    CALL ST_NUMB ( csn (2), jenstn, ier )
	    snef (2) = 'prof.' // jensid
	    CALL ST_LCUC ( jensid, jensid, ier )
	    CALL FL_SUNK ( snef (2), jenlun, ier )
	    IF ( snoutf .eq. ' ' ) THEN
C
C*      	Get parameter information
C
        	CALL DP_FILE ( snprmf, npsn, filprm, iscl, ioff,
     +			       ibit, pkflg, iret )
	    	nprmsn = npsn
	    	DO ip = 1, nprmsn
	            parmsn (ip) = filprm (ip)
	    	END DO
	        CALL NAMCFC ( snprmf, .true.,
     +			      npbfr, sncfac, snbprm, iret )
		IF ( iret .ne. 0 ) RETURN
	    END IF
	    IF ( sfoutf .eq. ' ' ) THEN
	    	CALL DP_FILE ( sfprmf, npsf, filprm, iscl,
     +			       ioff, ibit, pkflg, ier )
	    	nprmsf = npsf
	    	DO ip = 1, nprmsf
	            parmsf (ip) = filprm (ip)
	    	END DO
	    	CALL NAMCFC ( sfprmf, .false.,
     +			      n, sfcfac, reqprm, iret )
		IF ( iret .ne. 0 ) RETURN
	    END IF
	ELSE
	    snef (1) = snbufr
	    IF ( lnsf .eq. 0 .and. lnsn .eq. 0 ) THEN
		CALL NAMINV ( snef (1), iret )
		IF ( iret .ne. 0 ) THEN
		    CALL ER_WMSG ( 'NAMSND', iret, ' ', ier )
		END IF
		iret = 3
		RETURN
	    END IF
	END IF
C
C*	Open the sounding output file.
C
	IF ( snoutf .ne. ' ' ) THEN
	    CALL FL_INQR ( snoutf, exist, newfil, ier )
	    IF ( exist ) THEN
	        CALL SN_OPNF ( snoutf, .true., lunsn, iflsrc, npsn,
     +			       filprm, ivert, mrgdat, ier )
	        IF ( ier .eq. 0 ) THEN
	    	    CALL DP_FILE ( snprmf, npreq, reqprm, iscl,
     +			           ioff, ibit, pkflg, ier )
	    	    IF ( ier .ne. 0 ) THEN
		        iret = -4
		        RETURN
	    	    END IF
	    	    IF ( npsn .ne. npreq ) THEN
		        iret = -5
		        RETURN
	    	    END IF
	    	    DO i = 1, npsn
		        IF ( filprm (i) .ne. reqprm (i) ) THEN
		    	    iret = -6
		    	    RETURN
		        END IF
	    	    END DO
		  ELSE
		    iret = -55
		    RETURN
		END IF
	    ELSE
C
C*	        Create a new sounding data file.
C
C*          	Get the number of times and the number of stations to
C*          	add.
C
            	CALL ST_ILST  ( timstn, '/', IMISSD, 2, ilist, n,
     +				ier )
            	IF  ( ilist (1) .eq. IMISSD )  THEN
                    maxtim = 49
            	ELSE
                    maxtim = ilist (1)
            	END IF
            	IF  ( ilist (2) .eq. IMISSD )  THEN
                    maxstn = 700
            	ELSE
                    maxstn = ilist (2)
            	END IF
	    	CALL SN_CRFP ( snoutf, snprmf, 0, maxstn, maxtim, 
     +			       .true., lunsn, npsn, filprm, pkflg,
     +			       ier )
	    	IF ( ier .ne. 0 ) THEN
		    iret = -7
		    RETURN
	    	END IF
	    	CALL ER_WMSG ( 'NAMSND', 2, snoutf, ier )
	    END IF
	    nprmsn = npsn
	    DO ip = 1, nprmsn
	        parmsn (ip) = filprm (ip)
	    END DO
	    CALL NAMCFC ( snprmf, .true.,
     +			  npbfr, sncfac, snbprm, iret )
	    IF ( iret .ne. 0 ) RETURN
	END IF
C
C*	Open the surface output file(s).
C
	iplus = INDEX ( sfoutf, '+' )
	IF ( iplus .gt. 1 ) THEN
	    im = iplus - 1
	    sfout = sfoutf ( 1:im )
	    CALL ST_LSTR ( sfout ( 1:im ), lens, ier )
	    sfout2 = sfout ( 1:lens ) // '_aux'
	    CALL ST_LSTR ( sfprmf, lens, ier )
	    sfprm2 = sfprmf ( 1:lens ) // '_aux'
	ELSE
	    sfout = sfoutf
	    sfout2 = ' '
	END IF
	IF ( sfout .ne. ' ' ) THEN
	    CALL FL_INQR ( sfout, exist, newfil, ier )
	    IF ( exist ) THEN
	        CALL SF_OPNF ( sfout, .true., lunsf (1), iflsrc,
     +			       npsf, filprm, ier )
	        IF ( ier .eq. 0 ) THEN
	    	    CALL DP_FILE ( sfprmf, npreq, reqprm, iscl,
     +			           ioff, ibit, pkflg, ier )
	    	    IF ( ier .ne. 0 ) THEN
		        iret = -8
		        RETURN
	    	    END IF
	    	    IF ( npsf .ne. npreq ) THEN
		        iret = -9
		        RETURN
	    	    END IF
	    	    DO i = 1, npsf
		        IF ( filprm (i) .ne. reqprm (i) ) THEN
		    	    iret = -10
		    	    RETURN
		        END IF
	    	    END DO
		  ELSE
		    iret = -56
		    RETURN
		END IF
	    ELSE
C
C*	    	Create a new surface data file.
C
C*          	Get the number of times and the number of stations to
C*          	add.
C
            	CALL ST_ILST  ( timstn, '/', IMISSD, 2, ilist,
     +				n, ier )
            	IF  ( ilist (1) .eq. IMISSD )  THEN
                    maxtim = 49
            	ELSE
                    maxtim = ilist (1)
            	END IF
            	IF  ( ilist (2) .eq. IMISSD )  THEN
                    maxstn = 700
            	ELSE
                    maxstn = ilist (2)
            	END IF
	    	CALL SF_CRFP ( sfout, sfprmf, 0, maxstn, maxtim, 
     +			       .true., lunsf (1), npsf, filprm, pkflg,
     +			       ier )
	    	IF ( ier .ne. 0 ) THEN
		    iret = -11
		    RETURN
	    	END IF
	    	CALL ER_WMSG ( 'NAMSND', 2, sfout, ier )
	    END IF
	    nprmsf = npsf
	    DO ip = 1, nprmsf
	        parmsf (ip) = filprm (ip)
	    END DO
	    CALL NAMCFC ( sfprmf, .false.,
     +			  n, sfcfac, reqprm, iret )
	    IF ( iret .ne. 0 ) RETURN
	END IF
C
C*	Open a second file if needed.
C
	IF ( sfout2 .ne. ' ' ) THEN
	    CALL FL_INQR ( sfout2, exist, newfil, ier )
	    IF ( exist ) THEN
	        CALL SF_OPNF ( sfout2, .true., lunsf (2), iflsrc,
     +			       npsf, filprm, ier )
	        IF ( ier .eq. 0 ) THEN
	    	    CALL DP_FILE ( sfprm2, npreq, reqprm, iscl,
     +			           ioff, ibit, pkflg, ier )
	    	    IF ( ier .ne. 0 ) THEN
		        iret = -30
		        RETURN
	    	    END IF
	    	    IF ( npsf .ne. npreq ) THEN
		        iret = -31
		        RETURN
	    	    END IF
	    	    DO i = 1, npsf
		        IF ( filprm (i) .ne. reqprm (i) ) THEN
		    	    iret = -32
		    	    RETURN
		        END IF
	    	    END DO
		  ELSE
		    iret = -57
		    RETURN
		END IF
	    ELSE
C
C*	    	Create a new surface data file.
C
C*          	Get the number of times and the number of stations to
C*          	add.
C
            	CALL ST_ILST  ( timstn, '/', IMISSD, 2, ilist,
     +				n, ier )
            	IF  ( ilist (1) .eq. IMISSD )  THEN
                    maxtim = 49
            	ELSE
                    maxtim = ilist (1)
            	END IF
            	IF  ( ilist (2) .eq. IMISSD )  THEN
                    maxstn = 700
            	ELSE
                    maxstn = ilist (2)
            	END IF
	    	CALL SF_CRFP ( sfout2, sfprm2, 0, maxstn, maxtim, 
     +			       .true., lunsf (2), npsf, filprm, pkflg,
     +			       ier )
	    	IF ( ier .ne. 0 ) THEN
		    iret = -33
		    RETURN
	    	END IF
	    	CALL ER_WMSG ( 'NAMSND', 2, sfout2, ier )
	    END IF
	    nprms2 = npsf
	    DO ip = 1, nprms2
	        prmsf2 (ip) = filprm (ip)
	    END DO
	    CALL NAMCFC ( sfprm2, .false.,
     +			  n, sfcfc2, reqprm, iret )
	    IF ( iret .ne. 0 ) RETURN
	END IF
C
C*	Open the input file.
C
	bfrfl = snef (1)
	CALL SS_ENVR ( bfrfl, bfrfl, ier )
        CALL NAMPRP ( bfrfl, lunin, tblde, nde, degree, ier )
	IF ( ier .ne. 0 ) THEN
	    CALL ER_WMSG ( 'NAMSND', ier, ' ', ier2 )
	    iret = -3
	    RETURN
	END IF
	bfropn = .true.
C*
	RETURN
	END
