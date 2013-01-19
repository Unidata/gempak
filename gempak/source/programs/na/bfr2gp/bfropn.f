            SUBROUTINE  BFROPN ( snefil, sffsrc, snoutf, sfoutf,
     +				 snprmf, sfprmf, timstn,
     +				 mxnbsn, mxnbsf,
     +                    	 lunin, lunsn, lunsf,
     +				 nprmsn, nprmsf, sncfac, sfcfac,
     +				 snctrm, sfctrm, parmsn, parmsf,
     +				 bprmsn, bprmsf, irepsf, nbsn, nbsf,
     +				 istrtf, ship, upaflg, catflg, iret )
C************************************************************************
C* BFROPN								*
C*									*
C* This subroutine opens the files needed by BFR2GP.  The output files	*
C* are opened or created and the number of parameters is determined.	*
C* Conversion factors and terms for the parameters are read from the	*
C* BUFR-to-GEMPAK conversion table files.  The names of these files	*
C* are given in SNPRMF and SFPRMF for sounding and surface parameters,	*
C* respectively.							*
C*									*
C* The BUFR-to-GEMPAK conversion table files are parsed by BG_PARM.	*
C* They consist of four columns of the following data:			*
C*									*
C*	BUFR name  GEMPAK name  conversion factor  conversion term	*
C*									*
C* The GEMPAK value is computed as follows:				*
C*									*
C* 	GEMPAK value = BUFR value * factor + term			*
C*									*
C* The BUFR file name and associated table file name are entered in	*
C* SNEFIL separated by |.						*
C*									*
C* There might be up to 11 station information parameters that must	*
C* be listed first, in any order, in the conversion table for surface	*
C* parameters (SFPRMF).  These must have the names given in the table	*
C* below:								*
C*									*
C* GEMPAK name	Description	Units/type  Required?	Name in BFRRDD  *
C*									*
C*   STID	Identifier	 CHARACTER     NO	  STID		*
C*   BLOK       Block number     INTEGER       NO         BLOK          *
C*   STNM       Identifier	 INTEGER       NO	  ISTNM		*
C    SLAT	Latitude	 Degrees       YES	  SLAT		*
C*   SLON	Longitude	 Degrees       YES	  SLON		*
C*   SELV	Elevation	 Meters	       NO	  SELV		*
C*   YYYY       Year             INTEGER       NO         DATTIM        *
C*   MMMM       Month            INTEGER       NO         DATTIM        *
C*   DDDD       Day              INTEGER       NO         DATTIM        *
C*   HHHH       Hour             INTEGER       NO         DATTIM        *
C*   NNNN       Minute           INTEGER       NO         DATTIM        *
C*   TOFF       Offset time	 Seconds       NO         IHHMM 	*
C*   TADD       Offset time      Seconds       NO         DATTIM        *
C*									*
C* If the block number (BLOK) is indicated, it will be combined with    *
C* the STNM to form ISTNM in BFRRDD; otherwise, STNM alone is used to	*
C* to determine ISTNM, if it is present.  The value of ISTNM defaults	*
C* to an arbitrary sequence number.					*
C*									*
C* The first non-station information surface or single-level parameter	*
C* is stored in PARMSF (ISTRTF).					*
C*									*
C* If TIMSTN is set to a single number, a surface ship file is opened	*
C* with that number as the maximum reports.  If a sounding file is	*
C* also requested, the single number is the maximum number of stations, *
C* and the maximum number of times defaults to 24.			*
C*									*
C* If the sounding data parameter list terminates with the name LFLG,	*
C* then a standard sounding observation is assumed.  The sounding	*
C* parameter list must contain the following GEMPAK parameter names in	*
C* the order shown below:						*
C*									*
C* 		PRES HGHT TEMP DWPT DRCT SPED LFLG			*
C*									*
C* An unmerged sounding data file will be created.  LFLG will be checked*
C* to make the following correspondences:				*
C*									*
C*	INTEGER VALUE		DATA TYPE				*
C*									*
C*	    64			Surface					*
C*	    16			Tropopause level			*
C*	     8			Maximum wind level			*
C*	    32			Mandatory level				*
C*	     4			Significant temperature level		*
C*	     2			Significant wind level			*
C*									*
C* In this case, UPAFLG is set to true.					*
C*									*
C* If the sounding data parameter list terminates with the name LCAT,	*
C* then a standard sounding observation is assumed.  The sounding	*
C* parameter list must contain the following GEMPAK parameter names in	*
C* the order shown below:						*
C*									*
C* 		PRES HGHT TEMP DWPT DRCT SPED LCAT			*
C*									*
C* An unmerged sounding data file will be created.  LCAT will be checked*
C* to make the following correspondences:				*
C*									*
C*	INTEGER VALUE		DATA TYPE				*
C*									*
C*	     0			Surface					*
C*	     5			Tropopause level			*
C*	     1			Mandatory level				*
C*	     2			Significant temperature level		*
C*	3 or 4			Significant wind level			*
C*									*
C* In this case, CATFLG is set to true.					*
C* Thses are the still-used Office Note 29 categories.			*
C*									*
C*									*
C* BFROPN  ( SNEFIL, SNOUTF, SFOUTF, SNPRMF, SFPRMF, TIMSTN,		*
C*	     MXNBSN, MXNBSF,						*
C*	     LUNIN, LUNSN, LUNSF, NPRMSN, NPRMSF, SNCFAC, SFCFAC,	*
C*	     SNCTRM, SFCTRM, PARMSN, PARMSF, BPRMSN, BPRMSF, IREPSF,	*
C*	     NBSN, NBSF, ISTRTF, SHIP, UPAFLG, CATFLG, IRET )		*
C*									*
C* Input parameters:							*
C*	SNEFIL		CHAR*		BUFR file name | table file name*
C*	SFFSRC		CHAR*		BUFR message type		*
C*	SNOUTF		CHAR*		Output GEMPAK sounding file name*
C*	SFOUTF		CHAR*		Output GEMPAK surface file name	*
C*	SNPRMF		CHAR*		Sounding parm conversion file   *
C*	SFPRMF		CHAR*		Surface parm conversion file	*
C*	TIMSTN		CHAR*		Maximum times / stations	*
C*	MXNBSN		INTEGER		Maximum for NBSN		*
C*	MXNBSF		INTEGER		Maximum for NBSF		*
C*									*
C* Output parameters:							*
C*	LUNIN		INTEGER		BUFR file unit number		*
C*	LUNSN		INTEGER		Output snd data file number	*
C*	LUNSF		INTEGER		Output sfc data file number	*
C*	NPRMSN		INTEGER		Number of sounding parms	*
C*	NPRMSF		INTEGER		Number of surface parms		*
C*	SNCFAC (NPRMSN) REAL		Conversion factors for snd data	*
C*	SFCFAC (NPRMSF) REAL		Conversion factors for sfc data *
C*	SNCTRM (NPRMSN) REAL		Conversion terms for snd data	*
C*	SFCTRM (NPRMSF) REAL		Conversion terms for sfc data	*
C*	PARMSN (NPRMSN) CHAR*4		Sounding parm list		*
C*	PARMSF (NPRMSF) CHAR*4		Surface parm list		*
C*	BPRMSN (NBSN)	CHAR*80		BUFR sounding parm list		*
C*	BPRMSF (NBSF)	CHAR*80		BUFR surface parm list		*
C*	IREPSF (NBSF)	INTEGER		Replication #'s for sfc parms	*
C*	NBSN		INTEGER		# of 80-char sounding prm lists *
C*	NBSF		INTEGER		# of 80-char surface prm lists	*
C*	ISTRTF		INTEGER		Index of 1st real parm in PARMSF*
C*	SHIP		LOGICAL		Flag for ship surface file	*
C*	UPAFLG		LOGICAL		Flag for upper obs		*
C*	CATFLG		LOGICAL		Flag for ON 29 categories	*
C*	IRET		INTEGER		Return code			*
C*					 +2 = No parms for sfc file	*
C*					  0 = normal			*
C*					 -4 = no BUFR table file	*
C*					 -5 = BUFR file open failed	*
C*					 -6 = BG_PARM failed for snd	*
C*					 -7 = SNOUTF parms != PARMSN	*
C*					 -8 = SNOUTF cannot be opened	*
C*					 -9 = BG_PARM failed for sfc	*
C*					-10 = SFOUTF parms 1= PARMSF	*
C*					-11 = SFOUTF cannot be opened	*
C*					-12 = No station information	*
C*					-18 = Too many sounding parms	*
C*					-19 = Too many surface parms	*
C*					-29 = File is merged		*
C*					-30 = Cannot create unmerged fl *
C*					-34 = Standard parms not correct*
C**									*
C* Log:									*
C* K. Brill/EMC		 2/97						*
C* K. Brill/EMC		 2/97	SHIP and UPAFLG				*
C* K. Brill/EMC		 4/97	Added SFFSRC				*
C* K. Brill/EMC		 7/98	Changes for CATFLG (PREPBUFR--ON 29)	*
C* K. Brill/HPC		 3/01	Use ST_FIND for LFGL and LCAT		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	snefil, snoutf, sfoutf, snprmf, sfprmf, timstn
	CHARACTER*(*)	sffsrc
	CHARACTER*(*)   parmsn (*), parmsf (*)
	CHARACTER*80	bprmsn (*), bprmsf (*)
	INTEGER		irepsf (*)
	REAL		sncfac (*), sfcfac (*)
	REAL		snctrm (*), sfctrm (*)
	LOGICAL		ship, upaflg, catflg
C*
        INTEGER         iscl (MMPARM), iref (MMPARM), ibit (MMPARM)
	INTEGER		ilist (2)
        CHARACTER       filprm (MMPARM)*4
	CHARACTER	snef(2)*256
	LOGICAL		mrgdat
C----------------------------------------------------------------------
	iret  = 0
	upaflg = .false.
	catflg = .false.
	ship = .false.
	lunsn = 0
	lunsf = 0
	nprmsn = 0
	nprmsf = 0
C
C*	Check for existence of BUFR table file.
C
	iq = INDEX ( snefil, '|' )
	IF ( iq. eq. 0 ) THEN
	    iret = -4
	    RETURN
	END IF
	CALL ST_CLST ( snefil, '|', ' ', 2, snef, num, ier )
	IF ( num .ne. 2 ) THEN
	    iret = -4
	    RETURN
	END IF
C
C*	Open the BUFR input file.
C
	CALL FL_GLUN ( lunin, ier )
	CALL JB_OPEN ( snef (1), lunin, sffsrc, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -5
	    RETURN
	END IF
C
C*      Get the number of times and the number of stations to
C*      add.
C
        CALL ST_ILST  ( timstn, '/', IMISSD, 2, ilist, n,
     +		        ier )
	IF ( n .eq. 1 ) THEN
	    maxtim = 24
	    maxstn = ilist (1)
	    ship = .true.
        ELSE
	    ship = .false.
	    IF  ( ilist (1) .eq. IMISSD )  THEN
		maxtim = 24
	    ELSE
		maxtim = ilist (1)
	    END IF
	    IF  ( ilist (2) .eq. IMISSD )  THEN
                maxstn = LLSTFL
            ELSE
                maxstn = ilist (2)
            END IF
	END IF
C
C*	Open the sounding output file.
C
	IF ( snoutf .ne. ' ' ) THEN
C
C*	    First get the packing information.
C
	    nbsn = mxnbsn
	    nprmsn = MMPARM
	    CALL BG_PARM ( snef (2), snprmf, nbsn, nprmsn,
     +			   bprmsn, irepsf, parmsn,
     +			   iscl, iref, ibit, sncfac, snctrm, ier )
	    IF ( ier .ne. 0 ) THEN
		iret = -6
		IF ( ier .eq. -10 ) iret = -18
		RETURN
	    END IF
	END IF
	CALL ST_FIND ( 'LFLG', parmsn, nprmsn, iq, ier )
	upaflg = ( parmsn (1) .eq. 'PRES' .and.
     +	           parmsn (2) .eq. 'HGHT' .and.
     +	           parmsn (3) .eq. 'TEMP' .and.
     +	           parmsn (4) .eq. 'DWPT' .and.
     +	           parmsn (5) .eq. 'DRCT' .and.
     +	           parmsn (6) .eq. 'SPED' .and.
     +	           parmsn (7) .eq. 'LFLG' )
	IF ( iq .ne. 0 .and. .not. upaflg ) THEN
	    iret = -34
	    RETURN
	END IF
	CALL ST_FIND ( 'LCAT', parmsn, nprmsn, iq, ier )
	catflg = ( parmsn (1) .eq. 'PRES' .and.
     +	           parmsn (2) .eq. 'HGHT' .and.
     +	           parmsn (3) .eq. 'TEMP' .and.
     +	           parmsn (4) .eq. 'DWPT' .and.
     +	           parmsn (5) .eq. 'DRCT' .and.
     +	           parmsn (6) .eq. 'SPED' .and.
     +	           parmsn (7) .eq. 'LCAT' )
	IF ( iq .ne. 0 .and. .not. catflg ) THEN
	    iret = -34
	    RETURN
	END IF
C*
	IF ( upaflg .or. catflg ) THEN
C
C*	    This file as an unmerged data file.
C
	    CALL SN_OPNF ( snoutf, .true., lunsn, iflsrc, npsn,
     +			   filprm, ivert, mrgdat, ier )
	    IF ( ier .eq. 0 ) THEN
	        IF ( mrgdat ) THEN
		    iret = -29
		    RETURN
		END IF
	    ELSE
		CALL SN_CRUA ( snoutf, MFUNKN, 3, maxstn, maxtim,
     +			       .true., .true., .false., lunsn,
     +			       ier )
		IF ( ier .ne. 0 ) THEN
		    iret = -30
		    RETURN
		END IF
	    END IF
	ELSE IF ( snoutf .ne. ' ' ) THEN
C*
	    CALL SN_OPNF ( snoutf, .true., lunsn, iflsrc, npsn,
     +			   filprm, ivert, mrgdat, ier )
	    IF ( ier .eq. 0 ) THEN
	    	IF ( npsn .ne. nprmsn ) THEN
		    iret = -7
		    RETURN
	    	END IF
	    	DO i = 1, npsn
		    IF ( filprm (i) .ne. parmsn (i) ) THEN
		    	iret = -7
		    	RETURN
		    END IF
	    	END DO
	    ELSE
C
C*	        Create a new sounding data file.
C
		iflsrc = MFUNKN
	    	CALL SN_CREF ( snoutf, iflsrc, nprmsn, parmsn,
     +			       maxstn, maxtim,
     +			       .true., iscl, iref, ibit, .true.,
     +			       lunsn, ier )
	    	IF ( ier .ne. 0 ) THEN
		    iret = -8
		    RETURN
	    	END IF
	    END IF
	END IF
C
C*	First get the surface data parameters.  There must be single
C*	level parameter information even if there is no surface data
C*	output.
C
	nbsf = mxnbsf
	nprmsf = MMPARM + 13
	CALL BG_PARM ( snef (2), sfprmf, nbsf, nprmsf,
     +		       bprmsf, irepsf, parmsf,
     +		       iscl, iref, ibit, sfcfac, sfctrm, ier )
	IF ( ier .ne. 0 ) THEN
	    WRITE (6,*) ' Error from BG_PARM-> iret = ', ier
	    iret = -9
	    IF ( ier .eq. -10 ) iret = -19
	    RETURN
	END IF
	istrtf = 0
	istp = MIN ( nprmsf, 13 )
	ireq = 0
	DO i = 1, istp
	    IF ( parmsf (i) .eq. 'STID' ) istrtf = i
	    IF ( parmsf (i) .eq. 'BLOK' ) istrtf = i
	    IF ( parmsf (i) .eq. 'STNM' ) istrtf = i
	    IF ( parmsf (i) .eq. 'SLAT' ) THEN 
		istrtf = i
		ireq = ireq + 1
	    END IF
	    IF ( parmsf (i) .eq. 'SLON' ) THEN
		istrtf = i
		ireq = ireq + 1
	    END IF
	    IF ( parmsf (i) .eq. 'SELV' ) istrtf = i
	    IF ( parmsf (i) .eq. 'YYYY' ) istrtf = i
	    IF ( parmsf (i) .eq. 'MMMM' ) istrtf = i
	    IF ( parmsf (i) .eq. 'DDDD' ) istrtf = i
	    IF ( parmsf (i) .eq. 'HHHH' ) istrtf = i
	    IF ( parmsf (i) .eq. 'NNNN' ) istrtf = i
	    IF ( parmsf (i) .eq. 'TOFF' ) istrtf = i
	    IF ( parmsf (i) .eq. 'TADD' ) istrtf = i
	END DO
	IF ( ireq .ne. 2 ) THEN
	    iret = -12
	    RETURN
	END IF
	ioff = istrtf
	istrtf = istrtf + 1
C
C*	Open the surface output file(s).
C
	IF ( sfoutf .ne. ' ' ) THEN
	    IF ( nprmsf .lt. istrtf ) THEN
		iret = +2
		RETURN
	    ENDIF
C*
	    CALL SF_OPNF ( sfoutf, .true., lunsf, iflsrc,
     +			   npsf, filprm, ier )
	    IF ( ier .eq. 0 ) THEN
		nchck = nprmsf - ioff
	    	IF ( npsf .ne. nchck ) THEN
		    iret = -10
		    RETURN
	    	END IF
	    	DO i = 1, npsf
		    IF ( filprm (i) .ne. parmsf (i+ioff) ) THEN
		    	iret = -10
		    	RETURN
		    END IF
	    	END DO
	    ELSE IF ( .not. ship ) THEN
C
C*	    	Create a new surface data file.
C
		iflsrc = MFUNKN
		nprm = nprmsf - ioff
	    	CALL SF_CREF ( sfoutf, iflsrc, nprm, parmsf (istrtf),
     +			       maxstn, maxtim,
     +			       .true., iscl (istrtf), iref (istrtf),
     +			       ibit (istrtf), .true., lunsf, ier )
	    	IF ( ier .ne. 0 ) THEN
		    iret = -11
		    RETURN
	    	END IF
	    ELSE
C
C*		Create a new ship data file.
C
		iflsrc = MFUNKN
		nprm = nprmsf - ioff
	    	CALL SF_CSDF ( sfoutf, iflsrc, nprm, parmsf (istrtf),
     +			       maxstn,
     +			       .true., iscl (istrtf), iref (istrtf),
     +			       ibit (istrtf), .true., lunsf, ier )
	    	IF ( ier .ne. 0 ) THEN
		    iret = -11
		    RETURN
	    	END IF
		
	    END IF
	END IF
C*
	RETURN
	END
