	SUBROUTINE OANOPN  ( snfile, filnam, times, ntimes,
     +			     nfile, prmsn, nprmsn, ivcin, iret )
C************************************************************************
C* OANOPN								*
C*									*
C* This subroutine opens up to MMFILE upper-air files for an objective	*
C* analysis.  The parameter list for each file is returned.		*
C*									*
C* OANOPN  ( SNFILE, FILNAM, TIMES, NTIMES, NFILE, PRMSN, NPRMSN,	*
C*           IVCIN, IRET )						*
C*									*
C* Input parameters:							*
C*	SNFILE		CHAR*		Upper-air file name		*
C*									*
C* Output parameters:							*
C*	FILNAM	( * )	CHAR*		Upper-air file names		*
C*	TIMES 		CHAR*		Times				*
C*       (LLMXTM, * )                                                   *
C*	NTIMES	( * )	INTEGER		Number of times			*
C*      NFILE           INTEGER         Number of files			*
C*	PRMSN		CHAR*           Parameters in files		*
C*      (MMPARM, MMFILE)						*
C*	NPRMSN (MMFILE)	INTEGER		Number of parms in files	*
C*	IVCIN  (MMFILE)	INTEGER         Vertical coord numbers		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					 -1 = error 			*
C*					-11 = SN file could not be opnd	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/86						*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* K. Brill/GSC          4/90   Added code for multiple files		*
C* K. Brill/NMC          8/90   Fixed code for multiple files		*
C* T. Lee/GSC		 3/99	Returned file names instead in call seq	*
C* T. Piper/GSC	 	 3/99	Corrected prolog			*
C* R. Tian/SAIC		 3/05	Removed SNCUR				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	snfile, times (LLMXTM, *), 
     +                  prmsn (MMPARM, MMFILE), filnam (*)
	INTEGER         isnfln (MMFILE), ntimes ( * ), nprmsn (*),
     +			ivcin (*)
C*
	LOGICAL		mrgdat (MMFILE)
C------------------------------------------------------------------------
	iret = 0
C
C*	Get the individual input file names.
C
	CALL ST_CLST ( snfile, '+', ' ', MMFILE, filnam, nfile, ier )
	IF ( nfile .le. 0 ) THEN
	  nfile = 1
	  filnam (1) = ' '
	END IF
C*
	kfil = 0
	ktim = 0
C
C*      Now open these files.
C
	DO i = 1, nfile
C*
	    CALL SN_OPNF  ( filnam (i), .false., isnfln (i), iflsrc,
     +			    nprmsn (i), prmsn (1,i), ivcin (i),
     +                      mrgdat (i), iret )
	    IF  ( iret .eq. 0 )  THEN
C
C*		Get the times.
C
		CALL SN_GTIM  ( isnfln (i), LLMXTM, ntimes (i), 
     +				times (1, i), iret )
C
C*		If no times in the file, reset the parameters.
C
		IF  ( ntimes (i) .le. 0 )  THEN
		    CALL ER_WMSG  ( 'OABSND', +11, filnam (i), ier )
		    CALL SN_CLOS  ( isnfln (i), ier )
		    filnam (i) = ' '
		    DO j = 1, MMPARM
			prmsn ( j, i ) = ' '
		    END DO
		    nprmsn (i) = 0
		    isnfln (i) = 0
		    ivcin  (i) = 0
		    ktim = ktim + 1
		  ELSE
		    CALL SN_CLOS ( isnfln (i), ier )
		END IF
	      ELSE
C
C*		If the file does not exist, write warning message.
C
		CALL ER_WMSG  ( 'OABSND', +12, filnam (i), ier )
		filnam (i) = ' '
		kfil = kfil + 1
		iret = 0
	    END IF
	END DO
C
C*	If input files do not exist or there are no times in the
C*	files, return.
C
	IF  ( kfil .eq. 0 .and. ktim .eq. 0 )  THEN
	    RETURN
	  ELSE IF  ( ktim .eq. nfile )  THEN
	    CALL ER_WMSG ( 'OABSND', -8, ' ', ier )
	    iret = -1
	    RETURN
	  ELSE IF ( kfil .eq. nfile .or. (kfil+ktim) .eq. nfile )  THEN
	    CALL ER_WMSG ( 'OABSND', -11, ' ', ier )
	    iret = -1
	    RETURN
	END IF
C
C*	Rearrange parameters if blank files exist.
C
	k = 1
	DO WHILE ( k .le. nfile )
	    IF  ( filnam (k) .eq. ' ' )  THEN
		DO  i = k, nfile - 1
		    filnam (i) = filnam (i+1)
		    isnfln (i) = isnfln (i+1)
		    nprmsn (i) = nprmsn (i+1)
		    ivcin  (i) = ivcin  (i+1)
		    mrgdat (i) = mrgdat (i+1)
		    DO  j = 1, MMPARM
			prmsn (j, i) = prmsn (j, i+1)
		    END DO
		    DO j = 1, LLMXTM
			times (j, i) = times (j, i+1)
		    END DO
		END DO	    
		nfile = nfile - 1
	      ELSE
		k = k + 1
	    END IF
	END DO
C*
	RETURN
	END
