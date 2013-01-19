	SUBROUTINE OACOPN  ( sffile, filnam, times, ntimes, 
     +			     nfile, prmsf, nprmsf, iret )
C************************************************************************
C* OACOPN								*
C*									*
C* This subroutine opens the surface files for an objective analysis.	*
C* A maximum of MMFILE files can be opened at one time.  The parameter  *
C* list for each file is returned.					*
C*									*
C* OACOPN ( SFFILE, FILNAM, TIMES, NTIMES, NFILE, PRMSF, NPRMSF, IRET )	*
C*									*
C* Input parameters:							*
C*	SFFILE			CHAR*		Surface file name	*
C*									*
C* Output parameters:							*
C*	FILNAM	( * )		CHAR*		Surface file names	*
C*	TIMES ( LLMXTM, * )	CHAR*		Times			*
C*	NTIMES	( * )		INTEGER		Number of times		*
C*      NFILE           	INTEGER         Number of files		*
C*      PRMSF (MMPARM, MMFILE)	CHAR*           Parameters in the files	*
C*	NPRMSF (NFILE) 		INTEGER         # of parms in the file	*
C*	IRET			INTEGER		Return code		*
C*						  0 = normal		*
C*						 -1 = error 		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/86						*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* K. Brill/GSC          4/90   Added code for multiple files		*
C* K. Brill/NMC          8/90   Fixed code for multiple files		*
C* T. Lee/GSC		 2/99	Returned file names instead in call seq	*
C* T. Lee/GSC		 3/99	Handled missing data			*
C* R. Tian/SAIC		 3/05	Removed sfcur				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	sffile, times (LLMXTM, *),
     +                  prmsf (MMPARM, MMFILE), filnam (*)
	INTEGER         isffln (MMFILE), ntimes ( * ), nprmsf (*)
C*
C------------------------------------------------------------------------
	iret = 0
C
C*	Get the individual input file names.
C
	CALL ST_CLST ( sffile, '+', ' ', MMFILE, filnam, nfile, ier )
	IF ( nfile .le. 0 ) THEN
	  nfile = 1
	  filnam (1) = ' '
	END IF
C*
	kfil = 0
	ktim = 0
C
	DO i = 1, nfile
C
C*	    Open the file.
C
	    CALL SF_OPNF  ( filnam (i), .false., isffln (i), iflsrc,
     +			    nprmsf(i), prmsf (1,i), iret )
	    IF  ( iret .eq. 0 )  THEN
C
C*		Get the times.
C
		CALL SF_GTIM  ( isffln (i), LLMXTM, ntimes (i),
     +				times (1, i), iret )
C
C*		If no times in the file, reset the parameters.
C
		IF  ( ntimes (i) .le. 0 )  THEN
		    CALL ER_WMSG  ( 'OABSFC', +10, filnam (i), ier )
		    CALL SF_CLOS ( isffln (i), ier )
		    filnam (i) = ' '
		    DO j = 1, MMPARM
			prmsf ( j, i ) = ' '
		    END DO
		    nprmsf (i) = 0
		    isffln (i) = 0
		    ktim = ktim + 1
		  ELSE
		    CALL SF_CLOS ( isffln (i), ier )
		END IF
	      ELSE
C
C*		If the file does not exist, write warning message.
C
		CALL ER_WMSG ( 'OABSFC', +11, filnam (i), ier )
		filnam (i) = ' '
		kfil = kfil + 1
		iret = 0
	    END IF
	END DO
C
C*	If input files do not exist or there are no times in the
C*	files, return.
C
	IF ( kfil .eq. 0 .and. ktim .eq. 0 )  THEN
	    RETURN
	  ELSE IF ( ktim .eq. nfile ) THEN
	    CALL ER_WMSG ( 'OABSFC', -8, ' ', ier )
	    iret = -1
	    RETURN
	  ELSE IF ( kfil .eq. nfile .or. (kfil+ktim) .eq. nfile )  THEN  
	    CALL ER_WMSG ( 'OABSFC', -10, ' ', ier )
	    iret = -1
	    RETURN
	END IF
C
C*	Rearrange parameters if blank files exist.	
C
	k = 1
	DO  WHILE (  k .le. nfile )
	    IF  ( filnam (k) .eq. ' ' )  THEN
		DO i = k, nfile - 1
		    filnam (i) = filnam (i+1)
		    isffln (i) = isffln (i+1)
		    nprmsf (i) = nprmsf (i+1)
		    ntimes (i) = ntimes (i+1)
		    DO j = 1, MMPARM
			prmsf (j, i) = prmsf (j, i+1)
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
