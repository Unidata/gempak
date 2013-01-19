	SUBROUTINE DG_QTMS  ( mxtms, both, tmlst, ntms, trange, iret )
C************************************************************************
C* DG_QTMS								*
C*									*
C* This subroutine retrieves the list of times stored in the NFILE	*
C* block of DGCMN.CMN.							*
C*									*
C* If BOTH is false, only the first times are returned.  If BOTH is	*
C* .true. the first and second times are returned in the same element	*
C* of tmlst with : between them.  If the second time is blank, no : is	*
C* included.								*
C*									*
C* DG_QTMS  ( MXTMS, BOTH, TMLST, NTMS, TRANGE, IRET )			*
C*									*
C* Input parameters:							*
C*	MXTMS		INTEGER		Maximum # of times to return	*
C*      BOTH		LOGICAL		Flag to return two times with : *
C*									*
C* Output parameters:							*
C*	TMLST (*)	CHAR*		List of times			*
C*	NTMS		INTEGER		Number of times			*
C*      TRANGE          CHAR*           Range of grid times             *
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C**									*
C* Log:									*
C* R. Tian/SAIC          3/06   Fortran wrapper of DGC_QTMS             *
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
C*	
	LOGICAL		both
	CHARACTER*(*)	tmlst (*), trange
C*
	CHARACTER	tmptml*(LLMXGT*36), timarr (LLMXGT)*36
C------------------------------------------------------------------------
	CALL DGC_QTMS ( mxtms, both, tmptml, ntimes, trange, iret )
	CALL ST_RNUL ( tmptml, tmptml, nt, ier )
	CALL ST_RNUL ( trange, trange, nt, ier )
	IF ( ntimes .eq. 1 ) THEN
	    ntms = 1
	    tmlst (1) = tmptml
	  ELSE
	    CALL ST_CLSL ( tmptml, ';', ' ', LLMXGT, timarr, ntms, ier )
	    DO i = 1, ntms
		tmlst (i) = timarr (i)
	    END DO
	END IF
C*
	RETURN
	END
