	SUBROUTINE SN_DDAT  ( isnfln, iret )
C************************************************************************
C* SN_DDAT								*
C*									*
C* This subroutine deletes data for a particular station and time	*
C* from a sounding data file.  The time and station must be set		*
C* before calling this subroutine. 					*
C*									*
C* SN_DDAT  ( ISNFLN, IRET )						*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	 	Return code			*
C*				    	   0 = normal return		*
C*					  -7 = location not set		*
C*					 -15 = delete error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/88						*
C* D. Kidwell/NCEP	 2/01	Added more parts 			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'
C------------------------------------------------------------------------
	CALL SN_CHKF  ( isnfln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Check that station is set.
C
	IF ( (krow (isnfln) .le. 0) .or. (kcol (isnfln) .le. 0) ) THEN
	    iret = -7
	    RETURN
	END IF
C
C*	Delete the data.
C
	IF  ( mrgtyp (isnfln) )  THEN
	    CALL DM_DDAT  ( isnfln, krow (isnfln), kcol (isnfln), 
     +			    'SNDT', ier )
	    IF  ( ier .ne. 0 )  iret = -15
	  ELSE
	    CALL DM_DDAT  ( isnfln, krow (isnfln), kcol (isnfln), 
     +			    'TTAA', ier1 )
	    CALL DM_DDAT  ( isnfln, krow (isnfln), kcol (isnfln), 
     +			    'TTBB', ier2 )
	    CALL DM_DDAT  ( isnfln, krow (isnfln), kcol (isnfln), 
     +			    'TTCC', ier3 )
	    CALL DM_DDAT  ( isnfln, krow (isnfln), kcol (isnfln), 
     +			    'TTDD', ier4 )
	    CALL DM_DDAT  ( isnfln, krow (isnfln), kcol (isnfln), 
     +			    'PPBB', ier5 )
	    CALL DM_DDAT  ( isnfln, krow (isnfln), kcol (isnfln), 
     +			    'PPDD', ier6 )
	    CALL DM_DDAT  ( isnfln, krow (isnfln), kcol (isnfln), 
     +			    'PPAA', ier7 )
	    CALL DM_DDAT  ( isnfln, krow (isnfln), kcol (isnfln), 
     +			    'PPCC', ier8 )
	    CALL DM_DDAT  ( isnfln, krow (isnfln), kcol (isnfln), 
     +			    'TRPA', ier9 )
	    CALL DM_DDAT  ( isnfln, krow (isnfln), kcol (isnfln), 
     +			    'TRPC', ier10 )
	    CALL DM_DDAT  ( isnfln, krow (isnfln), kcol (isnfln), 
     +			    'MXWA', ier11 )
	    CALL DM_DDAT  ( isnfln, krow (isnfln), kcol (isnfln), 
     +			    'MXWC', ier12 )
	    CALL DM_DDAT  ( isnfln, krow (isnfln), kcol (isnfln), 
     +			    'TXTA', ier13 )
	    CALL DM_DDAT  ( isnfln, krow (isnfln), kcol (isnfln), 
     +			    'TXTB', ier14 )
	    CALL DM_DDAT  ( isnfln, krow (isnfln), kcol (isnfln), 
     +			    'TXPB', ier15 )
	    CALL DM_DDAT  ( isnfln, krow (isnfln), kcol (isnfln), 
     +			    'TXTC', ier16 )
	    IF  ( ( ier1 .ne. 0 ) .and. ( ier2 .ne. 0 ) .and. 
     +		  ( ier3 .ne. 0 ) .and. ( ier4 .ne. 0 ) .and.
     +		  ( ier5 .ne. 0 ) .and. ( ier6 .ne. 0 ) .and.
     +		  ( ier7 .ne. 0 ) .and. ( ier8 .ne. 0 ) .and.
     +		  ( ier9 .ne. 0 ) .and. ( ier10 .ne. 0 ) .and.
     +		  ( ier11 .ne. 0 ) .and. ( ier12 .ne. 0 ) .and.
     +		  ( ier13 .ne. 0 ) .and. ( ier14 .ne. 0 ) .and.
     +		  ( ier15 .ne. 0 ) .and. ( ier16 .ne. 0 ) )  iret = -15
	END IF
C*
	RETURN
	END
