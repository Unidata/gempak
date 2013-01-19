	SUBROUTINE OAGOPN  ( filnam, source, dattim, iflno, 
     +			     time, ivert, iret )
C************************************************************************
C* OAGOPN								*
C*									*
C* This subroutine opens the file to be used for computing the 		*
C* station spacing.							*
C*									*
C* OAGOPN  ( FILNAM, SOURCE, DATTIM, IFLNO, TIME, IVERT,		*
C*           IRET )							*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		File name			*
C*	SOURCE		CHAR*		Data type ( SN or SF )		*
C*	DATTIM		CHAR*		Input date/time			*
C*									*
C* Output parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	TIME		CHAR*		Date/time used			*
C*	IVERT		INTEGER		File vertical coordinate	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-11 = file not opened		*
C*					-12 = invalid time		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/85						*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* S. Schotz/GSC	 6/90	Removed respnd flag			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	filnam, source, dattim, time
C*
	LOGICAL		mrgdat
	CHARACTER	timlst (LLMXTM)*20, chparm (MMPARM)*4
C------------------------------------------------------------------------
	iret = 0
C
C*	Open new file.
C
	IF  ( source .eq. 'SN' )  THEN
	    CALL SN_OPNF  ( filnam, .false., iflno, isrc, nparm, 
     +			    chparm, ivert, mrgdat, iret )
	    IF  ( iret .ne. 0 ) THEN
		iret = -11
		CALL ER_WMSG  ( 'OAGRID', iret, filnam, ier )
		RETURN
	      ELSE
	        CALL PC_INIT  ( ivert, nparm, chparm, ier )
	        CALL SN_BEGS  ( iflno, ier )
	    END IF
C*
	  ELSE
	    CALL SF_OPNF  ( filnam, .false., iflno, isrc, nparm, chparm,
     +			    iret )
	    ivert = 0
	    IF  ( iret .ne. 0 ) THEN
		iret = -11
		CALL ER_WMSG  ( 'OAGRID', iret, filnam, ier )
		RETURN
	      ELSE
	        CALL PC_INIT  ( 0, nparm, chparm, ier )
	        CALL SF_BEGS  ( iflno, ier )
	    END IF
	END IF
C
C*	Get the times in the file and set the time correctly.
C
	IF  ( source .eq. 'SN' )  THEN
	    CALL SN_GTIM  ( iflno, LLMXTM, numtim, timlst, ier )
	    CALL TI_FIND  ( dattim, numtim, timlst, dattim,
     +			    nt, time, ier )
	    IF  ( ( ier .ne. 0 ) .or. ( nt .lt. 1 ) )  THEN
		iret = -12
		CALL ER_WMSG  ( 'OAGRID', iret, dattim, ier )
		RETURN
	    END IF
C
C*	    Set the time.
C
	    CALL SN_STIM  ( iflno, time, iret )
	  ELSE
	    CALL SF_GTIM  ( iflno, LLMXTM, numtim, timlst, iret )
	    CALL TI_FIND  ( dattim, numtim, timlst, dattim,
     +			    nt, time, ier )
	    IF  ( ( ier .ne. 0 ) .or. ( nt .lt. 1))  THEN
		iret = -12
		CALL ER_WMSG  ( 'OAGRID', iret, dattim, ier )
	    END IF
C
C*	    Set the time.
C
	    CALL SF_STIM (iflno, time, iret )
	END IF
C*
	RETURN
	END
