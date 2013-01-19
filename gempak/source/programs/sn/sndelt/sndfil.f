	SUBROUTINE SNDFIL  ( snfile, snfcur, iflno, ntdset, timdst,
     +			     iret )
C************************************************************************
C* SNDFIL								*
C*									*
C* This subroutine opens the file for SNDELT.				*
C*									*
C* SNDFIL  ( SNFILE, SNFCUR, IFLNO, NTDSET, TIMDST, IRET )		*
C*									*
C* Input parameters:							*
C*	SNFILE		CHAR*		Sounding file name		*
C*									*
C* Input and output parameters:						*
C*	SNFCUR		CHAR*		Current output file		*
C*	IFLNO		INTEGER		Sounding file number		*
C*									*
C* Output parameters:							*
C*	NTDSET		INTEGER		Number of times in file		*
C*	TIMDST (NTDSET)	CHAR*		Times in file			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					 -1 = file not opened		*
C**									*
C* Log:									*
C* I. Graffman/RDS	10/85						*
C* M. desJardins/GSFC	10/86	Added GEMPAK parameter names		*
C* M. desJardins/GSFC	 6/88	Rewrote					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	snfile, snfcur, timdst (*)
C*
	CHARACTER	prms (MMPARM)*4
	LOGICAL         mrgdat
C----------------------------------------------------------------------
	iret = 0
C
C*	Check to see if this is the same file.
C
	IF  ( ( snfile .ne. snfcur ) .or. ( snfile .eq. ' ' ) )  THEN
C
C*	    Close existing file.
C
	    IF  ( iflno .ne. 0 )  CALL SN_CLOS  ( iflno, ier )
	    iflno  = 0
	    snfcur = ' '
C
C*	    Open the file.
C
	    CALL SN_OPNF  ( snfile, .true., iflno, isrc, nprm, prms, 
     +			    ivert, mrgdat, ier )
	    IF  ( ier .ne. 0 )  THEN
		iret = -1
	        RETURN
	    END IF
C
C*	    Save new file name.
C
	    snfcur = snfile
	END IF	    
C
C*	Get the current times from the file.
C
	CALL SN_GTIM  ( iflno, LLMXTM, ntdset, timdst, ier )
C*
	RETURN
	END
