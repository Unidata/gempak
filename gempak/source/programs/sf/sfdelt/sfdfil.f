	SUBROUTINE SFDFIL  ( sffile, sffcur, iflno, ntdset, timdst,
     +			     iret )
C************************************************************************
C* SFDFIL								*
C*									*
C* This subroutine opens the file for SFDELT.				*
C*									*
C* SFDFIL  ( SFFILE, SFFCUR, IFLNO, NTDSET, TIMDST, IRET )		*
C*									*
C* Input parameters:							*
C*	SFFILE		CHAR*		Surface file name		*
C*									*
C* Input and output parameters:						*
C*	SFFCUR		CHAR*		Current output file		*
C*	IFLNO		INTEGER		Surface file number		*
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
	CHARACTER*(*)	sffile, sffcur, timdst (*)
C*
	CHARACTER	prms (MMPARM)*4
C----------------------------------------------------------------------
	iret = 0
C
C*	Check to see if this is the same file.
C
	IF  ( ( sffile .ne. sffcur ) .or. ( sffile .eq. ' ' ) )  THEN
C
C*	    Close existing file.
C
	    IF  ( iflno .ne. 0 )  CALL SF_CLOS  ( iflno, ier )
	    iflno  = 0
	    sffcur = ' '
C
C*	    Open the file.
C
	    CALL SF_OPNF  ( sffile, .true., iflno, isrc, nprm, prms, 
     +			    ier )
	    IF  ( ier .ne. 0 )  THEN
		iret = -1
	        RETURN
	    END IF
C
C*	    Save new file name.
C
	    sffcur = sffile
	END IF	    
C
C*	Get the current times from the file.
C
	CALL SF_GTIM  ( iflno, LLMXTM, ntdset, timdst, ier )
C*
	RETURN
	END
