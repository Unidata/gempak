	SUBROUTINE SFMDAT  ( dattim, iflno, newfil, datcur,
     +			     ntime,  times, iret )
C************************************************************************
C* SFMDAT								*
C*									*
C* This subroutine processes the date/time entered by the user.		*
C*									*
C* SFMDAT  ( DATTIM, IFLNO, NEWFIL, DATCUR, NTIME, TIMES,		*
C*           IRET )							*
C*									*
C* Input parameters:							*
C*	DATTIM		CHAR*		Input date/time			*
C*	IFLNO		INTEGER		Surface file number		*
C*	NEWFIL		LOGICAL		New file flag			*
C*									*
C* Input and output parameters:						*
C*	DATCUR		CHAR*		Current date/time		*
C*									*
C* Output parameters:							*
C*	NTIME		INTEGER		Number of times to process	*
C*	TIMES (NTIME)	CHAR*		Times to process		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 7/87	GEMPAK4					*
C* I. Graffman/RDS	12/87	Removed time pointers			*
C* M. desJardins/GSFC	 6/88	Reorganized				*
C* G. Huffman/GSC	 1/89	Convert DATTIM with ST_LCUC		*
C* S. Schotz/GSC	 5/90	Removed respnd flag			*
C* P. Bruehl/Unidata	 8/94	Increased tout for long time strings	*
C* T. Piper/SAIC	12/01	Increased timdst to 20 for consistency	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	datcur, dattim, times (*)	
	LOGICAL		newfil
C*
	CHARACTER	tout*30, timdst (LLMXTM)*20
C-------------------------------------------------------------------------
	iret = 0
C
C*	Check for new input time.
C
	CALL ST_LCUC  ( dattim, dattim, ier )
	IF  ( ( dattim .ne. datcur ) .or. ( dattim .eq. 'LIST' ) .or.
     +	      ( dattim .eq. ' ' )    .or.   newfil )  THEN
C
C*	    Get times from file.
C
	    CALL SF_GTIM  ( iflno, LLMXTM, ntimf, timdst, iret )
	    IF  ( iret .ne. 0 )  THEN
		CALL ER_WMSG  ( 'SF', iret, ' ', ier )
		iret = -6
		RETURN
	    END IF
C
C*	    Find the time.
C
	    CALL TI_FIND  ( dattim, ntimf, timdst, tout, ntime,
     +			    times,  ier )
	    IF  ( ier .eq. 0 )  THEN
	        datcur = dattim
	      ELSE
		iret   = -6
		datcur = ' '
	    END IF
	END IF
C*
	RETURN
	END
