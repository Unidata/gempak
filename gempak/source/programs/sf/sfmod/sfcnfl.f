	SUBROUTINE SFCNFL  ( sfoutf, sfparm, sffcur, iflnew, ntnew,
     +                       timnew, ncols, ipos, iret )
C************************************************************************
C* SFCNFL								*
C*									*
C* This subroutine processes the output surface file.			*
C*									*
C* SFCNFL  ( SFOUTF, SFPARM, SFFCUR, IFLNEW, NTNEW, TIMNEW, NCOLS,      *
C*	     IPOS, IRET )						*
C*									*
C* Input parameters:							*
C*	SFOUTF		CHAR*		Output surface file name	*
C*	SFPARM		CHAR*		Output data set parameter names *
C*									*
C* Input and output parameters:						*
C*	SFFCUR		CHAR*		Current output file		*
C*	IFLNEW		INTEGER		Output file number		*
C*									*
C* Output parameters:							*
C*	NTNEW		INTEGER		Number of times in file		*
C*	TIMNEW (NTNEW)	CHAR*		Times in file			*
C*	NCOLS		INTEGER		Number of output parameters     *
C*	IPOS (*)	INTEGER		Positions of output parms       *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					 -1 = file not opened		*
C*					 -3 = no computable parameters	*
C**									*
C* Log:									*
C* I. Graffman/RDS	10/85						*
C* M. desJardins/GSFC	10/86	Added GEMPAK parameter names		*
C* M. desJardins/GSFC	 6/88	Rewrote					*
C* S. Schotz/GSC	10/90	Save value of nprm, prms		*
C* D. Kidwell/NCEP	 3/99	Added SFPARM processing                 *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	sfoutf, sffcur, timnew (*), sfparm
	INTEGER		ipos (*)
C*
	CHARACTER	prms (MMPARM)*4, prmlst (MMPARM)*4,
     +			prmcnd (MMPARM)*12, parms (MMPARM)*4
	LOGICAL		chrflg (MMPARM), cmpflg (MMPARM)
	SAVE		nprm, prms
C----------------------------------------------------------------------
	iret = 0
C
C*	Check to see if this is the same file.
C
	IF  ( ( sfoutf .ne. sffcur ) .or. ( sfoutf .eq. ' ' ) )  THEN
C
C*	    Close existing file.
C
	    IF  ( iflnew .ne. 0 )  CALL SF_CLOS  ( iflnew, ier )
	    iflnew = 0
	    sffcur = ' '
C
C*	    Open the file.
C
	    CALL SF_OPNF  ( sfoutf, .true., iflnew, isrc, nprm, prms, 
     +			    ier )
	    IF  ( ier .ne. 0 )  THEN
		iret = -1
	        RETURN
	    END IF
C
C*	    Save new file name.
C
	    sffcur = sfoutf
	END IF	    
C
C*	Get the current times from the file.
C
	CALL SF_GTIM  ( iflnew, LLMXTM, ntnew, timnew, ier )
C
C*	Get the requested output parameters.
C
	CALL IN_PRMC ( MMPARM, sfparm, prmlst, prmcnd, ncprm, ier )
	IF ( ( ncprm .eq. 0 ) .or. ( prmlst ( 1 ) .eq. 'DSET' ) ) THEN
C
C*	    Use all the output parameters.
C
	    ncols = MMPARM
	    DO i = 1, ncols
		ipos ( i ) = i
	    END DO
	    nparms = nprm
	  ELSE
C
C*	    Find the requested parameters in the output file.
C
	    icols = 0
	    DO i = 1, ncprm
		CALL ST_FIND ( prmlst ( i ), prms, nprm, iloc, ier )
		IF ( iloc .eq. 0 ) THEN
		    CALL ER_WMSG ( 'SFMOD', +4, prmlst (i), ier )
		  ELSE
		    icols = icols + 1
		    ipos ( icols ) = iloc
		END IF
	    END DO
	    ncols  = icols
	    nparms = ncols
	END IF
C
C*	Check only the requested parameters.
C
	DO i = 1, nparms
	    parms ( i ) = prms ( ipos ( i ) ) 
	END DO
C
C*	Check to see which parameters can be computed.
C
	DO  i = 1, nparms
	    chrflg (i) = .false.
	    cmpflg (i) = .false.
	END DO
C*
	CALL PC_DFLV  ( nparms, parms, chrflg, cmpflg, npm, ier )
	IF  ( ier .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'PC',  ier, ' ', ierr )
	    iret = -3
	    RETURN
	END IF
C
C*	Check to see which parameters can be computed.
C
	knt = 0
	DO  i = 1, nparms
	    IF  ( .not. cmpflg (i) )  THEN
		CALL ER_WMSG  ( 'SFMOD', +1, parms (i), ier )
	      ELSE IF  ( chrflg (i) )  THEN
		CALL ER_WMSG  ( 'SFMOD', +2, parms (i), ier )
	      ELSE
		knt = knt + 1
	    END IF
	END DO
C
C*	If there are no parameters computable, write error message.
C
	IF  ( knt .lt. 1 ) THEN
	    iret = -3
	    CALL ER_WMSG  ( 'SFMOD', iret, ' ', ier )
	END IF
C*
	RETURN
	END
