	SUBROUTINE TB_PRMT  ( maxprm, nparms, parms, chrflg, intflg, 
     +			      extflg, angflg, iret )
C************************************************************************
C* TB_PRMT								*
C*									*
C* This subroutine reads the parameter type table and returns the	*
C* parameters and the character, interpolation, extrapolation and	*
C* angle flags.  If the file cannot be opened or there are too many	*
C* parameters, an error message will be written, but no error will	*
C* be returned.								*
C*									*
C* TB_PRMT  ( MAXPRM, NPARMS, PARMS, CHRFLG, INTFLG, EXTFLG, ANGFLG,	*
C*            IRET )							*
C*									*
C* Input parameters:							*
C*	MAXPRM		INTEGER		Maximum number of parameters	*
C*									*
C* Output parameters:							*
C*	NPARMS		INTEGER		Number of parameters		*
C*	PARMS  (NPARMS)	CHAR*		Parameter names			*
C*	CHRFLG (NPARMS)	LOGICAL		Character type flags		*
C*	INTFLG (NPARMS)	LOGICAL		Interpolation flags		*
C*	EXTFLG (NPARMS)	LOGICAL		Extrapolation flags		*
C*	ANGFLG (NPARMS)	LOGICAL		Angle flags			*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/84						*
C* M. desJardins/GSFC	 8/88	Cleaned up				*
C* D. Keiser/GSC	12/95	Changed FL_TOPN to FL_TBOP		*
C* S. Jacobs/NCEP	 7/96	Changed environ vars to "$.../" format	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	LOGICAL		chrflg (*), intflg (*), extflg (*), angflg (*)
	CHARACTER*(*)	parms (*)
C*
	CHARACTER	ppp*4
	LOGICAL		f1, f2, f3, f4
C------------------------------------------------------------------------
	iret   = 0
	nparms = 0
C
C*	Open parameter flag table file.
C
	CALL FL_TBOP  ( 'prmflg.tbl', 'parms', lun, ier )
	IF  ( ier .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'FL', ier, 'prmflg.tbl', ierr )
	    RETURN
	END IF
C
C*	Read in each parameter.  Check that the common area is large
C*	enough.
C
	iostat = 0
	DO WHILE  ( iostat .eq. 0 )
C
C*	    Read in next record.
C
	    READ   ( lun, 1000, IOSTAT = iostat )  ppp, f1, f2, f3, f4
1000	    FORMAT ( A, 2X, 4L6 )
C
C*	    Add to table.
C
	    IF  ( iostat .eq. 0 )  THEN
C
C*		Check for room in table.
C
		IF  ( nparms .ge. maxprm )  THEN
		    ier = -7
		    CALL ER_WMSG  ( 'TB', ier, ' ', ierr )
		    iostat = -1
		  ELSE
		    nparms = nparms + 1
		    parms ( nparms ) = ppp
		    chrflg ( nparms ) = f1
		    intflg ( nparms ) = f2
		    extflg ( nparms ) = f3
		    angflg ( nparms ) = f4
		END IF
	    END IF
	END DO
C
C*	Close table.
C
	CALL FL_CLOS  ( lun, ier )
C*
	RETURN
	END
