	SUBROUTINE PC_DFLS  ( noutpm, outprm, chrflg, cmpflg, levflg,
     +			      ncomp, iret )
C************************************************************************
C* PC_DFLS								*
C*									*
C* This subroutine is used to define the level and station output	*
C* parameters which will be returned when the subroutine PC_CMVS 	*
C* is called.  PC_INIT must be called before this subroutine is 	*
C* called.  CMPFLG will be set if a parameter is computable.  CHRFLG	*
C* and LEVFLG will be set for character data type and station type	*
C* parameters, respectively.  						*
C*									*
C* PC_DFLS  ( NOUTPM, OUTPRM, CHRFLG, CMPFLG, LEVFLG, NCOMP, IRET )	*
C*									*
C* Input parameters:							*
C*	NOUTPM		INTEGER		Number of output parameters	*
C*	OUTPRM (NOUTPM)	CHAR*4		Output parameters		*
C*									*
C* Input and output parameters:						*
C*	CHRFLG (NOUTPM)	LOGICAL		Character data flag 		*
C*	CMPFLG (NOUTPM)	LOGICAL		Computable data flag		*
C*	LEVFLG (NOUTPM)	LOGICAL		Level parameter flag		*
C*	NCOMP		INTEGER		Number of computable parms	*
C*	IRET		INTEGER		Return code			*
C*				  	   0 = normal completion	*
C*				 	  -4 = PC_INIT not called	*
C*				 	  -5 = invalid # of parameters	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/88	Written for GEMPAK4			*
C* S. Jacobs/EAI	 9/93	Added reset of ksprm if no station	*
C*				   parameters				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'pccmn.cmn'
C*
	LOGICAL		chrflg (*), cmpflg (*), levflg (*)
	CHARACTER*(*)	outprm (*)
C*
	LOGICAL		cmfff (MAXPRM), chfff (MAXPRM)
C-------------------------------------------------------------------------
	ncomp = 0
C
C*	Call PC_DFLV to get the level parameters.
C
	npm = 0
	CALL PC_DFLV  ( noutpm, outprm, chrflg, cmpflg, npm, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Set NCOMP and station flags.
C
	IF  ( npm .gt. 0 )  THEN
	    DO  i = 1, noutpm
		IF  ( cmpflg (i) )  levflg (i) = .true.
	    END DO
	END IF
	ncomp = npm
C
C*	Check for station parameters included in list.
C
	IF  ( noutpm .gt. ncomp )  THEN
	    CALL PC_DFST  ( noutpm, outprm, chfff, cmfff, nstn, ier )
	    DO  i = 1, noutpm
		IF  ( cmfff (i) )  THEN
		    cmpflg (i) = .true.
		    chrflg (i) =  chfff (i)
		    levflg (i) = .false.
		    ncomp = ncomp + 1
		END IF
	    END DO
	ELSE
	    ksprm = 0
	END IF
C*
	RETURN
	END
