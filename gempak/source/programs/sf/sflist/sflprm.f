	SUBROUTINE SFLPRM  ( newfil, parms, prmdst, npmdst, tflg, sflg,
     +			     prmcur, ncprm,  chrflg, cparm, iret )
C************************************************************************
C* SFLPRM								*
C*									*
C* This subroutine extracts and sets up parameters to process.		*
C*									*
C* SFLPRM  ( NEWFIL, PARMS, PRMDST, NPMDST, TFLG, SFLG, PRMCUR, NCPRM, 	*
C*	     CHRFLG, CPARM, IRET )					*
C*									*
C* Input parameters:							*
C*	NEWFIL		LOGICAL		New file flag			*
C*	PARMS		CHAR*		Input for PARMS			*
C*	PRMDST (NPMDST)	CHAR*		Data set parameters		*
C*	NPMDST		INTEGER		Number of parms in data set	*
C*	TFLG		LOGICAL		Text data flag			*
C*	SFLG		LOGICAL		Special data flag		*
C*									*
C* Input and output parameters:						*
C*	PRMCUR		CHAR*		Current PARMS string		*
C*									*
C* Output parameters:							*
C*	NCPRM		INTEGER		Number of output parameters	*
C*	CHRFLG (NCPRM)	LOGICAL		Character flags			*
C*	CPARM  (NCPRM)	CHAR*4		Parameter names			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = no computable parms	*
C**									*
C* Log:									*
C* I. Graffman/RDS	 7/87	GEMPAK4					*
C* M. desJardins/GSFC	10/87	Rewritten				*
C* M. desJardins/GSFC	 6/88	Changed name to make readable		*
C* M. desJardins/GSFC	11/89	Set conditions on parameters		*
C* D. Keiser/GSC	 4/96	Added tflg to calling sequence		*
C* S. Jacobs/NCEP	 6/96	Added sflg to calling sequence		*
C* T. Lee/SAIC		 8/01	Checked tflg and sflg			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	parms, prmcur, prmdst (*), cparm (*)
	LOGICAL		chrflg (*), newfil, tflg, sflg
C*
	CHARACTER	tmprm  (MMPARM)*4, tmpcnd (MMPARM)*12,
     +			prmlst (MMPARM)*4, prmcnd (MMPARM)*12
	LOGICAL		cmpflg (MMPARM), levflg (MMPARM)
C------------------------------------------------------------------------
	iret = 0
C
C*	Check that either the parameters or the file has changed.
C
	IF  ( ( ( parms .eq. prmcur ) .and. ( parms .ne. ' ' ) )
     +			.and. ( .not. newfil ) )  THEN
	    RETURN
	END IF
C
C*	Update parameter string.
C
	prmcur = parms
C
C*	Break parameters requested into a list.
C
	CALL IN_PRMC  ( MMPARM, parms, prmlst, prmcnd, ncprm, ier )
	IF  ( ncprm .gt. 0 )  THEN
C
C*	    Extract parameters replacing DSET if found.
C
	    knt = 0
	    DO  i = 1, ncprm 
		IF  ( prmlst (i) .eq. 'DSET' )  THEN
		    DO  j = 1, npmdst
		      IF  ( knt .lt. MMPARM )  THEN
			knt = knt + 1
			tmprm  ( knt ) = prmdst ( j )
			tmpcnd ( knt ) = prmcnd ( i )
		      END IF
		    END DO
		  ELSE
		    IF  ( knt .lt. MMPARM )  THEN
			knt = knt + 1
			tmprm  ( knt ) = prmlst ( i )
			tmpcnd ( knt ) = prmcnd ( i )
		    END IF
		END IF
	    END DO
C
C*	    Determine calculable parameters.
C
	    CALL PC_DFLS  (knt, tmprm, chrflg, cmpflg, levflg, np, iret)
C
C*	    Extract list of calculable parameters.
C
	    ncprm = 0
	    DO  i = 1, knt
		IF  ( cmpflg ( i ) )  THEN
		    ncprm = ncprm + 1
		    cparm  ( ncprm ) = tmprm ( i )
		    tmpcnd ( ncprm ) = tmpcnd ( i )
		  ELSE
		    IF  ( .not. sflg .and. .not. tflg ) 
     +			  CALL ER_WMSG  ( 'SFLIST', +1, tmprm (i), ier )
		END IF
	    END DO
C
C*	    Set up only computable parameters.
C
	    IF  ( ncprm .gt. 0 )  THEN
C
C*		Recall parameter setup for good parameters.
C
		IF  ( ncprm .lt. knt )  THEN
		    CALL PC_DFLS  ( ncprm, cparm, chrflg, cmpflg, 
     +				    levflg, np, iret )
		END IF
C
C*		Set conditions.
C
		CALL PC_SLCD  ( ncprm, tmpcnd, ier )
	    END IF
	END IF
C
C*	Check for no computable parameters.
C
	IF  ( ncprm .eq. 0 .and. .not. tflg .and. .not. sflg )  THEN
	    iret = -4
	    CALL ER_WMSG  ( 'SFLIST', iret, ' ', ier )
	    prmcur = ' '
	END IF
C*
	RETURN
	END
