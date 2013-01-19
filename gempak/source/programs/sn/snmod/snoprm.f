	SUBROUTINE SNOPRM  ( snparm, vcoord, prmdst, npmdst, exist,
     +			     prmout, npmout, parms, nparms, iret )
C************************************************************************
C* SNOPRM								*
C*									*
C* This subroutine extracts and sets up parameters to process.		*
C*									*
C* SNOPRM  ( SNPARM, VCOORD, PRMDST, NPMDST, EXIST, PRMOUT, NPMOUT,	*
C*           PARMS, NPARMS, IRET )					*
C*									*
C* Input parameters:							*
C*	SNPARM		CHAR*		Input parameter list		*
C*	VCOORD		CHAR*		Output vertical coordinate	*
C*	PRMDST (NPMDST)	CHAR*		Dataset parameters		*
C*	NPMDST		INTEGER		Number of dataset parms		*
C*	EXIST		LOGICAL		New output file flag		*
C*	PRMOUT (NPMOUT)	CHAR*		Parameters in output dataset	*
C*	NPMOUT		INTEGER		Number of parms in output ds	*
C*									*
C* Output parameters:							*
C*	PARMS  (NPARMS)	CHAR*		Parameters to compute		*
C*	NPARMS		INTEGER		Number of parameters		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = invalid output parms	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/88	Rewritten				*
C* M. desJardins/GSFC	11/89	Added conditions to station list	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	snparm, vcoord, prmdst (*), prmout (*), 
     +			parms (*)
	LOGICAL		exist
C*
	LOGICAL		chrflg (MMPARM), cmpflg (MMPARM)
	CHARACTER	tmprm  (MMPARM)*4, tmcnd (MMPARM)*12,
     +			prmcnd (MMPARM)*12
C------------------------------------------------------------------------
	iret   = 0
	nparms = 0
C
C*	Get parameters.
C
	CALL IN_PRMC  ( MMPARM, snparm, tmprm, tmcnd, np, ier )
	DO  i = 1, MMPARM
	    prmcnd (i) = ' '
	END DO
C
C*	Check for DSET with existing file.
C
	IF ( exist .and. ( np .eq. 1 ) .and. ( tmprm (1) .eq. 'DSET' ) )
     +							THEN
	    DO  j = 1, npmout
		parms (j) = prmout (j)
	    END DO
	    nparms = npmout
	  ELSE IF  ( np .gt. 0 )  THEN
C
C*	    Put output dataset coordinate first in list.
C
	    parms (1) = vcoord
	    nparms    = 1
C
C*	    Add other parameters to list.
C
	    DO  i = 1, np
		IF  ( ( .not. exist ) .and. ( tmprm (i) .eq. 'DSET' ) )
     +								THEN
		    DO  j = 1, npmdst
			IF  ( prmdst (j) .ne. vcoord )  THEN
			    nparms = nparms + 1
			    parms ( nparms ) = prmdst (j)
			END IF
		    END DO
		  ELSE
		    IF  ( tmprm (i) .ne. vcoord )  THEN
			nparms = nparms + 1
			parms  ( nparms ) = tmprm (i)
			prmcnd ( nparms ) = tmcnd (i)
		    END IF
		END IF
	    END DO
	END IF
C
C*	Determine calculable parameters.
C
	IF  ( nparms .gt. 0 )  THEN
C*
	    CALL PC_DFLV  ( nparms, parms, chrflg, cmpflg, np, iret )
C
C*	    Extract list of calculable level parameters.
C
	    np     = 0
	    DO  i = 1, nparms
		IF  ( .not. cmpflg (i) )  THEN
		    CALL ER_WMSG  ( 'SNMOD', -6, parms (i), ier )
		  ELSE IF  ( chrflg (i) )  THEN
		    CALL ER_WMSG  ( 'SNMOD', -7, parms (i), ier )
		  ELSE
		    np = np + 1
		    parms (np)  = parms (i)
		    prmcnd (np) = prmcnd (i)
		END IF
	    END DO
C
C*	    Check that vertical coordinate can be calculated.
C
	    IF  ( ( np .eq. 0 ) .or. ( parms (1) .ne. vcoord ) )  THEN
		CALL ER_WMSG  ( 'SNMOD', -8, vcoord, ier )
		nparms = 0
	      ELSE IF  ( np .eq. 1 )  THEN
		nparms = 0
	      ELSE IF  ( nparms .ne. np )  THEN
		CALL PC_DFLV  ( np, parms, chrflg, cmpflg, nn, ier )
		nparms = np
	    END IF
	END IF
C
C*	If no parameters can be calculated, return error.
C
	IF  ( nparms .eq. 0 )  THEN
	    iret = -9
	  ELSE IF  ( exist )  THEN
C
C*	    Check parameters are the same as those in dataset.
C
	    IF  ( nparms .eq. npmout )  THEN
		DO  j = 1, nparms
		    IF  ( parms (j) .ne. prmout (j) )  iret = -12
		END DO
	      ELSE
		iret = -12
	    END IF
	END IF
C
C*	Write error message if necessary.
C
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'SNMOD', iret, ' ', ier )
C
C*	    Set conditions.
C
	  ELSE
	    CALL PC_SLCD  ( nparms, prmcnd, ier )
	END IF
C*
	RETURN
	END
