	SUBROUTINE SNLPRM  ( snparm, stndex, vcoord, prmdst, npmdst,
     +			     nparms, prmlst, nstnp,  stnprm, iret )
C************************************************************************
C* SNLPRM								*
C*									*
C* This subroutine extracts and sets up parameters to process.		*
C*									*
C* SNLPRM  ( SNPARM, STNDEX, VCOORD, PRMDST, NPMDST, NPARMS, PRMLST,	*
C*           NSTNP,  STNPRM, IRET )					*
C*									*
C* Input parameters:							*
C*	SNPARM		CHAR*		Input parameter list		*
C*	STNDEX		CHAR*		Input station parm list		*
C*	VCOORD		CHAR*		Output vertical coordinate	*
C*	PRMDST (NPMDST)	CHAR*		Dataset parameters		*
C*	NPMDST		INTEGER		Number of dataset parms		*
C*									*
C* Output parameters:							*
C*	NPARMS		INTEGER		Number of level parameters	*
C*	PRMLST (NPARMS)	CHAR*4		Level parameters		*
C*	NSTNP		INTEGER		Number of station parameters	*
C*	STNPRM (NSTNP)	CHAR*4		Station parameters		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/88	Rewritten				*
C* M. desJardins/GSFC	11/89	Added conditions on parameters		*
C* K. Brill/NMC		01/92	Initialize PRMCND (1) to ' '		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	snparm, stndex, vcoord, prmdst (*), prmlst (*),
     +			stnprm (*)
C*
	LOGICAL		chrflg (MMPARM), cmpflg (MMPARM)
	CHARACTER	tmprm  (MMPARM)*4, tmcnd (MMPARM)*12,
     +			prmcnd (MMPARM)*12, vc*4
C--------------------------------------------------------------------------
	iret   = 0
	nparms = 0
C
C*	Get level parameters.
C
	CALL IN_PRMC  ( MMPARM, snparm, tmprm, tmcnd, np, ier )
	IF  ( np .gt. 0 )  THEN
C
C*	    Put output dataset coordinate first in list.
C
	    CALL ST_LCUC  ( vcoord, vc, ier )
	    prmlst (1) = vc
	    nparms     = 1
	    prmcnd (1) = ' '
C
C*	    Add other parameters to list.
C
	    DO  i = 1, np
		IF  ( tmprm (i) .eq. 'DSET' )  THEN
		    DO  j = 1, npmdst
			IF  ( prmdst (j) .ne. vc )  THEN
			    nparms = nparms + 1
			    prmlst ( nparms ) = prmdst (j)
			    prmcnd ( nparms ) = ' '
			END IF
		    END DO
		  ELSE
		    IF  ( tmprm (i) .ne. vc )  THEN
			nparms = nparms + 1
			prmlst ( nparms ) = tmprm (i)
			prmcnd ( nparms ) = tmcnd (i)
		    END IF
		END IF
	    END DO
C
C*	    Determine calculable level parameters.
C
	    CALL PC_DFLV  ( nparms, prmlst, chrflg, cmpflg, np, iret )
C
C*	    Extract list of calculable level parameters.
C
	    np     = 0
	    DO  i = 1, nparms
		IF  ( .not. cmpflg (i) )  THEN
		    CALL ER_WMSG  ( 'SNLIST', -4, prmlst (i), ier )
		  ELSE IF  ( chrflg (i) )  THEN
		    CALL ER_WMSG  ( 'SNLIST', -5, prmlst (i), ier )
		  ELSE
		    np = np + 1
		    prmlst (np) = prmlst (i)
		    prmcnd (np) = prmcnd (i)
		END IF
	    END DO
C
C*	    Check that vertical coordinate can be calculated.
C
	    IF  ( ( np .eq. 0 ) .or. ( prmlst (1) .ne. vc ) )  THEN
		CALL ER_WMSG  ( 'SNLIST', -6, vc, ier )
		nparms = 0
	      ELSE IF  ( np .eq. 1 )  THEN
		nparms = 0
	      ELSE IF  ( nparms .ne. np )  THEN
		CALL PC_DFLV  ( np, prmlst, chrflg, cmpflg, nn, ier )
		nparms = np
	    END IF
C
C*	    Set conditions.
C
	    IF  ( nparms .gt. 0 )  CALL PC_SLCD ( nparms, prmcnd, ier )
	END IF
C
C*	Get list of station parameters.
C
	nstnp = 0
	CALL IN_PRMC  ( MMPARM, stndex, tmprm, tmcnd, np, ier )
	IF  ( np .gt. 0 )  THEN
C
C*	    Determine list of calculable station parameters.
C
	    CALL PC_DFST  ( np, tmprm, chrflg, cmpflg, nn, ier )
C
C*	    Extract list of calculable station parameters.
C
	    nstnp = 0
	    DO  i = 1, np
		IF  ( .not. cmpflg (i) )  THEN
		    CALL ER_WMSG  ( 'SNLIST', -7, tmprm (i), ier )
		  ELSE IF  ( chrflg (i) )  THEN
		    CALL ER_WMSG  ( 'SNLIST', -8, tmprm (i), ier )
		  ELSE
		    nstnp = nstnp + 1
		    stnprm (nstnp) = tmprm (i)
		    tmcnd  (nstnp) = tmcnd (i)
		END IF
	    END DO
	    IF  ( ( np .ne. nstnp ) .and. ( nstnp .gt. 0 ) )  THEN
		CALL PC_DFST  ( nstnp, stnprm, chrflg, cmpflg, nn, ier )
	    END IF
C
C*	    Set conditions on station parameters.
C
	    IF  ( nstnp .gt. 0 )  CALL PC_SSCD ( nstnp, tmcnd, ier )
	END IF
C*
	RETURN
	END
