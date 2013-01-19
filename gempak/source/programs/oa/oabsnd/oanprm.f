	SUBROUTINE OANPRM  ( ilvert, parms, prmcnd, nparms, stnprm,
     +			     stncnd, nstnpm, nlevel, prmsn,
     +                       nprmsn, mandat, iret )
C************************************************************************
C* OANPRM								*
C*									*
C* This subroutine determines which parameters can be computed for	*
C* OABSND.  The PC package is initialized.				*
C*									*
C* OANPRM  ( ILVERT, PARMS, PRMCND, NPARMS, STNPRM, STNCND, NSTNPM,	*
C*           NLEVEL, PRMSN, NPRMSN, MANDAT, IRET )			*
C*									*
C* Input parameters:							*
C*	ILVERT		INTEGER		Vertical coordinate number	*
C*	PARMS (NPARMS)  CHAR*           Level parms to be analyzed	*
C*	PRMCND(NPARMS)	CHAR*		Conditions on level parms	*
C*	NPARMS		INTEGER		Number of level parms		*
C*	STNPRM(NSTNPM)	CHAR*		Station parms to be analyzed	*
C*	STNCND(NSTNPM) 	CHAR*		Conditions on station parms	*
C*	NSTNPM		INTEGER		Number of station parms		*
C*	NLEVEL		INTEGER		Number of levels		*
C*      PRMSN (NPRMSN)  CHAR*		Parms in data set		*
C*      NPRMSN		INTEGER         Number of parms in data set	*
C*	MANDAT		LOGICAL		Only mandatory data flag        *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*                                       +9 = parameter not found	*
C*         				 +8 = parm is a character	*
C*					 +7 = Parm not computable	*
C*					  0 = normal return		*
C*					 -9 = no computable parms	*
C*					-10 = level range is invalid	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* M. desJardins/GSFC	11/89	Added conditions to parameters		*
C* K. Brill/NMC          8/90   Changes for multiple files		*
C* D. Kidwell/NCEP	 5/99	Added arg mandat and call to PC_MAND    *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	parms (*), stnprm (*), prmcnd (*),
     +                  stncnd (*), prmsn (*)
	LOGICAL		mandat
C*
	LOGICAL		chrflg (MMPARM), cmpflg (MMPARM)
C------------------------------------------------------------------------
	iret   = 0
C
C*	Initialize the PC package.
C
	CALL PC_INIT  ( ilvert, nprmsn, prmsn, iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'PC', iret, ' ', ier )
	    iret   = -1
	    RETURN
	END IF
C
C*	Set interpolation flag.
C
	CALL PC_MAND ( mandat, ier )
C
C*	See which parameters can be computed.
C
	IF  ( ( nparms .gt. 0 ) .and. ( nlevel .gt. 0 ) )  THEN
	    CALL PC_DFLV  ( nparms, parms, chrflg, cmpflg, nout, ier )
	    IF  ( ier .ne. 0 )  THEN
		CALL ER_WMSG  ( 'PC', iret, ' ', jerr )
		iret = -9
		RETURN
	    END IF
C
C*	    Detect parameters which are computable and not characters.
C
	    DO  i = 1, nparms
		IF  ( .not. cmpflg (i) )  THEN
	            iret = +7
		    CALL ER_WMSG  ( 'OABSND', iret, parms (i), ier )
	        END IF
	        IF  ( chrflg (i) )  THEN
	            iret = +8
		    CALL ER_WMSG  ( 'OABSND', iret, parms (i), ier )
		END IF
	    END DO
	    IF ( iret .ne. 0 ) RETURN
C
C*	    Set the conditions.
C
	    IF  ( nparms .gt. 0 )  CALL PC_SLCD  ( nparms, prmcnd, ier )
	END IF

C*	See which station parameters can be computed.
C
	IF  ( nstnpm .gt. 0 )  THEN
	    CALL PC_DFST  ( nstnpm, stnprm, chrflg, cmpflg, nout, ier )
	    IF  ( ier .ne. 0 )  THEN
		CALL ER_WMSG  ( 'PC', iret, ' ', jerr )
		iret = -9
		RETURN
	    END IF
C
C*	    Get list of parameters which are computable and not 
C*	    characters.
C
	    DO  i = 1, nstnpm
		IF  ( .not. cmpflg (i) )  THEN
	            iret = +9
		    CALL ER_WMSG  ( 'OABSND', +9, stnprm (i), ier )
	        END IF
	        IF  ( chrflg (i) )  THEN
		    iret = +8
		    CALL ER_WMSG  ( 'OABSND', +8, stnprm (i), ier )
	        END IF
	    END DO
	    IF ( iret .ne. 0 ) RETURN
C
C*	    Set conditions for station parameters.
C
	    IF  ( nstnpm .gt. 0 )  CALL PC_SSCD  ( nstnpm, stncnd, ier )
	END IF
C*
 	RETURN
	END
