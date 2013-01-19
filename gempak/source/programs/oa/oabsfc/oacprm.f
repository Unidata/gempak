	SUBROUTINE OACPRM  ( isffln, gdatim, parms, prmcnd, nparms,
     +                       prmsf, nprmsf, nstn, sfdata, slat, slon,
     +                       stid, cosslt, iret )
C************************************************************************
C* OACPRM								*
C*									*
C* This subroutine initializes the PC package, determines which 	*
C* parameters can be computed, calls a subroutine to compute them	*
C* and returns the values for file ISFFLN.				*
C*									*
C* OACPRM  ( ISFFLN, GDATIM, PARMS, PRMCND, NPARMS, PRMSF, NPRMSF, 	*
C*           NSTN, SFDATA, SLAT, SLON, STID, COSSLT, IRET )		*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER         Surface file number		*
C*      GDATIM          CHAR*           Grid date time			*
C*	PARMS (*)       CHAR*           Parameters to analyse		*
C*      PRMCND (*)      CHAR*           Parameter conditions		*
C*      NPARMS          INTEGER         Number of parms to analyse	*
C*	PRMSF (*)	CHAR*		Parameters in file		*
C*	NPRMSF		INTEGER		Number of parmeters in file	*
C*									*
C* Input and output parameter:						*
C*	NSTN		INTEGER		Number of stations		*
C*									*
C* Output parameters:							*
C*      SFDATA		REAL            Surface data			*
C*	(NPARMS, NSTN) 							*
C*	SLAT (NSTN)    	REAL		Station latitude		*
C*	SLON (NSTN)	REAL		Station longitude		*
C*	STID (NSTN)	CHAR*		Station ID			*
C*	COSSLT (NSTN)	REAL		Cosine of station latitude	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*				         -1 = error			*
C*					 -9 = no computable parms	*
C**									*
C* Log:									*
C* I. Graffman/RDS	 6/86						*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* M. desJardins/GSFC	11/89	Added conditions to parameter list	*
C* K. Brill		 8/90   Multiple file fix			*
C* K. Brill/NMC		 3/91	Allow OA of station parameters		*
C* K. Brill/NMC		06/91	Added COSSLT				*
C* T. Lee/GSC		 3/99	Added STID in calling seq.		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)   gdatim
	CHARACTER*(*)   parms (*), prmcnd (*), prmsf (*), stid(*)
	REAL            sfdata (*), slat (*), slon (*), cosslt (*)
C*
	LOGICAL		chrflg (MMPARM), cmpflg (MMPARM), 
     +			lvlflg (MMPARM)
C------------------------------------------------------------------------
	iret   = 0
C
C*	Initialize the PC package.
C
	  ivert = 0
	  CALL PC_INIT  ( ivert, nprmsf, prmsf, iret )
	  IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'PC', iret, ' ', ier )
	    iret   = -1
	    RETURN
	  END IF  
C
C*	See which parameters can be computed.
C
	CALL PC_DFLS  ( nparms, parms, chrflg, cmpflg, lvlflg,
     +			nout, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -9
	    RETURN
	END IF
C
C*	Get list of parameters which are computable and not characters.
C
	DO  i = 1, nparms
	    IF  ( .not. cmpflg (i) )  THEN
	        iret = +7
		CALL ER_WMSG  ( 'OABSFC', iret, parms (i), ier )
	    END IF
	    IF  ( chrflg (i) )  THEN
	        iret = +8
		CALL ER_WMSG  ( 'OABSFC', iret, parms (i), ier )
            END IF
	END DO
	IF ( iret .ne. 0 ) RETURN
C
C*	Set conditions.
C
	CALL PC_SLCD  ( nparms, prmcnd, ier )
C
C*	Now go get the values of the parameters to be analysed.
C
        CALL SF_STIM  ( isffln, gdatim, ier )
        CALL SF_BEGS  ( isffln, ier )
	CALL OACDTA ( isffln, nparms, nstn, sfdata, slat, slon,
     +		      stid, cosslt, iret )
C*
 	RETURN
	END
