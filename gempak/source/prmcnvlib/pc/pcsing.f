	SUBROUTINE PC_SING  ( noutpm, outprm, cmpflg, np, iret )
C************************************************************************
C* PC_SING								*
C*									*
C* This subroutine sets up the computation of integrated parameters.	*
C* It is currently set up to check for DHGT, MGHT, PWTR and PSYM.	*
C* The computable flags for the vertical coordinate parameters		*
C* required for these computations have been saved by PC_SVRT.		*
C*									*
C* PC_SING  ( NOUTPM, OUTPRM, CMPFLG, NP, IRET )				*
C*									*
C* Input parameters:							*
C*	NOUTPM		INTEGER		Number of output parms		*
C*	OUTPRM (NOUTPM) CHAR*4		Output parms			*
C*									*
C* Input and output parameters:						*
C*	CMPFLG (NOUTPM) LOGICAL		Computable flags		* 
C*	NP		INTEGER		Number of computable parms	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/84						*
C* M. desJardins/GSFC	 9/88	Cleaned up				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'pccmn.cmn'
C*
	CHARACTER*(*)	outprm (*)
	LOGICAL		cmpflg (*)
C*
	CHARACTER*4	parm
C------------------------------------------------------------------------
	iret = 0
C
C*	Initialize common values.
C
	DO  i = 1, MAXVRT
	    qing   (i) = .false.
	    kinpos (i) = 0
	ENDDO
	king = 0
C
C*	If all parameters are already computable, return.
C*	Also exit if there is no vertical coordinate system.
C
	IF ( (noutpm .eq. kfound (1)) .or. (jcord .eq. 0) ) RETURN
C
C*	Check vertical table to see that pressure can be found.
C
	IF  ( .not. vcomp (1) ) RETURN
C
C*	Check if any non-computable parameters are integrated
C*	parameters.
C
	DO  i = 1, noutpm
	    IF  ( .not. cmpflg (i) )  THEN
		parm = outprm (i)
		IF  ( ( parm .eq. 'DHGT' ) .and. vcomp (5) )  THEN
		    king = king + 1
		    np   = np + 1
		    kinpos (9) = i
		    cmpflg (i) = .true.
		    qing   (9) = .true.
		  ELSE IF  ( ( parm .eq. 'MHGT' ) .and. 
     +			       vcomp (5) .and. vcomp (6) )  THEN
		    king = king + 1
		    np   = np + 1
		    kinpos (10) = i
		    cmpflg (i)  = .true.
		    qing   (10) = .true.
		  ELSE IF ( ( parm .eq. 'PWTR' ) .and. vcomp (6) ) THEN
		    king = king + 1
		    np   = np + 1
		    kinpos (11) = i
		    cmpflg (i)  = .true.
		    qing   (11) = .true.
		  ELSE IF ( ( parm .eq. 'PSYM' ) .and. vcomp (5) ) THEN
		    king = king + 1
		    np   = np + 1
		    kinpos (12) = i
		    cmpflg (i)  = .true.
		    qing   (12) = .true.
		END IF
	    END IF
	END DO
C*
	RETURN
	END
