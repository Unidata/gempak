	SUBROUTINE SNMPRM  ( newfil, snparm, parms, nparm, prmlst,
     +			     chrflg, ncprm, prcons, wndflg, iret )
C************************************************************************
C* SNMPRM								*
C*									*
C* This subroutine extracts and sets up parameters to process.		*
C*									*
C* SNMPRM  ( NEWFIL, SNPARM, PARMS, NPARM, PRMLST, 			*
C*           CHRFLG, NCPRM,  PRCONS, WNDFLG, IRET )			*
C*									*
C* Input parameters:							*
C*	NEWFIL		LOGICAL		New file flag			*
C*	SNPARM		CHAR*		Input parameter string		*
C*	PARMS (*)	CHAR*		Data set parameters		*
C*	NPARM		INTEGER		Number of parameters		*
C*									*
C* Output parameters:							*
C*	PRMLST (NCPRM)	CHAR*		Output parameters 		*
C*	CHRFLG (NCPRM)	LOGICAL		Character flags			*
C*	NCPRM		INTEGER		Number of valid parameters	*
C*	PRCONS (NCPRM)	CHAR*		Parameters and conditions	*
C*	WNDFLG		LOGICAL		Wind parm existence flag	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 7/87	GEMPAK4					*
C* M. desJardins/GSFC	 6/88	Reorganized; eliminated check for 	*
C*				same parm string since it didn't work	*
C* M. desJardins/GSFC	11/89	Add conditions to parameter list	*
C* S. Schotz/GSC	 8/90	Add plot at station option like SFMAP,	*
C*				removed call to SFMCOF			*
C* K. Brill/NMC		12/91	First PARM is centered; changes for	*
C*				winds; removed ONE; cleanup.		*
C* K. Brill/NMC		02/92	CALL PC_SACD to set conditions		*
C* S. Jacobs/NMC	 8/94	Changed PC_SACD to PC_SLCD since indices*
C*				  are treated a level parms in this prog*
C* S. Maxwell/GSC	 3/97	Added call to IN_MRKR                 	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	snparm, parms (*), prmlst (*), 
     +                  prcons (*)
	LOGICAL		chrflg (*), wndflg, newfil
C*
	CHARACTER	tmprm  (MMPARM)*4, tmcnd (MMPARM)*12,
     +			prmcnd (MMPARM)*24, condtn*12, pl*1
	LOGICAL		cmpflg (MMPARM), levflg (MMPARM)
C*	
	INTEGER		MXPRPL
C
C*	MXPRPL is the maximum number of parameters to be plotted.
C
	PARAMETER 	( MXPRPL = 11 )
C--------------------------------------------------------------------------
	iret = 0
        DO  i = 1, nparm
            prcons ( i ) = ' '
        END DO
C
C*	Break the input string into a list of parameters.
C
	CALL IN_PRMC  ( MMPARM, snparm, tmprm, tmcnd, ntmp, ier )
C
C*	Check for DSET.
C
	knt  =  0
	DO  i = 1, ntmp
	    IF  ( tmprm (i) .eq. 'DSET' )  THEN
		DO  j = 1, nparm
		    IF  ( knt .lt. MMPARM )  THEN
			knt = knt + 1
			prmlst ( knt ) = parms ( j )
			prmcnd ( knt ) = ' '
		    END IF
		END DO
	      ELSE
		IF  ( knt .lt. MMPARM )  THEN
		    knt = knt + 1
		    prmlst ( knt ) = tmprm ( i )
		    prmcnd ( knt ) = tmcnd ( i )
		END IF
	    END IF
	END DO
C
C*	Exit if there are no parameters to compute.
C
	IF  ( knt .eq. 0 )  THEN
	    ncprm = 0
	    RETURN
	END IF
C
C*	Determine calculable parameters.
C
	CALL PC_DFLS  ( knt, prmlst, chrflg, cmpflg, levflg, np, ier )
C
C*	Extract list of calculable parameters.
C
	ncprm = 0
	DO  i = 1, knt
	    IF  ( cmpflg (i) .or. ( prmlst (i) .eq. 'MARK' )) THEN 
		ncprm = ncprm + 1
                prmlst (ncprm) = prmlst (i)
                prmcnd (ncprm) = prmcnd (i)
	    ELSE IF ( prmlst (i) .eq. 'BLNK'  .or.
     +		      prmlst (i) .eq. 'SPAC' ) THEN
		ncprm = ncprm + 1
	    ELSE 
		CALL ER_WMSG  ( 'SNMAP', +1, prmlst (i), iret )
	    END IF
	END DO
C
C*	Check for case when there are no valid parameters.
C
	IF  ( ( ncprm .eq. 1 .and. ( prmlst (1) .eq. 'BLNK' .or.
     +	        prmlst (1) .eq. 'SPAC' ) ) .or. 
     +		ncprm .eq. 0 ) THEN
	    ncprm = 0
	    RETURN
	END IF
C
C*	Be sure there are no more than MXPRPL parameters.
C
	IF  ( ncprm .gt. MXPRPL ) ncprm = MXPRPL
C
C*	Recall parameter setup for good parameters and winds if
C*	some parameters were not calculable.
C
	IF  ( ncprm .ne. knt )  THEN
	    CALL PC_DFLS  ( ncprm, prmlst, chrflg, cmpflg, levflg, np, 
     +			    iret )
	END IF
C
C*	Check for wind parameters and set display type.
C
	DO i = 1, ncprm
            IF ( ( prmlst (i) .eq. 'ARRW' ) .or.
     +           ( prmlst (i) .eq. 'ARRM' ) .or.
     +           ( prmlst (i) .eq. 'ARRK' ) .or.
     +           ( prmlst (i) .eq. 'BARB' ) .or.
     +           ( prmlst (i) .eq. 'BRBM' ) .or.
     +		 ( prmlst (i) .eq. 'DARR' ) .or.
     +           ( prmlst (i) .eq. 'BRBK' ) )  THEN
                wndflg = .true.
                pl = prmlst (i) (1:1)
                CALL IN_PWND  ( prmcnd (i), pl, .false., condtn, ier )
C
                IF (prmlst (i) .eq. 'BARB') prmlst (i) = 'BRBM'
C
	     ELSE IF ( prmlst (i) .eq. 'MARK' ) THEN
		CALL IN_MRKR ( prmcnd (i), condtn, ier )
                prmcnd (i) = condtn
            END IF
	END DO
C
C*	Set conditions.
C
	CALL PC_SLCD ( ncprm, prmcnd, ier )
C
C*	Build parameter-condition strings
C
	DO  i = 1, ncprm
   	    IF  ( prmcnd (i) .ne. ' ' )  THEN
                CALL ST_LSTR ( prmlst (i), len, ier )
                prcons (i) = prmlst (i) (:len) // prmcnd (i)
            ELSE
                prcons (i) = prmlst (i)
            END IF
	END DO
C*
	RETURN
	END
