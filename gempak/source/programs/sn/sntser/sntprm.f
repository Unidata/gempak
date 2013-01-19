	SUBROUTINE SNTPRM  ( snparm, stndex, parm, stnprm, iret )
C************************************************************************
C* SNTPRM								*
C*									*
C* This subroutine extracts and sets up the parameter to process.	*
C*									*
C* SNTPRM  ( SNPARM, STNDEX, PARM, STNPRM, IRET )			*
C*									*
C* Input parameters:							*
C*	SNPARM		CHAR*		Input parameter list		*
C*	STNDEX		CHAR*		Input station parm list		*
C*									*
C* Output parameters:							*
C*	PARM		CHAR*4		Level parameter			*
C*	STNPRM		CHAR*4		Station parameter		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1   invalid parameters	*
C**									*
C* Log:									*
C* G. Huffmman/USRA	 5/89	Adapted from SNLPRM			*
C* S. Schotz/GSC	 7/90	Cleaned up.				*
C* K. Brill/NMC		02/92	Added conditions.			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	snparm, stndex, parm, stnprm
C*
	LOGICAL		chrflg (MMPARM), cmpflg (MMPARM)
	CHARACTER	tmprm  (MMPARM)*4, tmcnd (MMPARM)*12
C--------------------------------------------------------------------------
	iret   = -1
        parm = ' '
        stnprm = ' '
        np = 0
C
C*	Get level parameter first.  If found do not attempt to
C*      get station parameter.
C
	CALL IN_PRMC  ( MMPARM, snparm, tmprm, tmcnd, np, ier )
	IF  ( np .gt. 0 )  THEN
C
C*	    Check the input for 'DSET' and then put output dataset 
C*	    coordinate first in list.
C
	    IF  ( (tmprm (1) .eq. 'DSET') .or. ( np .gt. 1) )  THEN
		CALL ER_WMSG  ( 'SNTSER', 1, ' ', ier )
	    END IF
C
C*	    Determine calculable level parameters.
C
	    CALL PC_DFLV  ( 1, tmprm, chrflg, cmpflg, np, ier )
C
C*	    Check if paramter is calculable.
C
	    np = 0
	    IF  ( .not. cmpflg (1) )  THEN
		CALL ER_WMSG  ( 'SNTSER', -9, tmprm (1), ier )
            ELSE IF  ( chrflg (1) )  THEN
		CALL ER_WMSG  ( 'SNTSER', -10, tmprm (1), ier )
            ELSE
		np = 1
                parm = tmprm (1)
		CALL PC_SLCD ( 1, tmcnd, ier )
                iret = 0
	    END IF
        END IF
C
C*	Get list of station parameters (only the first is used).
C
        IF ( np .eq. 0 ) THEN
	    CALL IN_PRMC  ( MMPARM, stndex, tmprm, tmcnd, np, ier )
	    IF  ( np .gt. 0 )  THEN
C
C*	        Make sure the station parameter is calculable.
C
	        CALL PC_DFST  ( 1, tmprm, chrflg, cmpflg, nn, ier )
	        IF  ( .not. cmpflg (1) )  THEN
		    CALL ER_WMSG  ( 'SNTSER', -9, tmprm (1), ier )
		    stnprm = ' '
	        ELSE IF  ( chrflg (1) )  THEN
		    CALL ER_WMSG  ( 'SNTSER', -10, tmprm (1), ier )
		    stnprm = ' '
	        ELSE
                    iret = 0
		    stnprm = tmprm (1)
		    CALL PC_SSCD ( 1, tmcnd, ier )
                END IF
	    END IF
	END IF
	IF  ( np .le. 0 ) THEN
            CALL ER_WMSG ( 'SNTSER', -8, ' ', ier )
        END IF
C*
	RETURN
	END
