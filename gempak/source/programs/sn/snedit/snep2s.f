	SUBROUTINE SNEP2S ( parr, ptnam, nparm, levnum, kntflg, ddpflg, 
     +                      stdata, iret )
C************************************************************************
C* SNEP2S								*
C*									*
C* This routine takes data from the edit file part array for a level    *
C* and moves it to the sounding data array for a level.			*
C*									*
C* SNEP2S ( PARR, PTNAM, NPARM, LEVNUM, KNTFLG, DDPFLG, STDATA, IRET )	*
C*									*
C* Input Parameters: 							*
C*	PARR(6)		REAL		Array of parameter data		*
C*	PTNAM		CHAR*		Part name			*
C*      NPARM		INTEGER         Number of part parameters	*
C* 	LEVNUM		INTEGER 	Level number of parameters	*
C*	KNTFLG		LOGICAL		Flag for knots for wind data    *
C*	DDPFLG		LOGICAL		Flag for dew point depress data *
C*									*
C* Output Parameters:							*
C*	STDATA(*)	REAL		Sounding data at level  	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal                    *
C**									*
C* Log:									*
C* Schotz/GSC		12/89						*
C* D. Kidwell/NCEP	 2/01	Rewrote, added more parts and ret code  *
C* F. J. Yen/NCEP	 7/01	Replaced uninitialized variable yyiprm	*
C*				with iprm.  (S. Chiswell/Unidata)	*
C************************************************************************
	REAL		parr(*), stdata(*)
	LOGICAL		kntflg, ddpflg
	CHARACTER*(*)	ptnam
C*	
	LOGICAL		ddpcvt, kntcvt
C------------------------------------------------------------------------
	iret   = 0
C
C*	Parts TTAA, TTBB, TTCC, TTDD, TRPA and TRPC can have a dew point
C*	depression conversion.
C
	ddpcvt = ddpflg .and. ( ptnam ( :1 ) .eq. 'T' )
C
C*	Parts TTAA, TTCC, PPAA, PPBB, PPCC, PPDD, MXWA, MXWC, TRPA and
C*	TRPC can have a wind speed conversion.
C
	kntcvt = kntflg .and. ( ( ptnam .ne. 'TTBB' ) .and.
     +				( ptnam .ne. 'TTDD' ) )
C
C*	Loop over number of parameters to store for part
C
	DO iprm = 1, nparm
	    ind = ( levnum - 1 ) * nparm + iprm
            stdata (ind) = parr (iprm)
C
C*          Convert data if necessary
C
	    IF ( iprm .eq. 3 ) THEN
		IF ( ddpcvt ) THEN
                    stdata(ind) = PR_DWDP ( parr(2), parr(iprm) )
		END IF
		IF ( kntcvt ) THEN
		    IF ( ptnam ( :1 ) .ne. 'T' ) THEN
		        stdata(ind) = PR_KNMS ( parr(iprm) )
		    END IF
		END IF
	      ELSE IF ( iprm .eq. 5 ) THEN
		IF ( kntcvt ) THEN
		    stdata(ind) = PR_KNMS ( parr(iprm) )
		END IF	    
	    END IF
        END DO
C*
	RETURN
	END
