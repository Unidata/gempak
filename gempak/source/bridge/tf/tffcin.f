	SUBROUTINE TF_FCIN ( carr, num, ipos, itype, ibtime, ietime,
     +			     itprob, ibday, ieday, tmidxc, iret )
C************************************************************************
C* TF_FCIN                                                             	*
C*                                                                      *
C* This subroutine gets the type of forecast change indicator and the   *
C* forecast hours to which the change applies.                          *
C*									*
C* TF_FCIN ( CARR, NUM, IPOS, ITYPE, IBTIME, IETIME, IBDAY, ITPROB,     *
C*           IRET )                                                     *
C*								        *
C* Input parameters:						        *
C*	CARR(*)		CHAR*		TAF report as a list of words   *
C*	NUM		INTEGER		Number of words in list         *
C*	IPOS		INTEGER		Index of forecast change indic. *
C*								        *
C* Output parameters:						        *
C*	ITYPE		INTEGER		Type of next change indicator   *
C*	ITPROB		INTEGER		Prob. value assoc. with TEMPO   *
C*	IBTIME		INTEGER		Begin hour of next change indic.*
C*	IETIME		INTEGER		End hour of next change indic.  *
C*	IBDAY 		INTEGER		Begin day of next chane indic.  *
C*	IEDAY 		INTEGER		End day of next chane indicator.*
C*	TMIDXC	 	INTEGER		Flag true if the data in new CFI*
C*	IRET		INTEGER		Return code                     *
C*					  1 = skip indic. after TEMPO   *
C*					  0 = normal return             *
C*					 -1 = bad time field            *
C**								        *
C* Log:								        *
C* D. Kidwell/NCEP	 9/02                                           *
C* D. Kidwell/NCEP	10/02	Added argument itprob for PROB w/ TEMPO *
C* L. Lin/NCEP    	04/08	returns the flag and times for the CFI  *
C* L. Lin/NCEP    	11/08	Accepted "BECM" or "BEC" as "BECMG"     *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*) 	carr (*)
        LOGICAL         tmidxc
C*
	CHARACTER	pval*2
C------------------------------------------------------------------------
	iret  = 0
	itype = IMISSD
        ibday = IMISSD
        ieday = IMISSD
        tmidxc = .false.
	pval  = ' '
C
C*	Check for itype = 1   - 'FM'.
C*	Allow variations FMhh, FMhhZ and FMhhmmZ.
C
	IF ( carr ( ipos ) ( :2 ) .eq. 'FM' ) THEN
	    itype = 1
	    CALL ST_LSTR ( carr ( ipos ), lens, ier )
C
C*      Decode FMGG/FMGGgg/Z
C
	    IF ( ( lens .eq. 4 ) .or. ( lens .eq. 6 ) .or. 
     +		 ( ( ( lens .eq. 5 ) .or. ( lens .eq. 7 ) ) .and.
     +		   ( carr ( ipos ) ( lens:lens ) .eq. 'Z' ) ) ) THEN
	        CALL ST_INTG ( carr ( ipos ) ( 3:4 ), ibtime, ier )
	        IF ( ier .eq. 0 ) THEN
	            IF ( lens .ge. 6 ) THEN
		        CALL ST_INTG ( carr ( ipos ) (5:6), ibmin, ier )
		      ELSE
C
C*			No minutes were given.
C
			ibmin = 0
		    END IF
		END IF
C
		IF ( ier .eq. 0 ) THEN
	            ietime = IMISSD
		    IF ( ibmin .ge. 30 )  ibtime = ibtime + 1
		    IF ( ibtime .ge. 24 ) ibtime = ibtime - 24
		END IF
C
C*      Decode FMYYGGgg/Z
C
            ELSE IF ( ( lens .eq. 8 ) .or. ( ( lens .eq. 9 ) .and.
     +      ( carr ( ipos ) ( lens:lens ) .eq. 'Z' ) ) ) THEN
	        CALL ST_INTG ( carr ( ipos ) ( 3:4 ), ibday, ier )
                tmidxc = .true.
                IF ( ier .eq. 0 ) THEN
	           CALL ST_INTG ( carr ( ipos ) ( 5:6 ), ibtime, ier )
	           IF ( ier .eq. 0 ) THEN
	               IF ( lens .ge. 8 ) THEN
		           CALL ST_INTG ( carr ( ipos ) (7:8), ibmin, ier )
		         ELSE
C
C*			   No minutes were given.
C
			   ibmin = 0
		       END IF
		   END IF
                END IF
C
		IF ( ier .eq. 0 ) THEN
	            ietime = IMISSD
		    IF ( ibmin .ge. 30 )  ibtime = ibtime + 1
		    IF ( ibtime .ge. 24 ) ibtime = ibtime - 24
		END IF
C

	      ELSE
		ier = -1
	    END IF
C
C*	  Check for itype = 2	- 'BECMG'
C*			    3	- 'TEMPO'
C*			    4	- 'PROB30'
C*			    5	- 'PROB40'.
C
          ELSE
	    IF ( carr ( ipos ) .eq. 'BECMG' .or.
     +           carr ( ipos ) .eq. 'BECM' .or. 
     +           carr ( ipos ) .eq. 'BEC' ) THEN
		itype = 2
	      ELSE IF ( carr ( ipos ) .eq. 'TEMPO' ) THEN
		itype = 3
	      ELSE IF ( carr ( ipos ) .eq. 'PROB30' ) THEN
		itype = 4
	      ELSE IF ( carr ( ipos ) .eq. 'PROB40' ) THEN
		itype = 5
	    END IF
	    ipos1 = ipos + 1
C
C*	    Check for 'PROB' or 'BECMG' after 'TEMPO'.
C
	    IF ( itype .eq. 3 ) THEN
		IF ( ( carr ( ipos1 ) ( :4 ) .eq. 'PROB' ) .or.
     +		     ( carr ( ipos1 ) .eq. 'BECMG' ) .or.  
     +		     ( carr ( ipos1 ) .eq. 'BECM' ) .or.  
     +		     ( carr ( ipos1 ) .eq. 'BEC' ) ) THEN
		    IF ( carr ( ipos1 ) ( :4 ) .eq. 'PROB' )
     +			 pval = carr ( ipos1 ) ( 5:6 )
		    ipos1 = ipos1 + 1
		    iret  = 1
		END IF
	    END IF
C
	    CALL ST_LSTR ( carr ( ipos1 ), lens, ier )
	    IF ( lens .eq. 4 ) THEN
	        CALL ST_INTG ( carr ( ipos1 ) ( :2 ), ibtime, ier )
	        IF ( ier .eq. 0 )  CALL ST_INTG ( carr ( ipos1 ) (3:4), 
     +					          ietime, ier )
C
C*		For BECMG, use the midpoint time as the start time.
C
		IF ( ( itype .eq. 2 ) .and. ( ier .eq. 0 ) ) THEN
		    ibecmg = NINT ( FLOAT (ibtime + ietime) * .5 - .2 )
		    IF ( ietime .lt. ibtime ) ibecmg = ibecmg + 12
		    IF ( ibecmg .ge. 24 ) ibecmg = ibecmg - 24
		    ibtime = ibecmg
		    ietime = IMISSD
		END IF
C
C*		Check for valid hour values.
C
		IF ( ier .eq. 0 ) THEN
		    IF ( ibtime .eq. 24 ) ibtime = 0
		    IF ( ( ibtime .lt. 0 ) .or. ( ibtime .gt. 23 ) ) 
     +			 ier = -1
		    IF ( ietime .ne. IMISSD ) THEN
			IF ( ( ietime .lt. 0 ) .or. ( ietime .gt. 24 ) )
     +			     ier = -1
		    END IF
		END IF
C             
C*            Here, with a new forecast change indicator format
C*            such as YYGG/yygg, mainly for TEMPO or BECMG
C
              ELSE IF ( ( lens .eq. 9 ) .and. ( carr(ipos1)(5:5) 
     +                   .eq. '/' ) ) THEN
                tmidxc = .true.
	        CALL ST_INTG ( carr ( ipos1 ) ( :2 ), ibday, ier1 )
	        IF ( ier1 .eq. 0 )  CALL ST_INTG ( carr ( ipos1 ) (3:4), 
     +					          ibtime, ier2 )
	        IF ( ier2 .eq. 0 )  CALL ST_INTG ( carr ( ipos1 ) (6:7), 
     +					          ieday, ier3 )
	        IF ( ier3 .eq. 0 )  CALL ST_INTG ( carr ( ipos1 ) (8:9), 
     +					          ietime, ier4 )
                IF ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) .or.
     +               ( ier3 .ne. 0 ) .or. ( ier4 .ne. 0) )  THEN
                   ier = -1
                END IF
C
	      ELSE
		ier = -1
C
C*		Check for 'TEMPO' after 'PROB'.
C
		IF ( ( itype .ge. 4 ) .and.
     +		     ( carr ( ipos1 ) .eq. 'TEMPO' ) ) 
     +		       pval = carr ( ipos ) ( 5:6 )
	    END IF 
	END IF
	IF ( ier .ne. 0 ) iret = -1
	CALL ST_NUMB ( pval, itprob, ier )
C
	IF ( ( ibtime .ge. 24 ) .or. ( ietime .gt. 24 ) ) iret = -1
C*
	RETURN
	END
