	SUBROUTINE SNDEML ( pres, temp, dwpt, ibasek, bseinv, rhhold,
     +			    levout, iemlflg, emltop, emlthk, thdavg )
C************************************************************************
C* SNDEML								*
C*									*
C* This routine will compute variables related to the elevated mixed	*
C* layer (EML).								*
C*									*
C* SNDEML ( PRES, TEMP, DWPT, IBASEK, BSEINV, RHHOLD, LEVOUT, IEMLFLG,	*
C*	    EMLTOP, EMLTHK, THDAVG )					*
C*									*
C* Input parameters:							*
C*	PRES (LEVOUT)	INTEGER		Pressure			*
C*	TEMP (LEVOUT)	INTEGER		Temperature			*
C*	DWPT (LEVOUT)	INTEGER		Dew point			*
C*	IBASEK		INTEGER		Pressure level of base invrsn	*
C*	BSEINV		INTEGER		Height of base inversion	*
C*	RHHOLD		INTEGER		Previous RH break		*
C*	LEVOUT		INTEGER		Number of levels		*
C*									*
C* Output parameters:							*
C*	IEMLFLG		INTEGER		Elevated mixed layer flag	*
C*	EMLTOP		INTEGER		Elevated mixed layer height	*
C*	EMLTHK		INTEGER		Thickness of EML		*
C*	THDAVG		INTEGER		Average THTE in EMLTHK		*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 4/92						*
C* J. Whistler/SSAI	 4/93		Cleaned up header		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		pres(*), temp(*), dwpt(*), bseinv, rhhold,
     +			emltop, emlthk
C*
	INTEGER		pdiff, reltop
C------------------------------------------------------------------------
C*	Initialize the static stability limits.
C
	sigmin = 4.5
	sigmax = 7.0
	errbar = 1.5
C
C*	Buffer level index so it won't change the argument.
C*	K is index value of the pressure level of BSEINV.
C
	k = ibasek
C
C*	The top of the EML is defined where the static stability
C*	(SIGMA) within the layer becomes greater than SIGMIN
C*	(initialized above).
C
	theta1 = PR_THTA ( FLOAT(temp(k)), FLOAT(pres(k)) )
	theta2 = PR_THTA ( FLOAT(temp(k+1)), FLOAT(pres(k+1)) )
	theavg = ( theta1 + theta2 ) / 2.
	tmpavg = FLOAT ( temp(k) + temp(k+1) ) / 2.
	thdiff = theta1 - theta2
	pdiff  = pres(k) - pres(k+1)
	IF  ( ABS(thdiff) .le. errbar)  THEN
	    sigma = 0.
	ELSE
	    sigma = -( tmpavg/theavg ) * ( thdiff/FLOAT(pdiff) ) * 100.
	END IF
	avgthe = theta1
	icount = 1
C
C*	If the first layer above BSEINV is too stable and it is only
C*	50 mb or less thick, check the next layer to see if it is EML 
C*	material.
C
	IF  ( (sigma .gt. sigmin) .and. (sigma .lt. sigmax) .and.
     +	      (k .le. levout-2) )  THEN
	    IF  ( pdiff .le. 50 )  THEN
		theta3 = PR_THTA ( FLOAT(temp(k+2)),FLOAT(pres(k+2)) )
		theavg = ( theta2 + theta3 ) / 2.
		tmpavg = FLOAT ( temp(k+1) + temp(k+2) ) / 2.
		thdiff = theta2 - theta3
		IF  ( ABS(thdiff) .le. errbar )  THEN
		    sigma = 0.
		ELSE
		    sigma = -(tmpavg/theavg) *
     +			   (thdiff / FLOAT(pres(k)-pres(k+1))) * 100.
		END IF
		IF  ( sigma .le. sigmin )  THEN
C
C*		BETTER EML ABOVE 1ST LAYER SO CONTINUE UPWARD:
C
		    k = k + 1
		    avgthe = avgthe + theta2
		    icount = icount + 1
		    theta2 = theta3
		END IF
	    END IF
	END IF
C
C*	If SIGMA <= SIGMIN then the layer above BSEINV is unstable
C*	enough to be considered an elevated mixed layer. Find the top.
C
	IF  ( sigma .le. sigmin )  THEN
	    DO  WHILE ( (k .le. levout-2) .and. (sigma .le. sigmin) )
		k = k + 1
		theta1 = theta2
		theta2 = PR_THTA ( FLOAT(temp(k+1)),FLOAT(pres(k+1)) )
		tmpavg = FLOAT ( temp(k) + temp(k+1) ) / 2.
		theavg = ( theta1 + theta2 ) / 2.
		thdiff = theta1 - theta2
		IF  ( ABS(thdiff) .le. errbar )  THEN
		    sigma = 0.
		ELSE
		    sigma = -(tmpavg/theavg) *
     +			   ( thdiff / FLOAT(pres(k)-pres(k+1))) * 100.
		END IF
		avgthe = avgthe + theta1
		icount = icount + 1
C
		IF  ( (sigma .gt. sigmin) .and.
     +		      (sigma .lt. sigmax) .and.
     +		      (k .le. levout-2) )  THEN
C
C*		    This layer failed so check to see if the next
C*		    level is EML material:
C
		    theta1 = theta2
		    theta2 = PR_THTA(FLOAT(temp(k+2)),FLOAT(pres(k+2)))
		    theavg = ( theta1 + theta2 ) / 2.
		    tmpavg = FLOAT ( temp(k+1) + temp(k+2) ) / 2.
		    thdiff = theta1 - theta2
		    IF  ( ABS(thdiff) .le. errbar )  THEN
			sigma = 0.
		    ELSE
			sigma = -(tmpavg/theavg) *
     +			      (thdiff/FLOAT(pres(k+1)-pres(k+2))) * 100.
		    END IF
		    IF  ( sigma .le. sigmin )  THEN
C
C*			Continue upward.
C
			k = k + 1
			avgthe = avgthe + theta1
			icount = icount + 1
		    END IF
		END IF
	    END DO
	    thdavg  = PR_TMKC ( avgthe / FLOAT(icount) )
	    emltop  = pres(k)
	    emlthk  = bseinv-pres(k)
	    iemlflg = 1
	    CALL SNDRHF ( pres(k), temp(k), dwpt(k), reltop )
	    IF  ( reltop .gt. rhhold )  iemlflg = 0
	ELSE
C
C*	    Layer above PBL is too stable to be an EML.
C
	    iemlflg = 2
	    emltop  = 0.
	    emlthk  = 0.
	    thdavg  = PR_TMKC ( avgthe )
	END IF
C*
	RETURN
	END
