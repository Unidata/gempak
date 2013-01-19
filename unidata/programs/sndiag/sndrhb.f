	SUBROUTINE SNDRHB ( nlun, lun, levout, pres, temp, dwpt,
     +			    iemlflg, rrhbrk, bseinv, iret )
C************************************************************************
C* SNDRHB								*
C*									*
C* This routine will compute the inversion base using the the RH break.	*
C*									*
C* SNDRHB ( NLUN, LUN, LEVOUT, PRES, TEMP, DWPT, IEMLFLG, RRHBRK,	*
C*	    BSEINV, IRET )						*
C*									*
C* Input parameters:							*
C*	NLUN		INTEGER		Number of file numbers		*
C*	LUN (NLUN)	INTEGER		File numbers			*
C*	LEVOUT		INTEGER		Number of interpolated levels	*
C*	PRES		INTEGER		Pressure			*
C*	TEMP		INTEGER		Temperature			*
C*	DWPT		INTEGER		Dew point			*
C*									*
C* Output parameters:							*
C*	IEMLFLG		INTEGER		Elevated mixed layer flag	*
C*	RRHBRK		INTEGER		Relative humidity break		*
C*	BSEINV		INTEGER		Base inversion			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 4/92						*
C* J. Whistler/SSAI	 4/93		Cleaned up header		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		lun(*), pres(*), temp(*), dwpt(*),
     +			rrhbrk, bseinv
C*
	INTEGER		sbltop, bottom, relhum, relnxt, plmt, bsetmp,
     +			bsehld, emltop, emlthk, bufbrk, bufbse
C------------------------------------------------------------------------
	iret  =  0
	xcrit =  0.87
	rhtol = 10
C
	IF  ( pres(1) .gt. 500. .and. pres(levout) .le. 300. )  THEN
C
C*	    Get above boundary layer.
C
	    sbltop = pres(1) - 30
	    j = 2
	    DO WHILE ( pres(j) .gt. sbltop )
		j = j + 1
	    END DO
C
C*	    Check for RH discontinuity.
C
	    ibasflg = 0
	    ibrkflg = 0
	    bufbrk  = 0
	    bufbse  = 0
	    ibufflg = 4
C
500	    CALL SNDRHF ( pres(j), temp(j), dwpt(j), bottom )
600	    j = j + 1
	    DO  WHILE ( pres(j-1) .gt. 400 .and. ibasflg .eq. 0 )
		CALL SNDRHF ( pres(j), temp(j), dwpt(j), relhum )
		IF  ( bottom .ge. 30 )  THEN
		    rhdiff = FLOAT ( bottom - relhum )
		    IF  ( rhdiff .gt. 0. ) THEN
			pdiff = FLOAT ( pres(j-1) - pres(j) )
			IF  ( pdiff .le. 100. )  THEN
			    xlayer1 = rhdiff / pdiff
			    IF  ( xlayer1 .ge. xcrit )  THEN
C
C*			        Test the next layer to see if its xlayer
C*			        is greater than the current xlayer.
C
				CALL SNDRHF (pres(j+1),temp(j+1),
     +					     dwpt(j+1), relnxt )
				rhdiff = FLOAT ( relhum - relnxt )
				IF  ( rhdiff .le. 0. )  THEN
				    pdiff = FLOAT(pres(j)-pres(j+1))
				    IF  ( pdiff .le. 100. )  THEN
					xlayer2 = rhdiff / pdiff
					IF  ( xlayer2 .gt.
     +					      xlayer1 )  THEN
C
C*				    	    Upper layer has a better
C*					    RH discontinuity.
C
					    j = j + 1
					    relhum = relnxt
					END IF
				    END IF
				END IF
C
C*				The level the RH discontinuity occurs.
C
				rrhbrk = pres(j-1)
				brkhld = relhum
				IF  ( bufbrk .eq. 0. )  bufbrk = rrhbrk
				ibrkj   = j - 1
				ibrkflg = 1
C
C*				Now find thermal inversion.
C
				IF  ( pres(j) .gt. 400 )  THEN
				    plmt   = rrhbrk - 100
				    bsetmp = temp(j-1)
				    invflg = 0
C
C*				    Check for the exsistance of a
C*				    thermal inversion.
C
				    DO  WHILE (pres(j).ge.plmt .and.
     +						invflg .eq. 0 )
					IF  ( temp(j) .ge. bsetmp )
     +					    invflg = 1
					j = j + 1
				    END DO
				    IF  ( invflg .eq. 1 )  THEN
C
C*					Success.
C*					Get to top of the thermal
C*					inversion.
C
				    	DO  WHILE ( pres(j-1) .ge. 400
     +						    .and.
     +					         temp(j).ge.temp(j-1) )
					    j = j + 1
					END DO
C
C*					Now refine the level of the
C*					inversion nose which is
C*					defined here as the level of
C*					the maximum THW.
C
				    	IF  (pres(j-1).gt.400)  THEN
					    plmt = plmt - 10
					    k = j - 1
					    IF  ( pres(k) .ge. plmt )
     +						THEN
					        CALL SNDTWB ( pres(k),
     +							      temp(k),
     +							      temp(k),
     +							      3, thw1,
     +							      iplcl )
					        thw2 = thw1
					        DO  WHILE (
     +						    pres(k-1).ge.plmt
     +							.and.
     +						    thw2.ge.thw1 )
						    k = k + 1
						    thw1=thw2
						    CALL SNDTWB (
     +							pres(k),
     +							temp(k),
     +							temp(k),
     +							3, thw2, iplcl)
					        END DO
					    END IF
					    IF  ( pres(k-1) .lt. plmt )
     +						THEN
					    	ibsek = j - 1
					    ELSE
					    	ibsek = k - 1
					    END IF
					    k = ibsek
					    bseinv = pres(k)
					    CALL SNDRHF ( pres(k),
     +							  temp(k),
     +							  dwpt(k),
     +							  bsehld )
				    	ELSE
C
C*					    Failed.
C
					    bseinv = 400
				    	END IF
C
C*					Determine characteristics of
C*					the layer above BSEINV.
C
				    	IF  (bseinv .gt. 400) THEN
					    CALL SNDEML ( pres,
     +							  temp,
     +							  dwpt,
     +							  ibsek,
     +							  bseinv,
     +						          bsehld,
     +							  levout,
     +							  iemlflg,
     +							  emltop,
     +							  emlthk,
     +							  thdavg )
				    	ELSE
					    emltop  = 0
					    emlthk  = 0
					    thdavg  = 0.
					    iemlflg = 4
				    	END IF
C
C*					If IEMLFLG = 2 then the layer
C*					above RRHBRK is too stable a
C*					layer to be an EML. Therefore,
C*					save values and continue upward
C*					to see if a better EML exists.
C
					IF  ( iemlflg .eq. 2 )  THEN
					    IF  ( bufbse .eq. 0. .or.
     +						  ibufflg .eq. 3 )  THEN
						bufbrk  = rrhbrk
						bufbse  = bseinv
						ibufflg = iemlflg
					    END IF
					    j = ibsek
					    GOTO 500
					END IF
C
C*					If there is an EML check to
C*					see if it is dry.
C
					IF  ( iemlflg .le. 1 )  THEN
					    plmt = bseinv-(emlthk/2)
					    j = ibsek
					    DO  WHILE (
     +						pres(j).ge.plmt .and.
     +						iemlflg.le.1 )
						CALL SNDRHF ( pres(j),
     +							      temp(j),
     +							      dwpt(j),
     +							      relhum )
						IF  ( relhum .gt.
     +						      (brkhld+rhtol) )
     +						    iemlflg = 3
						j = j + 1
					    END DO
					END IF
C
C*					If EML is polluted save level
C*					and see if there is a better
C*					EML above current level.
C
					IF  ( iemlflg .eq. 3 )  THEN
					    IF  ( bufbse .eq. 0 )  THEN
						bufbrk  = rrhbrk
						bufbse  = bseinv
						ibufflg = iemlflg
					    END IF
					    j = ibsek
					    GOTO 500
					END IF
				    ELSE
C
C*					Failed.
C*					Keep going, make another
C*					attempt.
C
					bottom = relhum
					j = ibrkj
					GOTO 600
				    END IF
				ELSE
				    bseinv = 400.
				END IF
				ibasflg = 1
			    END IF
			END IF
		    END IF
		END IF
		bottom = relhum
		j = j + 1
	    END DO
C
	    IF  ( ibasflg .eq. 0 )  THEN
C
C*		No thermal inversion:
C
		IF  ( ibrkflg .eq. 0 )  THEN
C
C*		    No RH break:
C
		    rrhbrk = 400.
		    bseinv = 400.
		ELSE
C
C*		    An RH break but no thermal inversion:
C
		    bseinv = 400.
		END IF
	    END IF
	    IF  ( bseinv .eq. 400. )  THEN
		IF  ( bufbrk .ne. 0. )  rrhbrk = bufbrk
		IF  ( bufbse .ne. 0. )  THEN
		    bseinv  = bufbse
		    iemlflg = ibufflg
		ELSE
		    iemlflg = 4
		END IF
	    END IF
	END IF
C
C*	Write the output
C
	DO  i = 1, nlun
	    WRITE ( lun(i), 1000 ) rrhbrk, bseinv
	END DO
1000	FORMAT ( ' Pressure of RH Discontinuity : ',I10,' mb',/,
     +           ' Pressure of Lid Base         : ',I10,' mb' )
C*
	RETURN
	END
