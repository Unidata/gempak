	FUNCTION PS_LIFT ( datain, nparm, tvflag )
C************************************************************************
C* PS_LIFT								*
C*									*
C* This function computes the lifted index:				*
C*									*
C*	LIFT = T500 - Tparcel						*
C*									*
C*	     T500    = 500 mb temperature in Celsius			*
C*									*
C*	     Tparcel = 500 mb parcel temperature in Celsius on the	*
C*		       moist adiabat determined by the average		*
C*		       pressure, temperature and dewpoint in the	*
C*		       layer from the surface to 100 mb above the	*
C*		       surface						*
C*									*
C* TVFLAG determines if temperature or virtual temperature is used	*
C* in the calculation.							*
C*									*
C* REAL PS_LIFT ( DATAIN, NPARM, TVFLAG )				*
C*									*
C* Input parameters:							*
C*	DATAIN(NPARM,*)	REAL		Station data			*
C*	NPARM		INTEGER		Number of parameters		*
C*	TVFLAG		LOGICAL		Temperature flag		*
C*									*
C* Output parameters:							*
C*	PS_LIFT		REAL		Lifted index			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 9/86						*
C* M. desJardins/GSFC	10/86	Added GEMPAK parameter names		*
C* M. desJardins/GSFC	 9/87	Corrected indicies in call to PC_CMDT	*
C* M. desJardins/GSFC	 3/88	Documentation				*
C* G. Huffman/USRA	 8/89	Move error check to PC_CMDT, clean up	*
C* M. desJardins/GSFC	 7/90	GEMPAK 5				*
C* T. Lee		 8/97	Added a temperature flag		*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		datain (*)
C*
	REAL		datlev ( MMPARM )
	LOGICAL		tvflag
C*
	INCLUDE		'ERMISS.FNC'
	DATA		datlev / MMPARM * RMISSD /
C------------------------------------------------------------------------
	PS_LIFT = RMISSD
C
C*	Get values in parcel.
C
	CALL PS_PRCL  ( datain, nparm, 100., 1, 1, depth, idcord, pavg,
     +			tavg, tdavg, uavg, vavg, zavg, thavg, rmxavg,
     +			ier )
	IF  ( ( ier .ne. 0 ) .or. ERMISS (tavg) .or. ERMISS (tdavg) )
     +								THEN
	    RETURN
	end if

C
C*	Calculate equivalent potential temperature in average layer.
C*	Then compute parcel temperature on moist adiabat.
C
	thte  = PR_THTE  ( pavg, tavg, tdavg )
	guess = 0.
	tmpe  = PR_TMST  ( thte, 500., guess )
	IF  ( tvflag )  THEN
	    tmpc  = PR_TMKC  ( tmpe )
	    tmpe  = PR_TVRK  ( tmpc, tmpc, 500. )
	END IF
C
C*	Get temperature at 500 millibars.
C
	CALL PC_CMDT  ( 5, 6, 7, 500., 1, datain, datlev, ier )
	IF  ( tvflag )  THEN
	    t500 = datlev (7)
	  ELSE
	    t500 = datlev (2)
	END IF
C
C*	Check for errors and then compute lifted index.
C
	IF ( ERMISS (t500) .or. ERMISS (thte) .or. ERMISS (tmpe) ) THEN
	    RETURN
	  ELSE
	    PS_LIFT = t500 - PR_TMKC ( tmpe )
	END IF
C*
	RETURN
	END
