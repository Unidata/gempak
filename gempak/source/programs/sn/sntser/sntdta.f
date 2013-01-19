	SUBROUTINE SNTDTA  ( isnfln, vlevel, lvert, parm,
     +			     stnprm, timfil, ntim,  timfnd, npts,  
     +			     x, y,   iret )
C************************************************************************
C* SNTDTA								*
C*									*
C* This subroutine gets data from the sounding file for SNTSER.		*
C*									*
C* SNTDTA  ( ISNFLN, VLEVEL, LVERT, PARM, STNPRM, TIMFIL, NTIM,		*
C*           TIMFND, NPTS,   X, Y,  IRET )				*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	VLEVEL		REAL		Level				*
C*	LVERT		INTEGER		Output vertical coordinate	*
C*	PARM		CHAR*		Level parameter			*
C*	STNPRM		CHAR*		Station parameter		*
C*	TIMFIL (NTIM)	CHAR*		File time list			*
C*	NTIM		INTEGER		Number of file times		*
C*									*
C* Output parameters:							*
C*	TIMFND (NPTS)	CHAR*		Time list			*
C*	NPTS		INTEGER		Number of times			*
C*	X      (NPTS)	REAL		X coordinate values		*
C*	Y      (NPTS)	REAL		Y coordinate values		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = no data listed		*
C**									*
C* Log:									*
C* G. Huffman/USRA	 5/89	Adapted from SNLDTA			*
C* K. Brill/NMC		02/92	CALL PC_CMVR not _CMVS			*
C* K. Brill/NMC		 8/93	stid*4 -> stid*8			*
C* A. Hardy/GSC		 3/99   Added priority parameter to PC_SSTN     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	parm,  stnprm, timfil (*), timfnd (*)
	REAL		x (*), y (*)
C*
	REAL		data ( LLMXDT ), rdata ( LLMXDT )
	CHARACTER	stntim*20, stid*8, cdata ( MMPARM )*8
	LOGICAL		good, misspt
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Loop through times finding data.
C
	npts   = 0
	misspt = .false.
	DO  i = 1, ntim
C
C*	    Initialize Y-related values to missing.
C
	    rdata (1) = RMISSD
	    yy        = RMISSD
C
C*	    Set the time and station.
C
	    CALL SN_STIM  ( isnfln, timfil (i), ister )
	    IF  ( ister .eq. 0 )  CALL SN_SNXT  ( isnfln, stid, istnm, 
     +			    		      slat, slon, selv, ister )
C
C*	    Get the data if ISTER = 0; otherwise, set missing flags.
C
	    IF  ( ister .eq. 0 )  THEN
		CALL SN_RDAT  ( isnfln, ndlev, data, stntim, ister )
		IF  ( ister .eq. 0 )  THEN
		    good = .true.
		    ispri = 0
		    CALL PC_SSTN  ( stid, istnm, slat, slon, selv,
     +				    ispri, stntim, ndlev, ier )
		  ELSE
		    good = .false.
		END IF
	      ELSE
		good   = .false.
		misspt = .true.
	    END IF
C
C*	    Extract the level data.
C
	    IF  ( good )  THEN
	        IF  ( parm .ne. ' ' )  THEN
		    CALL PC_CMVR  ( vlevel, lvert, data, rdata, cdata,
     +				    ier )
		    yy = rdata (1)
C
C*	          Extract the station data.
C
		  ELSE IF  ( stnprm .ne. ' ' )  THEN
		    CALL PC_CMST  ( data, rdata, cdata, ier )
		    yy = rdata (1)
		END IF
	    END IF
C
C*	    For non-missing data convert the time found to minutes 
C*	    after the first (good)  time.  YY is still RMISSD if it
C*	    didn't get a value above.
C
	    IF  ( .not. ERMISS ( yy ) )  THEN
		npts            = npts + 1
		timfnd ( npts ) = timfil ( i )
		CALL TI_DIFF  ( timfnd (npts), timfnd (1), minx, ier )
		x (npts) = FLOAT ( minx )
		y (npts) = yy
	    END IF
	END DO
C
C*	Check the missing time-station flag and check that there are 
C*	some points.
C
	IF  ( misspt )  CALL ER_WMSG  ( 'SN', ister, ' ', ier )
	IF  ( npts .le. 0 )  THEN
	    iret = -6
	    CALL ER_WMSG  ( 'SNTSER', iret, ' ', ier )
	END IF
C*
	RETURN
	END
