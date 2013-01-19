	SUBROUTINE NDFDPDS  ( rnvblk, gparm, gvcrd, level, idtarr,
     +			       fcstim, gpid, ipds, iret )
C************************************************************************
C* NDFDPDS								*
C*									*
C* This subroutine combines user input, grid header information, and	*
C* information from other sections to make the full GRIB PDS.		*
C*									*
C* NDFDPDS ( RNVBLK, GPARM, GVCRD, LEVEL, IDTARR, FCSTIM, GPID, IPDS,	*
C*								IRET )	*
C*									*
C* Input parameters:							*
C*	RNVBLK(*)	REAL		Grid navigation block		*
C*	GPARM		CHAR*		GEMPAK parameter name string	*
C*	GVCRD		INTEGER		GEMPAK VCORD name string	*
C*	LEVEL(2)	INTEGER		GEMPAK vert level values	*
C*	IDTARR(5)	INTEGER		Initial Time Array		*
C*	FCSTIM		INTEGER		Forecast time in hours		*
C*	GPID		INTEGER		Generating Process ID		*
C*									*
C* Output parameters:							*
C*	IPDS (NBYTS)	INTEGER		GRIB PDS octets			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* T. Piper/SAIC	 3/03	Created from PDS_MAKE			*
C* T. Piper/SAIC	05/03	Replaced NDFDPARM with CTB_G2GNUM	*
C* T. Piper/SAIC	08/03	Added CPC support			*
C* C. Bailey/HPC	07/04	Added NDFD parameters			*
C* T. Piper/SAIC	08/04	Removed idhm parameter			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		rnvblk(*)
	INTEGER		gvcrd, level(2), gpid
	CHARACTER*(*)	gparm
	INTEGER		fcstim, ipds(*), idtarr(5), jdtarr(5), minuts
	LOGICAL		proctime
C*
C------------------------------------------------------------------------
	iret = 0
C
	ipds(5) = 4 
C!	Number of section
C
	ipds(6) = 0 
C!	Number of coordinates values after Template (see Note 1)
C
	CALL ST_NULL(gparm, gparm, lenp, iret)
	CALL CTB_G2GNUM(gparm, idisp, ipds(10), ipds(11), ipds(8), iret)
	IF ( iret .ne. 0 )  THEN
	    CALL ER_WMSG ( 'CTB', iret, ' ', ier )
	    return
	END IF
C!	ipds(10) = Product Definition Template (see Template 4.X, where X is the
C!	           Product Definition Template Number given in octects 8-9)
C!	ipds(11) = Parameter Number
C!	ipds(8) = Product Definition Template Number (see Code Table 4.0)
C
C!	Determine if this is a CPC Product
C
	IF ( ipds(8) .gt. 9 )  THEN
	   ipds(35) = ipds(8) / 10
	   ipds(8) = 9
	   icpc = 1
	ELSE
	   icpc = 0
	END IF 
	ipds(12) = 2
C!	Type of generating process (see Code Table 4.3)
C!	2 == Forecast
	ipds(13) = 0
C!	Background generating process identifier	
	ipds(14) = gpid
C!	Analysis or forecast generating process identifier
	ipds(15) = 65535
C!	Hours of observational data cutoff after reference time
	ipds(17) = 255
C!	Minutes of observational data cutoff after reference time
        
	IF ( icpc .gt. 0 )  THEN
	    IF ( fcstm .gt. 0 )  THEN
	        ipds(18) = 3
	    ELSE
		ipds(18) = 2	
	    END IF
	ELSE
	    ipds(18) = 1
	END IF
C!	Indicator of unit of time range (see Code Table 4.4)
C!	1 == Hour, 2 = Day, 3 = Month
C
	ipds(19) = fcstim 
C!	Forecast time in units defined by octet 18
	IF ( gvcrd .eq. 0 ) THEN
	    ipds(23) = 1
	    ipds(24) = -1
	    ipds(25) = -1
	ELSE
	    print *, "Vertical coordinate not NONE - cannot process."
	END IF	
	DO I = 26, 29
	    ipds(I) = 65535
	END DO	
	ipds(30) = -1
	ipds(31) = -1
	DO I = 32, 34
	    ipds(I) = 65535
	END DO
	minuts = fcstim*60
	CALL TI_ADDM(idtarr, minuts, jdtarr, iret)
	IF (iret .ne. 0) THEN
	    proctime = .false.
	ELSE
	    proctime = .true.
	END IF
C!	Add Forecast Hours (in Minutes) to the Initial Time	
	IF ( ipds(8) .eq. 8 )  THEN
	    IF ( proctime ) THEN 
	        ipds(35) = jdtarr(1)
	        ipds(37) = jdtarr(2)
	        ipds(38) = jdtarr(3)
	        ipds(39) = jdtarr(4)
	        ipds(40) = jdtarr(5)
	        ipds(41) = 0
	    END IF    
	    IF ( ipds(11) .eq. 4 ) THEN
	        ipds(47) = 2
	        ipds(49) = 1
	        ipds(50) = 12		
	        ipds(54) = 1
	    ELSE IF ( ipds(11) .eq. 5 ) THEN
	        ipds(47) = 3
	        ipds(49) = 1
	        ipds(50) = 12	
	        ipds(54) = 1
	    END IF
C! 	    Statistical Process Used To Calculate Processed Fields 	    
	    ipds(42) = 1
	    ipds(43) = 0
	    ipds(48) = 65535
	END IF
	IF ( ipds(8) .eq. 9 )  THEN
	    IF ( icpc .gt. 0 )  THEN
		ipds(36) = 3
	    ELSE
	        ipds(35) = 65535
	        ipds(36) = 65535
	    END IF
	    ipds(37) = 1
c	    ipds(38) = 65535
c	    ipds(39) = 65535
	    ipds(38) = -1
	    ipds(39) = -1
C!	    Scale Factor for upper limit
	    ipds(43) = 3
	    ipds(44) = 254
	    ipds(55) = 1
	    ipds(56) = 0
	    ipds(60) = 1
	    ipds(61) = 65535	    
	    IF ( proctime ) THEN 
	        ipds(48) = jdtarr(1)
	        ipds(50) = jdtarr(2)
	        ipds(51) = jdtarr(3)
	        ipds(52) = jdtarr(4)
	        ipds(53) = jdtarr(5)
	        ipds(54) = 0
	    END IF
C!	    Set End Of Forecast Period
	END IF
C!	Set time range
	ipos = 0
	DO I = 1, lenp 
	    CALL ST_ALNM(gparm(I:I), ityp, ier)
	    IF ( ityp .eq. 1 )  THEN
		ipos = I
		GO TO 10
	    END IF
	END DO
10	CONTINUE
	IF ( ipos .gt. 0 )  THEN
	    IF ( ipds(8) .eq. 8 )  THEN 
	        indx = 49		
	    ELSE IF ( ipds(8) .eq. 9 )  THEN 
	        indx = 62
	    END IF
	    ipds(indx) = 1
	    CALL ST_INTG(gparm(ipos:ipos+1), intg, ier)
	    ipds(indx+1) = intg 
	    ipds(indx+5) = 1
	    ipds(indx+6) = 0
	END IF
C*
	RETURN
	END
