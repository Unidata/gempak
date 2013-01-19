	SUBROUTINE FA_VTEC ( vtec, nvt, dattim, prdc, wacd, ostn, wtype, 
     +			     signif, wnum, strtim, endtim, ifix, icancl,
     +			     itst, iret)
C************************************************************************
C* FA_VTEC 								*
C*									*
C* This subroutine decodes the valid time event code in the flash flood	*
C* watch  bulletin.							*
C*                                                                      *
C* FA_VTEC ( VTEC, NVT, DATTIM, PRDC, WACD, OSTN, WTYPE, SIGNIF, WNUM,  *
C*	     STRTIM, ENDTIM, IFIX, ICANCL, ITST, IRET )			*
C*									*
C* Input parameters:	                                                *
C*  	VTEC            CHAR*           Valid time event code string	*
C*  	NVT             INTEGER		Number of VTEC lines		*
C*  	DATTIM          CHAR*           Date/time of bulletin		*
C*									*
C* Output parameters:							*
C*	PRDC		CHAR*		Product Class			*
C*	WACD		CHAR*		Action code 			*
C*	OSTN		CHAR*		Originating station		*
C*	WTYPE		CHAR*		Watch type  			*
C*	SIGNIF		CHAR		Watch significance code		*
C*	WNUM		CHAR*		Watch number			*
C*	STRTIM		CHAR*		Starting time, GEMPAK format	*
C*	ENDTIM		CHAR*		Ending time, GEMPAK format	*
C*	IFIX  	  	INTEGER	 	Correction flag			*
C*					    0 = no correction		*
C*					    1 = corrected		*
C*      ICANCL          INTEGER		Cancel flag			*
C*					    0 = new report		*
C*					    1 = Cancelled/expired 	*
C*      ITST           INTEGER		Test flag			*
C*					    0 = no test report		*
C*					    1 = test report		*
C*	IRET  	  	INTEGER	 	Return code			*
C*				         -10 = Error with VTEC string	*
C*									*
C**									*
C* Log:									*
C* H. Zeng/SAIC		07/05		Copied from wc_vtec		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	vtec(*), ostn(*), wtype(*), signif(*), wacd(*),
     +		        strtim(*), endtim(*), wnum(*), prdc(*), dattim
C*
        INTEGER		icancl(*), ifix(*), itst(*)
	CHARACTER	vact*4, vorig*4, vtype*2, vstrt*11, vsig*2,
     +                  vend*11, vnum*5, vprdc*4
C------------------------------------------------------------------------
	iret   = 0

C
C*	Loop over VTEC lines found.
C
	DO ii = 1, nvt
            itst(ii)   = 0
	    ifix (ii)  = 0
	    vprdc  = ' '
            vact   = ' '
            vorig  = ' '
            vsig   = ' '
            vnum   = ' '
            vstrt  = ' '
            vend   = ' '
            icancl(ii) = 0
C
C*	    Break up the VTEC string.
C
	    CALL BR_VTEC ( vtec(ii), vact, vorig, vtype, vsig, vnum, 
     +		           vstrt, vend, vprdc, ier)

            IF ( ier .ne. 0 ) THEN
	        iret = -10
	        CALL DC_WLOG ( 0, 'DCFFA', iret, vtec(ii), ierrr )
                RETURN
            END IF
C
C*	    Check length of watch number.
C
	    CALL ST_LSTR ( vnum, len, ier ) 
            IF ( len .gt. 4 ) THEN
	        iret = -10
	        CALL DC_WLOG ( 0, 'DCFFA', iret, vtec(ii), ierrr )
                RETURN
            END IF
C
C* 	    Set the variables.	
C
	    wacd (ii) = vact
	    ostn (ii) = vorig
            signif (ii) = vsig
            wnum   (ii) = vnum(:len)
	    strtim (ii) = vstrt  
	    endtim (ii) = vend   
	    wtype  (ii) = vtype
	    prdc   (ii) = vprdc
C
C*	    Set any flags based upon the action parameter.
C*          Set correction flag and cancallation flag.
C
            IF ( vact .eq. 'COR' ) THEN
                ifix (ii)   = 1
            ELSE IF ( vact .eq. 'CAN' )  THEN 
		icancl (ii) = 1
            END IF
C
C*          Set test flag based on the value of Product Class.
C
            IF ( vprdc(1:1) .eq. 'T' )  itst(ii) = 1
C   
        END DO
C*
	RETURN
	END
