	SUBROUTINE WC_VTEC ( vtec, nvt, dattim, wacd, ostn, wtype, 
     +			     signif, wnum, strtim, endtim, ifix, icancl, 
     +			     itst, iret)
C************************************************************************
C* WC_VTEC 								*
C*									*
C* This subroutine decodes the valid time event code in the watch	*
C* county notification bulletin.					*
C*                                                                      *
C* WC_VTEC ( VTEC, NVT, DATTIM, WACD, OSTN, WTYPE, SIGNIF, WNUM, STRTIM,*
C*	     ENDTIM, IFIX, ICANCL, ITST, IRET )				*
C*									*
C* Input parameters:	                                                *
C*  	VTEC            CHAR*           Valid time event code string	*
C*  	NVT             INTEGER		Number of VTEC lines		*
C*  	DATTIM          CHAR*           Date/time of bulletin		*
C*									*
C* Output parameters:							*
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
C* A. Hardy/NCEP	10/02						*
C* A. Hardy/NCEP	 2/03		Made VTEC variables arrays	*
C* A. Hardy/NCEP	 2/03		Check length of watch number	*
C* A. Hardy/NCEP	 8/04		Removed check for 'EXP' code	*
C* A. Hardy/NCEP	 3/05		Added storing VTEC action code	*
C* H. Zeng/SAIC		07/05		Added an argument for br_vtec	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	vtec(*), ostn(*), wtype(*), signif(*), wacd(*),
     +		        strtim(*), endtim(*), wnum(*), dattim
C*
        INTEGER		icancl(*), ifix(*)
	CHARACTER	vact*4, vorig*4, vtype*2, vstrt*11, vsig*2,
     +                  vend*11, vnum*5, vprdc*5
C------------------------------------------------------------------------
	iret   = 0

C
C*	Loop over VTEC lines found.
C
	DO ii = 1, nvt
            itst   = 0
            vact   = ' '
            vorig  = ' '
            vsig   = ' '
            vnum   = ' '
            vstrt  = ' '
            vend   = ' '
	    vprdc  = ' '
            icancl(ii) = 0
C
C*	    Break up the VTEC string.
C
	    CALL BR_VTEC ( vtec(ii), vact, vorig, vtype, vsig, vnum, 
     +		           vstrt, vend, vprdc, ier)

            IF ( ier .ne. 0 ) THEN
	        iret = -10
	        CALL DC_WLOG ( 0, 'DCWCN', iret, vtec(ii), ierrr )
                RETURN
            END IF
C
C*	    Check length of watch number.
C
	    CALL ST_LSTR ( vnum, len, ier ) 
            IF ( len .gt. 4 ) THEN
	        iret = -10
	        CALL DC_WLOG ( 0, 'DCWCN', iret, vtec(ii), ierrr )
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
C
C*	    Set any flags based upon the action parameter.
C*          Set correction flag.
C
            IF ( vact .eq. 'COR' ) THEN
                ifix (ii) = 1
              ELSE 
                ifix (ii) = 0
            END IF
C
C*          Set the watch type.
C
	    IF ( vtype .eq. 'TO' ) THEN
                wtype (ii) = 'TOR'
              ELSE IF ( vtype .eq. 'SV' ) THEN
                wtype (ii) = 'SVR'
              ELSE
                wtype (ii) = '   '
            END IF
C
C*          Set cancellation flag based on a canceled or expired report.
C
            IF ( vact .eq. 'CAN' ) THEN
                icancl (ii) = 1
              ELSE IF ( vact .eq. 'TES' ) THEN
                itst = 1
                icancl (ii) = 0
            END IF
        END DO
C*
	RETURN
	END
