	SUBROUTINE BR_VTEC ( vtec, vact, vorig, vtype, vsig, vnum, 
     +			     vstrt, vend, vprdc, iret)
C************************************************************************
C* BR_VTEC 								*
C*									*
C* This subroutine parses the primary valid time event code string.  	*
C* The P-VTEC string being passed in should not have either of the '/'  *
C* attached to it.							*
C*									*
C* Information on the P-VTEC string can be found in: National Weather	*
C* Service Instruction 10-1703, Valid Time Event Code (VTEC).		* 
C*                                                                      *
C* BR_VTEC ( VTEC, VACT, VORIG, VTYPE, VSIG, VNUM, VSTR, VEND, VPRDC,   *
C*	     IRET )							*
C*									*
C* Input parameters:	                                                *
C*  	VTEC            CHAR*           Valid time event code string	*
C*									*
C* Output parameters:							*
C*	VACT		CHAR*		Action				*
C*	VORIG		CHAR*		Originating station		*
C*	VTYPE		CHAR*		Phenomena type 			*
C*	VSIG		CHAR		Significance code		*
C*      VNUM		CHAR*		Event Tracking Number		*
C*	VSTRT		CHAR*		Starting time, YYMMDD/HHNN	*
C*	VEND		CHAR*		Ending time, YYMMDD/HHNN	*
C*	VPRDC		CHAR*		Product Class			*
C*	IRET  	  	INTEGER	 	Return code			*
C*					-1 = Not a valid VTEC line	*
C*					-3 = Error breaking up VTEC     *
C*					-4 = Error breaking up VTEC time*
C*									*
C**									*
C* Log:									*
C* A. Hardy/NCEP	10/02						*
C* A. Hardy/NCEP	 1/03		Added check for 'T' in VTEC line*
C* A. Hardy/NCEP	 5/04		Add check for new/old VTEC line *
C* A. Hardy/NCEP	 5/04		Add check fo proper VTEC line	*
C* H. Zeng/SAIC		07/05		Added product class		*
C* F. J. Yen/NCEP	 3/08		Corrected output parms in prolog*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	vtec, vact, vorig, vtype, vsig, vnum, 
     +			vstrt, vend, vprdc
C*
	CHARACTER	carr(7)*28, tarr(2)*14
C------------------------------------------------------------------------
	iret  = 0
        ipos  = 0 
        vorig = ' '
        vsig  = ' '
        vnum  = ' '
        vstrt = ' '
        vend  = ' '
	vprdc = ' '
C
C*	Parse the VTEC string.
C
  	CALL ST_CLST ( vtec, '.', ' ', 7, carr, numb, ier )
        IF ( numb .eq. 7 ) THEN
            ipos = 1 
          ELSE IF ( (numb .lt. 6) .or. ( numb .gt. 7 ) ) THEN
            iret = -1 
            RETURN
        END IF 
C
        IF ( ier .eq. 0 ) THEN
	    vprdc = carr(ipos)
	    vact  = carr(ipos + 1)
	    vorig = carr(ipos + 2)
	    vtype = carr(ipos + 3)
	    vsig  = carr(ipos + 4)
	    vnum  = carr(ipos + 5)
          ELSE
            iret = -3 
            CALL DC_WLOG ( 2, 'BR', iret, vtec, ierr )
        END IF
C
C*      Check lengths if proper VTEC codes.
C
        CALL ST_LSTR ( vact, ilen1, ier )
        CALL ST_LSTR ( vorig, ilen2, ier )
        CALL ST_LSTR ( vtype, ilen3, ier )
        CALL ST_LSTR ( vsig, ilen4, ier )
        CALL ST_LSTR ( vnum, ilen5, ier )
	CALL ST_LSTR ( vprdc,ilen6, ier )
        IF ( (ilen1 .ne. 3 ) .or. ( ilen2 .ne. 4 ) .or. ( ilen3 .ne. 2 )
     +      .or. ( ilen4 .ne. 1) .or. ( ilen5 .ne. 4 ) 
     +	    .or. ( ilen6 .ne. 1 ) ) THEN
            iret = -1 
            RETURN
        END IF
C
C*	Parse the VTEC starting and ending time section.
C
  	CALL ST_CLST ( carr(ipos + 6), '-', ' ', 2, tarr, numb, ier )
C
        IF ( ier .eq. 0 ) THEN
            CALL ST_ALNM (  tarr(1)(7:7), ityp, ier )
            IF ( ityp .ne. 1 ) THEN
	        vstrt = tarr(1)(:6) // '/' // tarr(1)(8:11)
	        vend  = tarr(2)(:6) // '/' // tarr(2)(8:11)
              ELSE
	        vstrt = tarr(1)(:6) // '/' // tarr(1)(7:10)
	        vend  = tarr(2)(:6) // '/' // tarr(2)(7:10)
            END IF
          ELSE
            iret = -4 
            CALL DC_WLOG ( 2, 'BR', iret, carr(6), ierr )
        END IF
C*
	RETURN
	END
