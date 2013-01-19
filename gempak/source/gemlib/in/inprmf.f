	SUBROUTINE IN_PRMF  ( prmfil, nparm, parms, iscale, iofset,
     +                        ibits, pkflg, iret )
C************************************************************************
C* IN_PRMF								*
C*									*
C* This subroutine receives the user input for parameter packing	*
C* and returns the packing information.  The input is either the	*
C* name of a file containing the information or the information		*
C* itself entered as follows:						*
C*									*
C*     PRM1/MIN1-MAX1-RES1;PRM2/MIN2-MAX2-RES2; ...			*
C* 									*
C* where PRMn is the parameter name, MINn is the minimum for PRMn,	*
C* MAXn is the maximum for PRMn, and RESn is the resolution.		*
C*									*
C* IN_PRMF ( PRMFIL, NPARM, PARMS, ISCALE, IOFSET, IBITS, PKFLG,	*
C*           IRET )							*
C*									*
C* Input parameters:							*
C*	PRMFIL		CHAR*		Input packing information	*
C*									*
C* Output parameters:							*
C*	NPARM		INTEGER		Number of parameters		*
C*	PARMS  (NPARM)	CHAR*		Parameter names			*
C*	ISCALE (NPARM)  INTEGER		Scaling for packing		*
C*	IOFSET (NPARM)	INTEGER		Offset for packing		*
C*	IBITS  (NPARM)	INTEGER		Number of packing bits		*
C*	PKFLG           LOGICAL		Packing flag			*
C*	IRET		INTEGER		Return code			*
C*				    	  0 = normal return		*
C*				   	 -9 = invalid packing info	*
C*					-10 = all parms must be packed	*
C*					-11 = all parms must be 4 char	*
C**									*
C* Log:									*
C* K. Brill/NMC		 9/90						*
C* J. Whistler/SSAI	 6/91	Removed call to FL_INQR			*
C* K. Brill/NMC		 9/91	Fixed false non-zero return code	*
C* K. Brill/NMC		 9/91	Added -11 error				*
C* M. desJardins/NMC	10/91	len-->lens; print parm in error msg	*
C* K. Brill/NMC		01/92	Change all len to lens			*
C* T. Piper/GSC		11/98	Updated prolog				*
C* S. Jacobs/NCEP	 5/01	Check for file before trying to read it	*
C* S. Jacobs/NCEP	 5/01	Changed to use FL_TINQ for table check	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*) 	prmfil, parms (*)
	LOGICAL 	pkflg
	INTEGER		iscale (*), iofset (*), ibits (*)
C*
	CHARACTER       pstrng (MMPARM) * 32, newfil (132)
	REAL		rval (3)
	LOGICAL		exist
C------------------------------------------------------------------------
	iret = 0
	pkflg = .false.
C
C*	Check to see if the input is blank.
C
	IF ( prmfil .eq. ' ' ) 	THEN
	    iret = -9
	    CALL ER_WMSG ( 'IN', iret, ' ', ier )
	    RETURN
	END IF
C
C*	Check to see if the file exists, read it; if not, get the
C*	information for PRMFIL itself.
C
	CALL FL_TINQ  ( prmfil, 'pack', exist, newfil, ier )
	IF  ( exist )  THEN
	    CALL DP_FILE  ( prmfil, nparm, parms, iscale, iofset,
     +			    ibits, pkflg, ireto)
	    IF  ( ireto .ne. 0 )  THEN
		CALL ER_WMSG ( 'DP', ireto, ' ', ier )
		iret = -9
		CALL ER_WMSG ( 'IN', iret, ' ', ier )
	    END IF
	  ELSE
C
C*	    Get the parameter information form the input.
C
	    CALL ST_CLST ( prmfil, ';', ' ', MMPARM, pstrng, nparm,
     +			   ier )
C
C*	    Check for packing information.
C
	    ipack = 0
	    DO ip = 1, nparm
	        islsh = INDEX ( pstrng (ip), '/' )
C
C*	        Check for invalid parameter entry.
C
		CALL ST_LSTR ( pstrng (ip), lens, ier )
	        IF ( ( islsh .ne. 0 .and. islsh .ne. 5 ) .or.
     +               ( islsh .eq. 0 .and. lens .ne. 4 ) ) THEN
		    nparm = 0
	    	    iret  = -11
	            CALL ER_WMSG ( 'IN', iret, pstrng (ip), ier )
	            RETURN
	        END IF          
C*
	        IF ( islsh .eq. 0 ) THEN
C
C*	            No packing is to be done.
C
	            parms (ip) = pstrng ( ip )
	            CALL ST_LCUC ( parms (ip), parms (ip), ier )
	        ELSE
C
C*	            Get the packing information.
C
	            ipack = ipack + 1
	            pkflg = .true.
	            IF ( pstrng (ip) ( lens:lens ) .eq. '/' )
     +			 pstrng (ip) ( lens:lens ) = ' '
	            CALL ST_RMBL ( pstrng (ip), pstrng (ip), lens, ier )
	            CALL ST_LCUC ( pstrng (ip), pstrng (ip), ier )
		    parms (ip) = pstrng (ip) ( 1:islsh-1 )
C
C*	            Replace '-' delimiter with a blank with
C*                  consideration of the double minus problem.
C
		    ic = islsh + 2
	            iccnt = 0
	            DO WHILE ( ic .le. lens )
	                IF ( pstrng (ip) (ic:ic) .eq. '-' ) THEN
	                    pstrng (ip) (ic:ic) = ' '
	                    ic = ic + 2
	                    iccnt = iccnt + 1
	                ELSE
	                    ic = ic + 1
	                END IF
	            END DO
	            IF ( iccnt .gt. 2 ) THEN
	                iret = -9
	                CALL ER_WMSG ( 'IN', iret, ' ', ier )
	                RETURN
	            END IF
	   	    CALL ST_RLST ( pstrng (ip) ( islsh+1:lens ), ' ',
     +                             RMISSD, 3, rval, nr, ier )
		    CALL DP_TERM  ( rval (1), rval (2), rval (3), 
     +			   	    iscale (ip), iofset (ip),
     +                              ibits (ip), ier )
	            IF ( ier .ne. 0 ) THEN
	                iret = -9
	                CALL ER_WMSG ( 'IN', iret, ' ', ier )
	  	        RETURN
	            END IF
	        END IF
	    END DO
	    IF ( pkflg .and. ipack .ne. nparm ) THEN
	        iret = -10
	        CALL ER_WMSG ( 'IN', iret, ' ', ier )
	        RETURN
	    END IF
	END IF
	RETURN
	END
