	SUBROUTINE SNLLEV  ( isnfln, levels, vcoord, ivert, nlev,
     +			     rlevel, levtyp, voutc, lvert, nparts, 
     +			     iret )
C************************************************************************
C* SNLLEV								*
C*									*
C* This subroutine returns a list of levels or the limits of a range.	*
C* The vertical coordinate type is also converted into an integer code. *
C*									*
C* SNLLEV ( ISNFLN, LEVELS, VCOORD, IVERT, NLEV, RLEVEL, LEVTYP, 	*
C*		VOUTC, LVERT, NPARTS, IRET )				*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	LEVELS		CHAR*		User input for LEVELS		*
C*	VCOORD		CHAR*		User input for VCOORD		*
C*	IVERT		INTEGER		Dataset coordinate system	*
C*									*
C* Output parameters:							*
C*	NLEV		INTEGER		Number of levels		*
C*	RLEVEL (NLEV)	REAL		List of levels 			*
C*	LEVTYP		INTEGER		Type of level specification	*
C*					  1 = list of levels		*
C*					  2 = range without increment	*
C*	VOUTC		CHAR*		Output vertical coordinate	*
C*	LVERT		INTEGER		Output vertical coordinate	*
C*	NPARTS		INTEGER 	Parts flag for text output      *
C*					  1 = TTAA                      *
C*					  2 = TTAA, TTCC                *
C*					  3 = TTAA, TTBB, PPBB          *
C*					  4 = TTAA, TTBB, PPBB, TTCC    *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. Goodman/RDS	11/84	Original source				*
C* M. desJardins/GSFC	 5/86	Call new LV subs; change error proc	*
C* M. desJardins/GSFC	10/86	Added GEMPAK parameter names		*
C* M. desJardins/GSFC	10/88	GEMPAK 4.1				*
C* M. desJardins/GSFC	 4/89	LV error msg; add LV_CCRD		*
C* S. Schotz/GSC	 8/90	Improve error processing		*
C* S. Jacobs/NMC	 3/95	Added SN file number and mand data flag	*
C* D. Kidwell/NCEP	 5/99	Added call to PC_MAND                   *
C* D. Kidwell/NCEP	 2/01	Added nparts to calling sequence        *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	levels, vcoord, voutc
	REAL		rlevel (*)
C*
	LOGICAL		mandat
C------------------------------------------------------------------------
	iret = 0
C
C*	Change vcoord from DSET to actual vertical coordinate.
C
	CALL ST_LCUC  ( vcoord, voutc, ier )
	IF  ( voutc .eq. 'DSET' )  CALL LV_CCRD ( ivert, voutc, ier )
C
C*	Get levels.
C
	CALL LV_INPT  ( levels, LLMXLV, voutc, nlev, rlevel, levtyp, 
     +			voutc,  lvert,  mandat, iret )
	IF  ( iret .ne. 0 )  THEN
	    IF  ( levels .ne. ' ' )  THEN
                IF  ( iret .eq. -2 ) THEN
                    CALL ER_WMSG  ( 'LV', iret, 'MAN', ier )
                ELSE
		    CALL ER_WMSG  ( 'LV', iret, levels, ier )
                END IF
	    ELSE
                iret = 0          
	        nlev = 0
            END IF
	END IF
C
C*	Set mandatory data only flag.
C
	CALL SN_MAND ( isnfln, mandat, ier )
C
C*	Set interpolation flag.
C
	CALL PC_MAND ( mandat, ier )
C
C*	Set parts flag for text output.
C
	IF ( nlev .gt. 0 ) THEN
	    IF  ( mandat ) THEN
	        IF ( ( lvert .eq. 1 ) .and. 
     +		     ( rlevel ( nlev ) .gt. 99. ) ) THEN
		    nparts = 1
		  ELSE
		    nparts = 2
	        END IF
	      ELSE IF ( ( lvert .eq. 1 ) .and. 
     +			( rlevel ( nlev ) .gt. 99. ) ) THEN
		nparts = 3
	      ELSE
		nparts = 4
	    END IF
	  ELSE
	    nparts = 0
	END IF
C*
	RETURN
	END
