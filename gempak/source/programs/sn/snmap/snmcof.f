	SUBROUTINE SNMCOF  ( nparm, parms, wndflg, filtfc, offset, 
     +			     iret )
C************************************************************************
C* SNMCOF								*
C*									*
C* This subroutine calculates the offsets for parameters.		*
C*									*
C* SNMCOF  ( NPARM, PARMS, WNDFLG, FILTFC, OFFSET, IRET )		*
C*									*
C* Input parameters:							*
C*	NPARM		INTEGER		Number of parameters to plot	*
C*	PARMS (NPARM)	CHAR*		Parameter names			*
C*	WNDFLG		LOGICAL		Wind flag			*
C*	FILTFC		REAL		Filter factor			*
C*									*
C* Output parameters:							*
C*	OFFSET  (*)	REAL		L,R,T,B offsets	in normlized	*
C*					coordinates			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 7/87	GEMPAK					*
C* M. desJardins/GSFC	 1/86	Assume 2.7 chars/data rather than 4	*
C* M. desJardins/GSFC	 6/88	Reorganized				*
C* S. Schotz/GSC	 8/90	Removed number of digits, reorganized	*
C8				like SFMCOF				*
C* J. Nielsen/TAMU	11/91	Check for wind barbs when params exist	*
C*				Added filter factor			*
C* K. Brill/NMC		12/91	Changes for new wind parms		*
C* S. Maxwell/GSC	 3/97	Removed marker color                  	* 
C* A. Hardy/GSC          4/98   Adjusted for 12 postion arrays          *
C************************************************************************
	CHARACTER*(*)	parms  (*)
	REAL		filtfc, offset  (*)
	LOGICAL		wndflg
C*
	REAL 		psmull (12), psmulr (12), psmulu (12) 
	REAL		psmuld (12), apos (12), bpos  (12), cpos (12)
	REAL		dpos (12), offxy(4)
C*
	DATA 		psmull /.5, 1., 1., 0., 0., 1., 0., .5, .5, 
     +                         .5, .5, .5/
	DATA 		psmulr /.5, 0., 0., 1., 1., 0., 1., .5, .5, 
     +                          .5, .5, .5/
	DATA 		psmulu /.5, 1.5, .5, 1.5, .5, 0., 0., 2.5, 0., 
     +	                        1.5, 0, .5/
	DATA 		psmuld /.5, 0., .5, 0., .5, 1.5, 1.5, 0., 2.5,
     +	                         0, 1.5, .5/
C------------------------------------------------------------------------
	iret = 0
C
C*	Initialize position arrays.
C
	DO  i = 1, 12
	    apos (i) = 0
	    bpos (i) = 0
	    cpos (i) = 0
            dpos (i) = 0
	END DO
C
C*	Determine character and barb size in N coords.
C
	CALL GQSYSZ  ( rwmrk, rhmrk, rwtxt, rhtxt, rwbrb, rhbrb, iret )
C
C*	Calculate multipliers for each parameter position.
C
	k = 0
	DO  i = 1, nparm
            IF  ( parms (i) .ne. 'BLNK' )  THEN
	      	k = k + 1
                rimul =  2.7	
	        apos (k) = psmull (i) * rimul
	        bpos (k) = psmulr (i) * rimul
	        cpos (k) = psmulu (i) 
	        dpos (k) = psmuld (i) 
	    END IF
	END DO
C
C*	Get maximum as offset.
C
	offxy (1) = MAX ( apos (1), apos (2), apos (3), apos (4), 
     +		          apos (5), apos (6), apos (7), apos (8),
     +			  apos (9), apos (10), apos(11), apos(12) )
C
	offxy (2) = MAX ( bpos (1), bpos (2), bpos (3), bpos (4), 
     +			  bpos (5), bpos (6), bpos (7), bpos (8),
     +			  bpos (9), bpos (10), bpos(11), bpos(12) )
C
	offxy (3) = MAX ( cpos (1), cpos (2), cpos (3), cpos (4), 
     + 		          cpos (5), cpos (6), cpos (7), cpos (8),
     +			  cpos (9), cpos (10), cpos (11), cpos(12) )
C
	offxy (4) = MAX ( dpos (1), dpos (2), dpos (3), dpos (4), 
     +		          dpos (5), dpos (6), dpos (7), dpos (8),
     +			  dpos (9), dpos (10), dpos(11), dpos(12) )
C
C*	Multiply character sizes by offsets to get offsets in N coords.
C
	offset (1) = offxy (1) * rwtxt * filtfc
	offset (2) = offxy (2) * rwtxt * filtfc
	offset (3) = offxy (3) * rhtxt * filtfc
	offset (4) = offxy (4) * rhtxt * filtfc
	IF ( wndflg )  THEN
	    DO  i = 1, 4
	    	offset(i) = MAX ( offset(i), rwbrb * 0.5 * filtfc )
	    END DO
	END IF
C*
	RETURN
	END
