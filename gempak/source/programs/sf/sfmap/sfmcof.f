	SUBROUTINE SFMCOF ( nparm, parms, wndflg, filtfc, offset, 
     +                      iret )
C************************************************************************
C* SFMCOF								*
C*									*
C* This subroutine calculates the parameter offsets for filtering.	*
C*									*
C* SFMCOF  ( NPARM, PARMS, WNDFLG, FILTFC, OFFSET, IRET )		*
C*									*
C* Input parameters:							*
C*	NPARM		INTEGER		Number of parameters to plot	*
C*	PARMS (NPARM)	CHAR*		Parameter names			*
C*	WNDFLG		LOGICAL		Flag for winds			*
C*	FILTFC		REAL		Filter factor			*
C*									*
C* Output parameters:							*
C*	OFFSET (*)	REAL		L,R,T,B offsets	in normalized	*
C*					coordinates			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 7/87	GEMPAK					*
C* M. desJardins/GSFC	 1/86	Assume 2.7 chars/data rather than 4	*
C* M. desJardins/GSFC	 6/88	Reorganized				*
C* S. Schotz/GSC	 4/90   Removed data types assignments		*
C* S. Schotz/GSC	 5/90	Reorganized and Pass back offsets 	*
C*				in normalized coords			*
C* S. Schotz/GSC	 6/90	Fixed bug for filter size for centered	*
C*				case					*
C* S. Schotz/GSC	 8/90	Removed NDIG				*
C* J. Nielsen/TAMU	11/91	Include wind barb check for offsets	*
C*				Added filter factor			*
C* K. Brill/NMC		11/91	Removed wind color number		*
C* S. Maxwell/GSC	 3/97	Removed IMKCLR				*
C* A. Hardy/GSC          4/98   Adjusted for 12 position arrays         *
C* A. Hardy/GSC          2/98   Increased offsets from 12 -> 26         *
C************************************************************************
	CHARACTER*(*)	parms  (*)
	LOGICAL		wndflg
	REAL		offset  (*)
C*
	REAL 		psmull (26), psmulr (26), psmulu (26) 
	REAL		psmuld (26), apos (26), bpos (26), cpos (26)
	REAL		dpos (26), offxy(4)
C*
	DATA 		psmull /.5, 1., 1., 0., 0., 1., 0., .5, .5, 
     +                          .5, .5, 1.5, -0.5, 1., 1., 0., 0.,
     +                         1.5, 1.5, 1.5, 1.5, -0.5, -0.5, -0.5,
     +                         -0.5, .5 /
	DATA 		psmulr /.5, 0., 0., 1., 1., 0., 1., .5, .5, 
     +                          .5, .5, -0.5, 1.5, 0., 0., 1., 1.,
     +                          -0.5, -0.5, -0.5, -0.5, 1.5, 1.5, 1.5,
     +                          1.5, .5 /
	DATA 		psmulu /.5, 1.5, .5, 1.5, .5, 0., 0., 2.5, 0., 
     +	                        1.5, 0, .5, .5, 2.5, 0., 2.5, 0., 2.5,
     +                          1.5, 0., 0., 2.5, 1.5, 0., 0., .5 /
	DATA 		psmuld /.5, 0., .5, 0., .5, 1.5, 1.5, 0., 2.5,
     +	                         0, 1.5, .5, .5, 0., 2.5, 0., 2.5, 0., 
     +                           0., 1.5, 2.5, 0., 0., 1.5, 2.5, .5/
C------------------------------------------------------------------------
	iret = 0
C
C*	Initialize position arrays.
C
	DO  i = 1, 26
	    apos (i) = 0
	    bpos (i) = 0
	    cpos (i) = 0
            dpos (i) = 0
	END DO

C
C*	Check to see if wind barbs are to be
C
C*	Determine character and barb size in N coords.
C
	CALL GQSYSZ  ( rwmrk, rhmrk, rwtxt, rhtxt, rwbrb, rhbrb, iret )
C
C*      Calculate multipliers for each parameter position.
C
	k = 0
	DO  i = 1, nparm
	    IF  ( parms (i) .ne. 'BLNK' )  THEN
	         k = k + 1
                 rimul = 2.7	
	         apos (k) = psmull (i) * rimul
	         bpos (k) = psmulr (i) * rimul
	         cpos (k) = psmulu (i) 
	         dpos (k) = psmuld (i) 
	    END IF
	END DO
C
C*	Get maximum as offset.

        offxy (1) = MAX ( apos (1), apos (2), apos (3), apos (4), 
     +	 	          apos (5), apos (6), apos (7), apos (8),
     +			  apos (9), apos (10), apos(11), apos(12), 
     +			  apos (13), apos (14), apos(15), apos(16),
     +			  apos (17), apos (18), apos(19), apos(20), 
     +			  apos (21), apos (22), apos(23), apos(24),
     +			  apos (25), apos (26) )
C
	offxy (2) = MAX ( bpos (1), bpos (2), bpos (3), bpos (4), 
     +			  bpos (5), bpos (6), bpos (7), bpos (8),
     +			  bpos (9), bpos (10), bpos(11), bpos(12),
     +			  bpos (13), bpos (14), bpos(15), bpos(16),
     +			  bpos (17), bpos (18), bpos(19), bpos(20), 
     +			  bpos (21), bpos (22), bpos(23), bpos(24),
     +			  bpos (25), bpos (26) )

        offxy (3) = MAX ( cpos (1), cpos (2), cpos (3), cpos (4), 
     +	  		  cpos (5), cpos (6), cpos (7), cpos (8),
     +			  cpos (9), cpos (10), cpos (11), cpos(12),
     +			  cpos (13), cpos (14), cpos(15), cpos(16),
     +			  cpos (17), cpos (18), cpos(19), cpos(20), 
     +			  cpos (21), cpos (22), cpos(23), cpos(24),
     +			  cpos (25), cpos (26) )
C
	offxy (4) = MAX ( dpos (1), dpos (2), dpos (3), dpos (4), 
     +			  dpos (5), dpos (6), dpos (7), dpos (8),
     +			  dpos (9), dpos (10), dpos(11), dpos(12),
     +			  dpos (13), dpos (14), dpos(15), dpos(16),
     +			  dpos (17), dpos (18), dpos(19), dpos(20), 
     +			  dpos (21), dpos (22), dpos(23), dpos(24),
     +			  dpos (25), dpos (26) )
C
C       Multiply character sizes by offsets to get offsets in N coords.
C
	offset (1) = offxy (1) * rwtxt * filtfc
	offset (2) = offxy (2) * rwtxt * filtfc
	offset (3) = offxy (3) * rhtxt * filtfc
	offset (4) = offxy (4) * rhtxt * filtfc

C       Make sure wind barbs aren't actually bigger than that.
C
        IF ( wndflg )  THEN
	   DO i = 1, 4
	        offset (i) = MAX ( offset(i), rwbrb*0.5*filtfc )
	   END DO
	END IF
C*
	RETURN
	END
