       SUBROUTINE GH_WWCV ( ibegus, iendus, clist, zlist, ivtec, bkpstr,
     +			    iret )
C************************************************************************
C* GH_WWCV                                                              *
C*                                                                      *
C* This subroutine looks for IUSGEC or IKEYS segments with the same     *
C* VTEC code(s) which can be combined into a single segment.  The       *
C* segments can be adjacent, overlapping, or wholly contained.		*
C*                                                                      *
C* GH_WWCV ( IBEGUS, IENDUS, CLIST, ZLIST, IVTEC, BKPSTR, IRET )   	*
C*                                                                      *
C* Input parameters:                                                    *
C*      IBEGUS		INTEGER	 Beginning segment index for area       *
C*	IENDUS		INTEGER  Ending segment index for area          *
C*                                                                      *
C* Input and output parameters:                                         *
C*      CLIST(NSEG)	CHAR*	 Lists of county UGCs by segment	*
C*      ZLIST(NSEG)	CHAR*	 Lists of marine zone UGCs by segment	*
C*      IVTEC(3,NSEG)	INTEGER	 VTEC action and watch/warning code 	*
C*				 values by segment			*
C*      BKPSTR(NSEG)	CHAR*	 Breakpoint text strings by segment	*
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C**                                                                     *
C* Log:                                                                 *
C* D. Kidwell/NCEP	 9/05	                                        *
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
	INCLUDE		'ghcmn.cmn'
C*
	PARAMETER	( MAXSEG = 50, MAXSG2 = 2 * MAXSEG )
C*
	CHARACTER*(*) 	clist (*), zlist (*), bkpstr (*)
	INTEGER		ivtec (3,*)
C*
	INTEGER 	isev (MAXSEG), iadvtp (MAXSEG), numbkp (MAXSEG),
     +			ibkpts (4,MAXSG2), numb (4), iarea (4,MAXSG2),
     +			icnt (4,MAXSEG), mtch (MAXSEG)
        CHARACTER	bpnams (MAXSG2)*40, bpnam (2,MAXSEG)*55
	LOGICAL		VMATCH, check (MAXSEG), more
C*
	VMATCH ( i1, j1, i2, j2, i3, j3 ) = ( ( i1 .eq. j1 ) .and.
     +					      ( i2 .eq. j2 ) .and.
     +					      ( i3 .eq. j3 ) ) 
C-----------------------------------------------------------------------
        iret  = 0
C
C*	Hardwire values for the later call to GH_BKGB to use only the 
C*	first array entry, for breakpoint pairs.
C
	DO ii = 1, MAXSEG
	    isev ( ii ) = 1
	    iadvtp ( ii ) = 1
	    numbkp ( ii ) = 2	
	END DO
C
	DO ii = ibegus, iendus
	    check ( ii ) = .true.
C
C*	    Check whether the segment has already been eliminated.
C
	    IF ( ivtec ( 1, ii ) .eq. 0 ) check ( ii ) = .false.
   	END DO
C
C*	Loop over segments with non-zero VTEC values.
C
	DO ii = ibegus, iendus - 1
	    IF ( check ( ii ) ) THEN
		icount = 1
		mtch ( 1 ) = ii
		iv11 = ivtec ( 1, ii ) 
		iv12 = ivtec ( 2, ii ) 
		iv13 = ivtec ( 3, ii )
		DO jj = ii + 1, iendus
		    IF ( check ( jj ) ) THEN
		        iv21 = ivtec ( 1, jj ) 
		        iv22 = ivtec ( 2, jj ) 
		        iv23 = ivtec ( 3, jj )
		        IF ( VMATCH ( iv11,iv21,iv12,iv22,iv13,iv23 )
     +				   .or.  
     +		       	     VMATCH ( iv11,iv21,iv12,iv23,iv13,iv22 )
     +				   .or.  
     +		       	     VMATCH ( iv11,iv22,iv12,iv21,iv13,iv23 )
     +				   .or.  
     +		             VMATCH ( iv11,iv22,iv12,iv23,iv13,iv21 )
     +				   .or.  
     +		       	     VMATCH ( iv11,iv23,iv12,iv21,iv13,iv22 )
     +				   .or.  
     +		       	     VMATCH ( iv11,iv23,iv12,iv22,iv13,iv21 ) )
     +				   THEN
C
C*			    A match has been found on the VTEC values.
C
		            icount = icount + 1
			    mtch ( icount ) = jj
		        END IF
		    END IF
		END DO
C
		IF ( icount .gt. 1 ) THEN
C
C*		    Look for segments which can be combined.
C
		    DO jj = 1, icount
			mdx = mtch ( jj )	    
			check ( mdx ) = .false.
			mm = ( jj - 1 ) * 2		    
C
C*			Get the breakpoint names to search the table.
C
			CALL ST_LSTR ( bkpstr ( mdx ), lenbp, ier )
			isemi = index ( bkpstr ( mdx ), ';' )
			bpnam ( 1, mdx ) = bkpstr ( mdx ) ( 1:isemi-1 )
			bpnam ( 2, mdx ) = 
     +			               bkpstr ( mdx ) ( isemi+1:lenbp )
			DO ll = 1, 2
			    lenbp = INDEX ( bpnam ( ll, mdx ), ' ' ) - 1
			    IF ( bpnam ( ll, mdx ) ( lenbp-2:lenbp-2 )
     +				         .eq.  '+' ) lenbp = lenbp - 3
			    bpnams ( mm + ll ) = 
     +					 bpnam ( ll, mdx ) ( :lenbp )
			END DO
		    END DO	
	
C
C*		    Get the breakpoint numbers from the names.
C
		    CALL GH_BKGB ( icount, isev, iadvtp, bpnams, numbkp,
     +		       		   ibkpts, numb, iarea, icnt, ier )
C
		    DO jj = 1, icount - 1
			mdx1 = mtch ( jj )
			jpt  = jj * 2
			ib1  = ibkpts ( 1, jpt - 1 )
			ie1  = ibkpts ( 1, jpt )
			more = .true.
			kk   = jj + 1
			DO WHILE ( more )
			    mdx2 = mtch ( kk )
			    kpt  = kk * 2
			    ib2  = ibkpts ( 1, kpt - 1 )
			    ie2  = ibkpts ( 1, kpt )
			    IF ( ( ie1 .lt. ib2 ) .or.
     +				 ( ie2 .lt. ib1 ) ) THEN
C
C*				There is no overlap for these segments.
C
			      ELSE
C
C*				The segments are adjacent or overlap.
C
				more = .false.
				ibkpts ( 1, kpt - 1 ) = MIN0 ( ib1, ib2)
				ibkpts ( 1, kpt )     = MAX0 ( ie1, ie2)
				DO ll = 1, 3 
				    ivtec ( ll, mdx1 ) = 0
				END DO
C
C*				Combine the UGC lists.
C
                                CALL ST_LSTR ( clist ( mdx1 ), l1, ier )
                                CALL ST_LSTR ( clist ( mdx2 ), l2, ier )
                                IF ( ( l1 .gt. 0 ) .and. ( l2 .gt. 0 )) 
     +				       THEN
                                    clist ( mdx2 ) = clist ( mdx1 )(:l1)
     +                                     // ';' // clist ( mdx2 )(:l2)
                                  ELSE IF ( l1 .gt. 0 ) THEN
                                    clist ( mdx2 ) = clist ( mdx1 )(:l1)
                                END IF
C
                                CALL ST_LSTR ( zlist ( mdx1 ), l1, ier )
                                CALL ST_LSTR ( zlist ( mdx2 ), l2, ier )
                                IF ( ( l1 .gt. 0 ) .and. ( l2 .gt. 0 ))
     +				       THEN
                                    zlist ( mdx2 ) = zlist ( mdx1 )(:l1)
     +                                     // ';' // zlist ( mdx2 )(:l2)
                                  ELSE IF ( l1 .gt. 0 ) THEN
                                    zlist ( mdx2 ) = zlist ( mdx1 )(:l1)
                                END IF
C
C*				Combine the breakpoint strings.
C
				IF ( ib1 .lt. ib2 ) THEN
				    mdxb = mdx1
				  ELSE
				    mdxb = mdx2
				END IF
				IF ( ie1 .le. ie2 ) THEN
				    mdxe = mdx2
				  ELSE
				    mdxe = mdx1
				END IF
				bpnam ( 1, mdx2 ) = bpnam ( 1, mdxb )
				bpnam ( 2, mdx2 ) = bpnam ( 2, mdxe )
                                CALL ST_LSTR ( bpnam ( 1, mdxb ), l1,
     +					       ier )
                                CALL ST_LSTR ( bpnam ( 2, mdxe ), l2, 
     +					       ier )
                                bkpstr ( mdx2 ) = bpnam( 1, mdxb ) (:l1)
     +                                  // ';' // bpnam( 2, mdxe ) (:l2)
			    END IF
			    kk = kk + 1
			    IF ( kk .gt. icount ) more = .false.
			END DO
		    END DO
		END IF
	    END IF
	END DO
C
C*	Format the breakpoint strings using '-' as the word separator.
C      
	DO ii = ibegus, iendus
	    IF ( ivtec ( 1, ii ) .gt. 0 ) THEN
		CALL ST_LSTR ( bkpstr ( ii ), l1, ier )
		ipos = 100
		DO WHILE ( ipos .ne. 0 ) 
		    CALL ST_RPST ( bkpstr ( ii ) ( :l1 ) , '_', '-', 
     +				   ipos, bkpstr ( ii ) , ier )
		END DO
		ipos = 100
		DO WHILE ( ipos .ne. 0 ) 
		    CALL ST_RPST ( bkpstr ( ii ) ( :l1 ) , '+', '-', 
     +				   ipos, bkpstr ( ii ) , ier )
		END DO
	    END IF
	END DO
C*
	RETURN
	END
