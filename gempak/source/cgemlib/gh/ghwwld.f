	SUBROUTINE GH_WWLD ( ibeg, iend, ibegwt, iendwt, lpri, ivtec, 
     +			     clist, zlist, iret )
C************************************************************************
C* GH_WWLD                                                              *
C*									*
C* This subroutine compares VTEC codes for two geographic areas, and    *
C* eliminates redundant or conflicting counties and zones.  It also     *
C* performs a check for combining segments with identical UGCs.         *
C*                                                                      *
C* GH_WWLD ( IBEG, IEND, IBEGWT, IENDWT, LPRI, IVTEC, CLIST, ZLIST,     *
C*	     IRET )   						        *
C*           								*
C* Input parameters:                                                    *
C*	IBEG		INTEGER		Beginning seg. index - 1st area *
C*	IEND      	INTEGER		Ending segment index - 1st area *
C*	IBEGWT   	INTEGER		Beginning seg. index - 2nd area *
C*	IENDWT		INTEGER		Ending segment index - 2nd area *
C*	LPRI		LOGICAL		Priority flag - land over water *
C*									*
C* Input and output parameters:                                         *
C*	IVTEC(3,*)	INTEGER		VTEC action & event code values *
C*	CLIST(*)	CHAR*		County UGCs by segment          *
C*	ZLIST(*)	CHAR*		Marine zone UGCs by segment     *
C*									*
C* Output parameters:                                                   *
C*	IRET		INTEGER		Return code                     *
C*									*
C* Log:									*
C* D. Kidwell/NCEP	 2/05	                  			*
C* D. Kidwell/NCEP	 2/05	Added more checks			*
C* D. Kidwell/NCEP	 4/05	Checked for empty list; mod. prologue   *
C* D. Kidwell/NCEP	 9/05	Added check on 1st area = 2nd area      *
C* D. Kidwell/NCEP	12/05	CSC for lpri, added pri chks, restruct. *
C* M. Li/SAIC            2/06   Changed county codes to zone            *
C************************************************************************
	CHARACTER*(*)	clist (*), zlist (*)
     	INTEGER		ivtec (3,*)
	LOGICAL		lpri
C*
	CHARACTER	ugc*6, drop*6
	CHARACTER       tag*25, dirsym*160, tblnam*72, value*12, cc
	INTEGER		ivtc1 (19), ivtc2 (19), nwvtec (3), jvtec (6)
	LOGICAL 	done, match, county, VPRI, VMATCH
C*	
	DATA	drop	/ 'XXXXXX' /
	DATA	ivtc1	/  2,  2,  6,  6,  6, 10, 10,  1,  1,  1,  2,
     +			   2,  2,  3,  3,  3,  4,  4,  4 /
	DATA	ivtc2	/  3,  7,  3,  7, 11,  7, 11, 10, 11, 12,  9,
     +			  11, 12,  9, 10, 12,  9, 10, 11 /
C*
	VPRI ( iv1, iv2, iv3, icode ) = ( ( iv1 .eq. icode ) .or.
     +				          ( iv2 .eq. icode ) .or.
     +				          ( iv3 .eq. icode ) ) 
        VMATCH ( i1, j1, i2, j2, i3, j3 ) = ( ( i1 .eq. j1 ) .and.
     +                                        ( i2 .eq. j2 ) .and.
     +                                        ( i3 .eq. j3 ) )
C------------------------------------------------------------------------
	iret = 0
        value = ' '
C
C*      Read the value of tag TCV_ZONE in prefs.tbl.
C
        tag     = 'TCV_ZONE'
        tblnam  = 'prefs.tbl'
        dirsym  = 'config'
C
        CALL ST_NULL ( tblnam, tblnam, lens, ier )
        CALL ST_NULL ( dirsym, dirsym, lens, ier )
        CALL ST_NULL ( tag, tag, lens, ier)
        CALL CTB_RDPRF ( tblnam, dirsym, tag, value, ier1 )
        CALL ST_RNUL ( value, value, lens, ier )
C
	IF ( value .eq. 'COUNTY' ) THEN
            cc = 'C'   	 
          ELSE
	    cc = 'Z'
        END IF
C
C*	Look for matching VTEC codes and matching UGCs across segments
C*      in different geographic areas, or for conflicting VTEC codes and
C*	UGCs across segments in different geographic areas.  If found,
C*      drop the UGC from one segment.
C
	IF ( ( ibeg .eq. ibegwt ) .and. ( iend .eq. iendwt ) ) THEN
C
C*	    This is a special case - check a geographic area only 
C*	    against itself.
C
	    istart = 3
	    istop  = 1
	  ELSE
C
C*	    Check the areas against each other, and check each area
C*	    against itself.
C
	    istart = 1
	    istop  = 2
	END IF
C
	DO nn = istart, 3
	  IF ( nn .eq. 1 ) THEN
	    nbeg = ibeg
	    nend = iend
	    nbegwt = ibegwt
	    nendwt = iendwt
	   ELSE IF ( nn .eq. 2 ) THEN
	    nbeg = ibegwt
	    nend = iendwt
	    nbegwt = ibegwt
	    nendwt = iendwt
	   ELSE IF ( nn .eq. 3 ) THEN
	    nbeg = ibeg
	    nend = iend
	    nbegwt = ibeg
	    nendwt = iend
	  END IF
C
	  DO ii = nbeg, nend
	    county = .true.
	    DO icnmz = 1, 2
	      IF ( icnmz .eq. 2 ) county = .false.
C
C*	      Check the counties, then the marine zones.  ST_CLST cannot
C*	      be used because the list string may be too long.
C
	      done = .false.
 	      nocc = 1
	      last = 1
	      IF ( county ) THEN
	        CALL ST_LSTR ( clist ( ii ), lencz, ier )
	       ELSE
	        CALL ST_LSTR ( zlist ( ii ), lencz, ier )
	      END IF
	      IF ( lencz .lt. 6 ) done = .true.
	      DO WHILE ( .not. done )
		IF ( county ) THEN
		    CALL ST_NOCC ( clist ( ii ) ( :lencz ), ';', nocc,
     +		    		   isemi, ier )
		  ELSE
		    CALL ST_NOCC ( zlist ( ii ) ( :lencz ), ';', nocc,
     +		    		   isemi, ier )
		END IF
		IF ( isemi .eq. 0 ) THEN
		    isemi = lencz + 1
		    done  = .true.
		END IF
		IF ( county ) THEN
		    ugc  = clist ( ii ) ( last:isemi - 1 )
		  ELSE
		    ugc  = zlist ( ii ) ( last:isemi - 1 )
		END IF
		last = isemi + 1
		nocc = nocc + 1
C
		DO jj = nbegwt, nendwt
		  IF ( ( nn .eq. 1 ) .or. ( ( nn .gt. 1 ) .and.
     +		       ( ii .ne. jj ) ) ) THEN
		    IF ( county ) THEN
		        locc = INDEX ( clist ( jj ), ugc )
		      ELSE
		        locc = INDEX ( zlist ( jj ), ugc )
		    END IF 
		    IF ( ( locc .ne. 0 ) .and. ( ugc .ne. drop ) ) THEN
C
C*			A match on the county or marine zone UGC was 
C*			found.  Check the VTECs.
C
			match = .false.
			i1 = ivtec ( 1, ii ) 		
			i2 = ivtec ( 2, ii ) 		
			i3 = ivtec ( 3, ii ) 		
			j1 = ivtec ( 1, jj ) 		
			j2 = ivtec ( 2, jj ) 		
			j3 = ivtec ( 3, jj ) 		
                        IF ( VMATCH ( i1, j1, i2, j2, i3, j3 )
     +                             .or.
     +                       VMATCH ( i1, j1, i2, j3, i3, j2 )
     +                             .or.
     +                       VMATCH ( i1, j2, i2, j1, i3, j3 )
     +                             .or.
     +                       VMATCH ( i1, j2, i2, j3, i3, j1 )
     +                             .or.
     +                       VMATCH ( i1, j3, i2, j1, i3, j2 )
     +                             .or.
     +                       VMATCH ( i1, j3, i2, j2, i3, j1 ) )
     +                             THEN
C
C*			    The UGCs match and all the VTEC codes 
C*			    match.  Drop the UGC from the 2nd
C*			    segment, by replacing it with 
C*			    'XXXXXX' to maintain position.
C
			    IF ( county ) THEN
		                CALL ST_RPST ( clist (jj), ugc, drop,
     +					       ipos, clist (jj), ier )
			      ELSE
		                CALL ST_RPST ( zlist (jj), ugc, drop,
     +					       ipos, zlist (jj), ier )
			    END IF
			    match = .true.
			END IF
C
			IF ( .not. match ) THEN
			    ivcnt = 0
			    DO kk = 1, 3
			        IF ( ivtec ( kk, ii ) .gt. 0 ) 
     +				     ivcnt = ivcnt + 1
			        IF ( ivtec ( kk, jj ) .gt. 0 )
     +				     ivcnt = ivcnt + 1
			    END DO
			    IF ( ( ivcnt .eq. 2 ) .or.
     +				 ( ivcnt .eq. 3 ) ) THEN
C
C*			        Check for allowable pair combinations of
C*			        VTEC codes.
C
			        imatch = 0
				DO kk = 1, 3
			          DO mm = 1, 19
				    IF ( ivtec ( kk, ii ) .eq.
     +				         ivtc1 ( mm ) ) THEN
				       DO ll = 1, 3
					 IF ( ivtec ( ll, jj ) .eq.
     +					      ivtc2 ( mm ) ) THEN
					     imatch = imatch + 1
					 END IF
				       END DO
				    END IF
				    IF ( ivtec ( kk, jj ) .eq.
     +				         ivtc1 ( mm ) ) THEN
				       DO ll = 1, 3
					 IF ( ivtec ( ll, ii ) .eq.
     +					      ivtc2 ( mm ) ) THEN
					     imatch = imatch + 1
					  END IF
				       END DO
				    END IF
			          END DO
				END DO
			        IF ( imatch .eq. ( ivcnt - 1 ) ) 
     +				     match = .true.
			    END IF
			END IF
C
			IF ( .not. match ) THEN 
C
C*			    The VTECs conflict.  Remove the one with the
C*			    lower priority.  (The numeric VTEC codes are
C*			    documented in GH_WWSG and GH_WWLG.)
C*			    Highest to lowest event priority is
C*			    HU.W - 1, 5, 9
C* 			    HU.A/TR.W - 2, 6, 10 / 3, 7, 11
C*			    TR.A - 4, 8, 12
C*			    Highest to lowest action priority is
C*			    CON - 5 through 8
C*			    NEW - 1 through 4
C*			    CAN - 9 through 12
C*			    Check event first, then action within event.
C*			    Exception - if lpri is true, use the coastal
C*			    (first) VTEC for the counties.  This ensures
C*			    that the coastal watch/warning will take
C*			    precedence over the water one for land.
C
			    ij = jj
			    IF ( county .and. lpri .and. ( nn .eq. 1 ) )
     +				 THEN
C*				Do nothing. ij retains the value jj.
			      ELSE
			        IF ( VPRI ( i1, i2, i3, 5 ) ) THEN
C*				    Do nothing. ij retains the value jj.
			          ELSE IF ( VPRI ( j1, j2, j3, 5 ) )
     +					    THEN
				    ij = ii
			          ELSE IF ( VPRI ( i1, i2, i3, 1 ) )
     +					    THEN
C*				    Do nothing. ij retains the value jj.
			          ELSE IF ( VPRI ( j1, j2, j3, 1 ) )
     +					    THEN
				    ij = ii
			          ELSE IF ( VPRI ( i1, i2, i3, 9 ) )
     +					    THEN
C*				    Do nothing. ij retains the value jj.
			          ELSE IF ( VPRI ( j1, j2, j3, 9 ) )
     +					    THEN
				    ij = ii
			          ELSE IF ( VPRI ( i1, i2, i3, 6 ) 
     +				       .or. VPRI ( i1, i2, i3, 7 ) )
     +					    THEN
C*				    Do nothing. ij retains the value jj.
			          ELSE IF ( VPRI ( j1, j2, j3, 6 ) 
     +				       .or. VPRI ( j1, j2, j3, 7 ) )
     +					    THEN
				    ij = ii
			          ELSE IF ( VPRI ( i1, i2, i3, 2 ) 
     +				       .or. VPRI ( i1, i2, i3, 3 ) )
     +					    THEN
C*				    Do nothing. ij retains the value jj.
			          ELSE IF ( VPRI ( j1, j2, j3, 2 ) 
     +				       .or. VPRI ( j1, j2, j3, 3 ) )
     +					    THEN
				    ij = ii
			          ELSE IF ( VPRI ( i1, i2, i3, 10 ) 
     +				       .or. VPRI ( i1, i2, i3, 11 ) )
     +					    THEN
C*				    Do nothing. ij retains the value jj.
			          ELSE IF ( VPRI ( j1, j2, j3, 10 ) 
     +				       .or. VPRI ( j1, j2, j3, 11 ) )
     +					    THEN
				    ij = ii
			          ELSE IF ( VPRI ( i1, i2, i3, 8 ) )
     +					    THEN
C*				    Do nothing. ij retains the value jj.
			          ELSE IF ( VPRI ( j1, j2, j3, 8 ) )
     +					    THEN
				    ij = ii
			          ELSE IF ( VPRI ( i1, i2, i3, 4 ) )
     +					    THEN
C*				    Do nothing. ij retains the value jj.
			          ELSE IF ( VPRI ( j1, j2, j3, 4 ) )
     +					    THEN
				    ij = ii
			        END IF
			    END IF
C
C*			    Drop the UGC from the lower priority list.
C
			    IF ( county ) THEN
		                CALL ST_RPST ( clist (ij), ugc, drop,
     +					       ipos, clist (ij), ier )
			      ELSE
		                CALL ST_RPST ( zlist (ij), ugc, drop,
     +					       ipos, zlist (ij), ier )
			    END IF
			END IF
		    END IF
		  END IF
		END DO
	      END DO
	    END DO
	  END DO
C
C*	  Remove the 'XXXXXX' strings and eliminate any empty sequences 
C*	  from the UGC lists. 
C
	  IF ( nn .eq. 1 ) THEN
	    ipass = 2
	   ELSE
	    ipass = 1
	  END IF
	  nnbeg = nbeg
	  nnend = nend
	  DO nnn = 1, ipass
	    IF ( nnn .eq. 2 ) THEN
		nnbeg = nbegwt
		nnend = nendwt
	    END IF
C
	    DO jj = nnbeg, nnend
	        done = .false.
      	        DO WHILE ( .not. done )
	            CALL ST_RPST ( clist ( jj ), drop, ';', ipos, 
     +			           clist ( jj ), ier )
		    IF ( ipos .eq. 0 ) done = .true.
	        END DO
	        done = .false.
      	        DO WHILE ( .not. done )
	            CALL ST_RPST ( clist ( jj ), ';;', ';', ipos, 
     +			           clist ( jj ), ier )
		    IF ( ipos .eq. 0 ) done = .true.
	        END DO
	        CALL ST_LSTR ( clist ( jj ), lenc, ier )
	        IF ( clist ( jj ) ( 1:1 ) .eq. ';' ) THEN
		    IF ( lenc .gt. 1 ) THEN
     	                clist ( jj ) = clist ( jj ) ( 2:lenc )
		    END IF
		    lenc = lenc - 1
	        END IF
	        IF ( lenc .gt. 0 ) THEN
     	            IF ( clist ( jj ) ( lenc:lenc ) .eq. ';' )
     +	                 clist ( jj ) ( lenc:lenc )= ' '
	        END IF
C		 
	        done = .false.
      	        DO WHILE ( .not. done )
	            CALL ST_RPST ( zlist ( jj ), drop, ';', ipos, 
     +			           zlist ( jj ), ier )
		    IF ( ipos .eq. 0 ) done = .true.
	        END DO
	        done = .false.
      	        DO WHILE ( .not. done )
	            CALL ST_RPST ( zlist ( jj ), ';;', ';', ipos, 
     +			           zlist ( jj ), ier )
		    IF ( ipos .eq. 0 ) done = .true.
	        END DO
	        CALL ST_LSTR ( zlist ( jj ), lenz, ier )
	        IF ( zlist ( jj ) ( 1:1 ) .eq. ';' ) THEN
		    IF ( lenz .gt. 1 ) THEN
     	                zlist ( jj ) = zlist ( jj ) ( 2:lenz )
		    END IF
		    lenz = lenz - 1
	        END IF
	        IF ( lenz .gt. 0 ) THEN
	            IF ( zlist ( jj ) ( lenz:lenz) .eq. ';' )
     +	                 zlist ( jj ) ( lenz:lenz )= ' '
	        END IF
C
C*	        If an  empty list results, zero out the VTEC array for
C*	        the segment.
C
	        IF ( ( lenc .lt. 6 ) .and. ( lenz .lt. 6 ) ) THEN
		    DO ll = 1, 3
		        ivtec ( ll, jj ) = 0
		        clist ( jj ) = ' '
		        zlist ( jj ) = ' ' 
		    END DO
	        END IF
	    END DO
	  END DO
	END DO
C
C*	Check for UGC lists which now are identical and combine the VTEC
C*	codes.
C
	DO nn = 1, istop
	    IF ( nn .eq. 1 ) THEN
	        nbeg = ibeg
	        nend = iend
	      ELSE
	        nbeg = ibegwt
	        nend = iendwt
	    END IF
	    DO jj = nbeg, nend - 1
	        CALL ST_LSTR ( clist ( jj ), lenc, ier )
	        CALL ST_LSTR ( zlist ( jj ), lenz, ier )
	        DO kk = jj + 1, nend
	            CALL ST_LSTR ( clist ( kk ), lenc2, ier )
	            CALL ST_LSTR ( zlist ( kk ), lenz2, ier )
		    IF ( ( clist (jj)(:lenc) .eq. clist (kk)(:lenc2) )
     +		           .and.
     +	   	         ( zlist (jj)(:lenz) .eq. zlist (kk)(:lenz2) ) )
     +		           THEN
		        icnt = 0
		        DO ll = 1, 3
			    nwvtec ( ll ) = 0
			    icnt = icnt + 1
			    jvtec ( icnt ) = ivtec ( ll, jj )
			    icnt = icnt + 1
			    jvtec ( icnt ) = ivtec ( ll, kk )
		        END DO
		        icnt = 0
		        DO ll = 1, 6
			    IF ( jvtec ( ll ) .ne. 0 ) THEN
			        match = .false.
			        DO mm = 1, icnt
				    IF ( jvtec ( ll ) .eq. 
     +				         nwvtec ( mm ) ) THEN 
				        match = .true.
				    END IF
			        END DO
			        IF ( .not. match .and. icnt .lt. 3) THEN
				    icnt = icnt + 1
				    nwvtec ( icnt ) = jvtec ( ll )
			        END IF
			    END IF
		        END DO
		        DO ll = 1, 3
			    ivtec ( ll, jj ) = nwvtec ( ll ) 
			    ivtec ( ll, kk ) = 0
		        END DO 
		    END IF
	        END DO
	    END DO
	END DO
C
C*	Make a final check for empty UGC lists.
C
	DO nn = 1, istop
	    IF ( nn .eq. 1 ) THEN
		nbeg = ibeg
		nend = iend
	      ELSE
		nbeg = ibegwt
		nend = iendwt
	    END IF
	    DO ii = nbeg, nend
		iempty = 0
		IF ( INDEX ( clist ( ii ), cc ) .eq. 0 ) THEN
     		    clist ( ii ) = ' '
		    iempty = iempty + 1
		END IF
		IF ( INDEX ( zlist ( ii ), 'Z' ) .eq. 0 ) THEN
     		    zlist ( ii ) = ' '
		    iempty = iempty + 1
		END IF
		IF ( iempty .eq. 2 ) THEN
		    DO ll = 1, 3
			ivtec ( ll, ii ) = 0
		    END DO 
		END IF
	    END DO
	END DO
C*
	RETURN
	END
