	SUBROUTINE WS_SGMT ( segment, lens, dattim, timstr, wtype, wthr,
     +		   	     strtim, stptim, icancl, nhdln, zones, 
     +			     nzone, iret)
C************************************************************************
C* WS_SGMT 								*
C*									*
C* This subroutine decodes a single winter storm segment.            	*
C*                                                                      *
C* WS_SGMT ( SEGMENT, LENS, DATTIM, TIMSTR, WTYPE, WTHR, STRTIM, STPTIM,*
C*	     ICANCL, NHDLN, ZONES, NZONE, IRET )			*
C*									*
C* Input parameters:	                                                *
C*  	SEGMENT	  	CHAR* 	  Bulletin segment			*
C*	LENS  	  	INTEGER	  Length of segment			*
C*	DATTIM		CHAR*	  Default start time, GEMPAK frmt	*
C*	TIMSTR		CHAR*	  Initial local time string	        *
C*									*
C* Output parameters:							*
C*  	WTYPE (2)  	CHAR* 	  Winter storm message type		*
C*	WTHR  (2)  	CHAR* 	  Weather type				*
C*	STRTIM (2)  	CHAR  	  Report start time, GEMPAK format	*
C*	STPTIM (2)  	CHAR* 	  Report stop time, GEMPAK format	*
C*	ICANCL (2)  	INTEGER	  Cancellation flag		 	*
C* 	NHDLN		INTEGER	  Number of headlines			*
C*	ZONES (NZONE)   CHAR*6	  Zone names in storm area	 	*
C*	NZONE 	  	INTEGER	  Number of zones in area	 	*
C*	IRET  	  	INTEGER	  Return code			 	*
C*				    0 = normal return			*
C*				    1 = expired at orig time		*
C*				   -1 = no zone information found       *
C*				   -2 = could not get end time          *
C*				   -3 = no message type found           *
C*				   -7 = error in zone information       *
C*									*
C**									*
C* Log:									*
C* M. Li/SAIC		08/02						*
C* D. Kidwell/NCEP	 9/02	Added error checks; fixed typo          *
C* M. Li/SAIC		10/02	Improved decoding functions		*
C* D. Kidwell/NCEP	10/02	Added check ff TI_LOCL; fixed ihead     *
C* M. Li/SAIC		10/02	Modified method to get start time	*
C* D. Kidwell/NCEP	11/02	Added timstr, plural, recoded for endtim*
C* D. Kidwell/NCEP	11/02	Bug fix to find time group when day = 20*
C* M. Li/SAIC		02/03	Increase dim to handle double headlines	* 
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	segment, dattim, wtype(*), wthr(*), strtim(*), 
     +			stptim(*), zones(*), timstr
C*
	PARAMETER       ( NZONES = 11 )
	CHARACTER	strbuf*160, endtim*20, purge*6,
     +		  	wmsg(3)*10, tzones (NZONES)*3, newtim*20,
     + 	 		tmptim*50, timseg*50
	INTEGER		idtarr (5), irdtar (5), inew (5), iend (160)
	INTEGER		icancl(*), ihdln(2), nhead(2)
	LOGICAL		done, find, good, expir1
C*
	DATA 		wmsg / ' WATCH', ' WARNING', ' ADVISOR' /
	DATA            tzones / 'GMT', 'UTC', 'Z  ',
     +                           'EDT', 'EST', 'CDT', 'CST',
     +                           'MDT', 'MST', 'PDT', 'PST' /
C------------------------------------------------------------------------
	iret  = 0
C
C*	Ensure that segment is upper case.
C
	CALL ST_LCUC ( segment, segment, ier )
C
C*	Look for zone information string and decode it.
C
	strbuf = segment ( :MIN(160, lens) )
	CALL ST_RMBL ( strbuf, strbuf, lenb, ier )
	done = .false.
	ix = 1
	good = .true.
	DO WHILE ( .not. done )
	    CALL ST_NOCC ( strbuf, '-', ix, iend(ix), ier )
	    IF ( ier .eq. 0 .and. iend(ix) - iend(ix-1) .eq. 7 ) THEN
		inum = 1
		DO ii = 1, 6
		    CALL ST_ALNM ( strbuf(iend(ix)-ii:iend(ix)-ii),
     +				   kk, ier )
		    inum = inum * kk 
		END DO
	  	IF ( inum .eq. 1 ) THEN
		    izone = iend (ix)
		    done = .true.
		  ELSE
		    ix = ix + 1
		    IF ( ix .ge. lenb ) THEN
			done = .true.
			good = .false.
		    END IF
		END IF
	      ELSE 
	  	ix = ix + 1
		IF ( ix .ge. lenb ) THEN
		    done = .true.
		    good = .false.
		END IF
	    END IF 
	END DO
C
	IF ( good ) THEN
C
C*	    Decode zone info.
C
	    CALL WS_ZONE ( strbuf (:izone), izone, zones, nzone, purge,
     +			   ier )
	    IF ( ier .eq. -1 ) iret = -7
C
C*	    Look for WARNING, WATCH, or ADVISORY.  Check for plurals.   
C
	    itime = izone + 1 
	    iw = lens
	    DO ii = 1, 3
	        CALL ST_LSTR ( wmsg(ii), nlen, ier )
	        nn = INDEX ( segment(itime:lens), wmsg(ii)(:nlen) )
	        ie = itime + nn + nlen - 1
	        IF ( nn .gt. 0 ) THEN 
		    IF ( ( ii .eq. 1 ) .and.
     +			 ( segment ( ie:ie+1 ) .eq. 'ES' ) ) THEN
		        ie = ie + 2
		      ELSE IF ( ( ii .eq. 2 ) .and.
     +				( segment ( ie:ie ) .eq. 'S' ) ) THEN
			ie = ie + 1
		      ELSE IF ( ii .eq. 3 ) THEN
			IF ( segment ( ie:ie ) .eq. 'Y' ) THEN
			    ie = ie + 1
			  ELSE IF ( segment ( ie:ie+2) .eq. 'IES' ) THEN
			    ie = ie + 3
			END IF
		    END IF		
 		    IF ( ( segment (ie:ie) .eq. ' ' ) .or. 
     +		         ( segment (ie:ie) .eq. '.') ) THEN
		        IF ( iw .gt. nn ) iw = nn
	            END IF
		END IF
	    END DO 
	    IF ( iw .eq. lens ) THEN
		iret = -3
		RETURN
	    END IF
C
C*          Look for the time group.
C
            ntime = 0 
	    find = .false.
	    IF ( iw .gt. 0 ) THEN
	        ifind = 0
	        DO WHILE ( .not. find .and. ifind .le. iw )
		    iam = 0
	  	    jj = ifind + itime	
		    CALL ST_ALNM ( segment(jj:jj), ityp, ier )
		    iam = INDEX ( segment(jj:jj+10), ' AM ' ) +
     +                        INDEX ( segment(jj:jj+10), ' PM ' )
		    IF ( ityp .eq. 1 .and. iam .gt. 0 ) THEN
			done = .false.
			kk   = jj
			myr  = kk + 50
			DO WHILE ( .not. done )
		  	  iyr = INDEX ( segment(kk:myr), ' 20' )
			  nn1 = kk + iyr + 2
            	   	  nn2 = nn1 + 1       
            	   	  CALL ST_ALNM ( segment (nn1:nn1), ityp1, ier )
            	  	  CALL ST_ALNM ( segment (nn2:nn2), ityp2, ier )
C
		   	  IF ( iyr .gt. 0 .and. ityp1 .eq. 1 .and.
     +                         ityp2 .eq. 1 ) THEN
                              itime = jj 
                              ntime = kk + iyr + 4
			      find  = .true.
			      done = .true.
			  ELSE
			      kk = kk + iyr
			      IF ( ( iyr .eq. 0 ) .or. ( kk .ge. myr ) ) 
     +				   THEN
      			          done = .true.
				  ifind = iw + 1
			      END IF
                          END IF
			END DO
C
		      ELSE
		        ifind = ifind + 1
		    END IF
	        END DO		    
	    END IF
C
C*          Get the start time if given.
C
	    IF ( find ) THEN
	        CALL TI_LOCL (segment(itime:(ntime -1)), strtim(1),
     +			      ier) 
		IF ( ier .eq. 0 ) THEN
		    timseg = segment ( itime:( ntime - 1 ) )
		  ELSE
		    find = .false.
		END IF
	    END IF
	    IF ( .not. find ) THEN
		strtim(1) = dattim
		timseg = timstr
	    END IF
C
C*          Compute the end time. 
C
	    CALL TI_CTOI ( strtim(1), idtarr, ier1 )
	    CALL ST_INTG ( purge, ipurge, ier2 )
	    IF ( ( ier1 .eq. 0 ) .and. ( ier2 .eq. 0 ) ) THEN
	        iday = ipurge / 10000
	        ihour = ipurge /100 - iday * 100
	        imin = ipurge - iday * 10000 - ihour * 100
	        CALL DC_ITIM ( idtarr, iday, ihour, imin, irdtar, ier ) 
	        CALL TI_ITOC ( irdtar, stptim(1), ier )
	      ELSE
		iret = -2
		RETURN
	    END IF
C
C*	    Look for the headline and decode it.
C
	    ihead = 0
	    ii = iw-2
	    DO WHILE ( ii .gt. 0 )
		kk = izone + ii 
		IF ( segment (kk:kk) .eq. '.' .and. 
     + 		     segment (kk-1:kk-1) .eq. '.' .and.
     +		     segment (kk-2:kk-2) .eq. '.' ) THEN
		    ihead = kk - 2 
		    ii = 0
		END IF 
		ii = ii - 1
	    END DO
	    IF ( ihead .le. izone ) ihead = izone + iw - 20
C
	    msch = MIN ( 200, (lens - ihead) )
	    idot = INDEX ( segment (ihead+3:ihead+msch), '...' )
C
C*	    Look for the second headline
C
	    nhdln = 1
	    IF ( idot .gt. 0 ) THEN
		idot2 = INDEX (segment (ihead+3:ihead+msch), '... ...')	   
	        IF ( idot2 .eq. idot ) THEN
		   iend2 = INDEX (segment (ihead+idot+9:ihead+msch), '...') 
		   IF ( iend2 .gt. 0 ) THEN
		      nhdln = 2
		      strtim(2) = strtim(1)
		      stptim(2) = stptim(1)
		      ihdln(2) = ihead + idot + 9
		      nhead(2) = iend2 + 3
		   END IF
		END IF
	     ELSE 
     +		idot = INDEX ( segment (ihead+3:ihead+msch), '.' ) 
	    END IF
C
	    IF ( idot .gt. 0 .and. idot .le. lens ) THEN
		nhead(1) = idot + 3
	    ELSE
		nhead(1) = msch
	    END IF 
	    ihdln(1) = ihead
C
	 numhds = nhdln
	 expir1 = .false.
	 DO nn = 1, numhds
 	    CALL WS_HDLN ( segment (ihdln(nn):ihdln(nn)+nhead(nn)), 
     +      nhead(nn), wtype(nn), wthr(nn), icancl(nn), endtim, ier )
	    IF ( wtype(nn) .eq. ' ' ) THEN
		IF ( nn .eq. 1 ) THEN
		    iret = -3
		  ELSE
	            nhdln = 1
	            IF ( expir1 ) iret = 1
		END IF
	        RETURN
	    END IF
C
	    IF (  ( ier .eq. 0 ) .and. ( endtim .ne. ' ' ) ) THEN
C
C*		A time string was embedded in the headline.  First, 
C*		check for a time zone in endtim.
C
		CALL ST_LSTR ( endtim, ll, ier)
		CALL ST_FIND (endtim(ll-2:), tzones, NZONES, ipos, ier1)
		IF ( ipos .eq. 0 ) THEN
C
C*		    The embedded time string did not have a time zone.
C
		    im = INDEX ( endtim ( :ll ), 'M' )
		    ib = INDEX ( timseg, 'M' ) + 1
		  ELSE
C
C*		    The embedded time string included a time zone.
C
		    im = ll
		    ib = INDEX ( timseg, 'T' ) + 1
		END IF	     
C
C*		Try to construct a new start time (for a cancellation) 
C*		or stop time (for a non-cancellation) using the embedded
C*		time string.
C
		IF ( ( im .gt. 0 ) .and. ( ib .gt. 1 ) ) THEN
		    tmptim = endtim ( :im ) // 
     +			     timseg ( ib: )     
		    CALL TI_LOCL ( tmptim, newtim, ier )
		    IF ( ier .eq. 0 ) THEN
            	        CALL TI_CTOI ( newtim, inew, ier )
		        IF ( ier .eq. 0 ) THEN
			    CALL TI_MDIF ( inew, idtarr, nmin, ier )
			    IF ( icancl(nn) .eq. 0 ) THEN
C
C*				This is not a cancellation.  Replace the
C*				stop time, using the embedded string 
C*				time in place of the product purge time.
C
				IF ( nmin .le. 0 ) CALL TI_ADDD ( inew,
     +							     inew, ier )
				CALL TI_ITOC ( inew, tmptim, ier )
				IF ( ier .eq. 0 ) stptim(nn) = tmptim
			      ELSE
C
C*				This is a cancellation.  If the
C*				expiration time is within 4 hours of the
C*				issue time, replace the start time,
C*				using the embedded string time in place
C*				of the local time string.
C
				IF ( nmin .lt. -240 ) THEN
				    CALL TI_ADDD ( inew, inew, ier )
				    CALL TI_MDIF ( inew, idtarr, nmin, 
     +						   ier )
				END IF
				IF ( ( nmin .ge. -240 ) .and. 
     +				     ( nmin .le. 240  ) ) THEN
				    CALL TI_ITOC ( inew, tmptim, ier )
				    IF ( ier .eq. 0 ) strtim(nn) = tmptim
				END IF
			    END IF
	 	        END IF
		    END IF		
		END IF
	      ELSE IF ( ier .eq. 1 ) THEN
		IF ( nhdln .eq. 1 ) THEN
		    iret = 1
		  ELSE
		    IF ( nn .eq. 2 ) THEN
		        nhdln = 1
	 	        IF ( expir1 ) iret = 1
		      ELSE
			expir1 = .true.
	            END IF
		END IF
	    END IF
C
	  END DO
C
C* 	  Check the icancl
C
	  IF ( nhdln .eq. 2 ) THEN
	      IF ( icancl(1) .eq. 0 .and. icancl(2) .eq. 0 ) nhdln = 1
	  END IF
C
C*	  Check for the case of 2 headlines
C
	  IF ( expir1 .and. ( iret .ne. 1 ) ) THEN
	     nhdln = 1
	     wtype(1) = wtype(2)
	     wthr (1) = wthr (2)
	     strtim (1) = strtim (2)
	     stptim (1) = stptim (2)
	     icancl (1) = icancl (2)
	  END IF
C*
	  ELSE
	    iret = -1
	END IF
C*
	RETURN
	END
