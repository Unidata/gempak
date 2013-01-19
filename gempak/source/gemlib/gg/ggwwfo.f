        SUBROUTINE GG_WWFO ( wtchlst, wfolst, iret )
C************************************************************************
C* GG_WWFO        							*
C*        								*
C* This subroutine creates a unique list of the WFOs in all of the 	*
C* listed watches. The WFOs are separated by "...".			*
C*        								*
C* GG_WWFO ( WTCHLST, WFOLST, IRET )					* 
C*        								*
C* Input parameters:        						*
C*        WTCHLST	CHAR*		Watch list			* 
C*        								*
C* Output parameters:        						*
C*        WFOLST 	CHAR*		WFOs list 			*
C*        IRET		INTEGER		Return code			*
C*        			         8 = Num of cnties/wfocty exceed*
C*        			         7 = Dim of sacod exceeded	*
C*					 0 = Normal return		*
C*					-1 = No watches/no WFOs		*
C*									*
C**									*
C* Log:        								*
C* M. Li/SAIC		06/03                                      	* 
C* A. Hardy/NCEP         8/03 		Changed GG_WACT calling seq.    *
C* B. Yin/SAIC           3/04   	Changed SS_GTIM to CSS_GTIM.    *
C* A. Hardy/NCEP         4/04 		Added ST_RNUL			*
C* A. Hardy/NCEP         8/04 		Added itest to gg_wact		*
C* A. Hardy/NCEP         3/05 		Added read of WCN action code   *
C* A. Hardy/NCEP         3/05 		Added ircntm to GG_WACT		*
C* F. J. Yen/NCEP	 3/07		Increased dim. of sacod.Replaced*
C*					dim. LLSTFL w/ MAX_CNTY.Improved*
C*					getting & making WFO IDs unique.*
C************************************************************************
        INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*) 	wtchlst, wfolst
C*
	CHARACTER       system*20, strtim*20, stptim*20, type*3, 
     +			cnties(MAX_CNTY)*156,
     +			mtchid(MAXWFO)*3, tmpstr*128, sacod(100)*4, 
     +                  wfocty(MAX_CNTY)*128, ircntm*20
	INTEGER		iwarr(80), itype, numarr 
	LOGICAL         have, etmchg
C-----------------------------------------------------------------------
	iret   = 0
	iunum = 0
	wfolst = ' '
        ircntm = ' '
C
	CALL ST_RXBL ( wtchlst, wtchlst, lenw, ier )
	CALL ST_ILST ( wtchlst, ' ', 0, 80, iwarr, iunum, ier )

	IF ( iunum .eq. 0 ) THEN
	    iret = -1
	    RETURN
	END IF
C
C*     	Find a list of active counties for each active watch number.
C
	itype = 1
	CALL CSS_GTIM ( itype, system, ier )
	CALL ST_RNUL ( system, system, len, ier )

	DO ii = 1, MAXWFO
            mtchid(ii) = ' '
        END DO

	imid = 0
       	DO iw = 1, iunum
            inumb = iwarr(iw)
            DO ii = 1, MAX_CNTY
               	cnties(ii) = ' '
            END DO
            etmchg = .false.
C
            CALL GG_WACT ( inumb, system, system, type, strtim,
     +                    stptim, ncnty, cnties, wfocty, knt,
     +                    etmchg, itest, sacod, numarr, ircntm, iret )
	    IF ( ncnty .gt. 0 ) THEN
C*
C*		Get unique WFO IDs from cnties and store into mtchid
C*
		IF ( imid .eq. 0 ) THEN
		    imid = 1
		    mtchid (imid) = cnties (1) (90:92)
		    jwst = 2
		  ELSE
		    jwst = 1
		END IF
		IF ( jwst .le. ncnty ) THEN
		  DO jw = jwst, ncnty
		    have = .false.
		    im = 1
		    DO WHILE ( .not. have .and. im .le. imid )
			IF ( cnties (jw) (90:92) .eq.
     +				     mtchid (im) ) THEN
			    have = .true.
			END IF    
			im = im + 1
		    END DO
		    IF ( .not. have ) THEN
			imid = imid + 1
			mtchid (imid) = cnties (jw) (90:92)
		    END IF
	          END DO 
		END IF
	    END IF
	END DO
C
C*     WFO attention line.  Write out the WFOs list.
C
       tmpstr = ' '
       IF ( imid .gt. 0 ) THEN
           DO ii = 1, imid
               CALL ST_LSTR ( tmpstr, lenw, ier )
               tmpstr= tmpstr(:lenw) // '...' // mtchid(ii)
           END DO
       END IF

       CALL ST_LSTR(tmpstr, lent, ier)
       wfolst = tmpstr(:lent) // CHNULL
C*
       RETURN
       END
