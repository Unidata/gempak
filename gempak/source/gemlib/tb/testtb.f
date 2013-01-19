	PROGRAM TESTTB
C************************************************************************
C* TESTTB								*
C*									*
C* This program tests the TABLE library subroutines			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/84						*
C* M. desJardins/GSFC	10/88	Rewritten				*
C* S. Jacobs/EAI	 7/92	Added FL_TOPN				*
C* K. Brill/NMC		 8/93	stnid*4 -> stnid*8, ISPRI		*
C* D. Keiser/GSC	12/95	Added FL_TBOP and removed FL_TOPN	*
C* G. Krueger/EAI	 5/96	Crnr pts, default proj; Check bad table	*
C* L. Sager/NCEP         6/96   Changed calling sequence for TB_RSTN    *
C*                              to add additional parameter string      *
C* L. Sager/NCEP	 6/96   Added TB_ASTN				*
C* S. Schotz/NCEP	 7/96	Added option to write out all stations  *
C*				for TB_ASTN, added FL_CLOS		*
C* S. Jacobs/NCEP	11/96	Added station name to TB_ASTN; Clean up	*
C* S. Maxwell/GSC	 2/97	Added TB_PARM				*
C* S. Jacobs/NCEP	12/97	Added TB_NIDS				*
C* S. Jacobs/NCEP	 7/98	Added TB_FONT				*
C* A. Hardy/GSC		 8/99   Added TB_IDST				*
C* S. Jacobs/NCEP	12/99	Changed size of tbchr and tbchrs 14->20	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER 	file*72, geog*20, cdproj*30, stnid*8, stnnam*32,
     +			state*2, contry*2, type*72, tbchrs*20,
     +			id (LLSTFL)*8, tbchr (LLSTFL)*20,
     +			st (LLSTFL)*2, con (LLSTFL)*2,
     +			stnam (LLSTFL)*32, prmlst*72, colors*72,
     +			parms*72, prdnam*8, units*8, desc*20, font*12
	REAL      	rla (LLSTFL), rlo (LLSTFL), hgh (LLSTFL) 
	INTEGER		inum (LLSTFL), ispi (LLSTFL) 
C------------------------------------------------------------------------
	CALL IN_BDTA ( iret )
	iostat = 0
	DO WHILE ( iostat .eq. 0 )
	    WRITE(6,20)
20	    FORMAT 
     +  ('  1 = TB_FGEO    2 = TB_RSTN    3 = TB_ASTN '/
     +   '  4 = TB_PARM    5 = TB_NIDS    6 = TB_FONT '/
     +   '  7 = TB_IDST '/
     +   ' 20 = FL_SWOP   21 = FL_APND   22 = FL_TBOP '/
     +   ' 23 = FL_CLOS   24 = FL_REWD'/ )
C
	    CALL TM_INT ( 'Select a subroutine number', .false.,
     +      .false., 1, numsub, n, ier )
	IF ( ier .eq. 2 ) THEN
	   iostat = -1
           numsub = -1
 	END IF
2	    FORMAT (A)
C------------------------------------------------------------------------
	    IF (numsub .eq. 1) THEN
		WRITE (6,*) 'Enter GEOG'
		READ  (5,2)  geog
		CALL  TB_FGEO  ( geog, rlatll, rlonll, rlatur, rlonur,
     +				 cdproj, cenlat, cenlon, iret )
		WRITE (6,*) 'RLATLL,RLONLL,RLATUR,RLONUR:',
     +			    rlatll,rlonll,rlatur,rlonur
		WRITE (6,*) 'CDPROJ:', cdproj
		WRITE (6,4) 'CENLAT,CENLON,IRET:', cenlat,cenlon,iret
4	    	FORMAT (A, 2F8.2, I4)
	        IF ( (iret .ne. 0) .and.
     +		     (iret .ne. 1) .and. (iret .ne. -10) ) THEN
		    CALL ER_WMSG ( 'TB', iret, geog, ier )
		END IF
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 2) THEN
	        WRITE (6,*) 'ENTER LUN'
	        READ  (5,*)  lun
		CALL TB_RSTN  ( lun, stnid, stnnam, isnum, state,
     +				contry, rlat, rlon, hght, ispri,
     +				tbchrs, iret )
		WRITE (6,*) 'IRET = ', IRET
	        WRITE (6,1000)  stnid, isnum, stnnam, state, contry,
     +				rlat, rlon, hght, ispri, tbchrs
1000		FORMAT ( A, 1X, I6, 1X, A, 1X, A, 1X, A, 1X,
     +                   F9.2, 1X, F9.2, 1X, F9.2, 1X, I2, 1X, A )
	        IF (iret .ne. 0) CALL ER_WMSG ( 'TB', iret, ' ', ier )
C------------------------------------------------------------------------
              ELSE IF (numsub .eq. 3) THEN
                WRITE (6,*) 'ENTER LUN'
                READ  (5,*)  lun
                WRITE (6,*) 'ENTER MAXSTN, the number of stns to read'
                READ  (5,*)  maxstn
                CALL TB_ASTN  ( lun, maxstn, nstn, id, stnam, inum, st,
     +				con, rla, rlo, hgh, ispi, tbchr, iret )
		WRITE (6,*) 'Number of stations read, NSTN = ', nstn
                WRITE (6,*) 'IRET = ', iret 
		IF (nstn .gt. 0 ) THEN
    		    WRITE (6,*) ' To list all the stations read,',
     +				' enter 1:'
		    READ  (5,*) lststn
		    IF ( lststn .eq. 1 ) THEN
		        DO i = 1, nstn
		           WRITE (6,1000) id (i), inum (i), stnam (i),
     +					  st (i), con (i), rla (i),
     +					  rlo (i), hgh (i), ispi (i),
     +					  tbchr (i)
	      	        END DO
		    END IF
		END IF
                IF (iret .ne. 0) CALL ER_WMSG ( 'TB', iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 4)  THEN
		WRITE (6,*) ' ENTER ALIAS NAME:'
		READ  (5,2) prmlst
		CALL TB_PARM ( prmlst, parms, colors, iret )
		WRITE (6,*) 'PARMS = ',parms
		WRITE (6,*) 'COLORS = ',colors
		WRITE (6,*) 'IRET = ', iret
                IF (iret .ne. 0) CALL ER_WMSG ( 'TB', iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 5)  THEN
		WRITE (6,*) 'Enter source ID number:'
		READ  (5,*) isrc
		WRITE (6,*) 'Enter product ID number:'
		READ  (5,*) iprod
		CALL TB_NIDS ( isrc, iprod, prdnam, nlev, units,
     +			       res, desc, iret )
		IF  ( iret .eq. 0 )  THEN
		    CALL ST_RNUL(prdnam, prdnam, lens, ier )
		    CALL ST_RNUL(units, units, lens, ier )
		    CALL ST_RNUL(desc, desc, lens, ier )
		    WRITE (6,*) 'PRDNAM = ', prdnam
		    WRITE (6,*) 'NLEV   = ', nlev
		    WRITE (6,*) 'UNITS  = ', units
		    WRITE (6,*) 'RES    = ', res
		    WRITE (6,*) 'DESC   = ', desc
		END IF
		WRITE (6,*) 'IRET   = ', iret
                IF (iret .ne. 0) CALL ER_WMSG ( 'TB', iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 6)  THEN
		WRITE (6,*) ' Enter font size name:'
		READ  (5,2) font
		CALL TB_FONT ( font, fsize, iret )
		WRITE (6,*) 'Font size = ', fsize
		WRITE (6,*) 'IRET   = ', iret
                IF (iret .ne. 0) CALL ER_WMSG ( 'TB', iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 7) THEN
		WRITE (6,*) 'Enter the 2 character state ID'
                READ(5,2)state
		CALL TB_IDST  ( state, stnnam, iret )
		WRITE (6,*) 'IRET = ', IRET
		WRITE (6,*) 'State is : ',stnnam
	        IF (iret .ne. 0) CALL ER_WMSG ( 'TB', iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 20) THEN
		WRITE (6,*) 'ENTER FILE'
		READ  (5,2)  file
		CALL FL_SWOP  ( file, lun, iret )
		WRITE (6,*) 'LUN,IRET: ', lun, iret
	        IF (iret .ne.. 0) CALL ER_WMSG ('FL', iret, ' ', ier)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 21 )  THEN
		WRITE (6,*) 'ENTER LUN'
		READ  (5,*)  lun
		CALL FL_APND  ( lun, iret )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 22)  THEN
		WRITE (6,*)' ENTER FILE'
		READ  (5,2) file
		WRITE (6,*)' ENTER TYPE'
		READ  (5,2) type
		CALL FL_TBOP ( file, type, lun, iret )
		WRITE (6,*) 'lun, iret ', lun, iret
		IF (iret .ne. 0) CALL ER_WMSG ( 'FL',  iret,
     +                                          file, ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 23)  THEN
		WRITE (6,*) ' ENTER LUN'
		READ (5,*) lun
		CALL FL_CLOS (lun, iret)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 24)  THEN
		WRITE (6,*) ' ENTER LUN'
		READ (5,*) lun
		CALL FL_REWD (lun, iret)
C------------------------------------------------------------------------
	    END IF
	END DO
C
	END
