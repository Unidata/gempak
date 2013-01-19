	SUBROUTINE GG_SPLT ( stnplt, iret )
C************************************************************************
C* GG_SPLT								*
C*									*
C* This subroutine plots the station information and marker using the	*
C* information in a GEMPAK station file. Setting the text color to 0 	*
C* disables the	display of the station ID. Setting the marker color to 	*
C* 0 disables the display of the station marker and centers the station *
C* ID string on the station.  Multiple files may be plotted by 		*
C* separating STNPLT values with a "+" sign, i.e., 			*
C*	STNPLT = 3|1|table1.tbl + 5|2//2|table2.tbl			*
C* An optional station table column to be printed may be specified	*
C* following the station table name separated by the '#' character.	*
C* If the table column is not specified, the station ID is used.	*
C*									*
C* GG_SPLT ( STNPLT, IRET )						*
C*									*
C* Input parameters:							*
C*	STNPLT		CHAR*		Txtcolr/txtsz/txtfnt/txtwdth/	*
C*					border/rotn/just/hwflg|mkcolr/	*
C*					mktyp/mksz/mkwdth/hwflg|stnfl	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C**									*
C* Log:									*
C* D. Keiser/GSC	12/95						*
C* S. Jacobs/NCEP	 5/96	Increased starr from 28 to 72 chars	*
C* L. Sager/NCEP 	 6/96	Changed calling sequence for TB_RSTN	*
C*				to add character string parameter	*
C* S. Jacobs/NCEP	11/96	Eliminated arrays for station info	*
C* D.W.Plummer/NCEP	 8/97	Added '+' separator for multiple files	*
C* T. Lee/GSC		 6/98	Added text attributes			*
C* S. Jacobs/NCEP	12/99	Changed size of tbchrs 14->20		*
C* S. Chiswell/Unidata	12/01	Added '#' separator to station file	*
C*				to specify data column (1 to 10)	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)   stnplt
C
	CHARACTER	stnarr(10)*72, stnplx*72, pltarr(2)*72
	CHARACTER	starr(3)*72, stid*8, stnnam*32, stat*2, coun*2,
     +			tbchrs*20, txts*72, cstnplt*32
	LOGICAL		done
C------------------------------------------------------------------------
	iret  = 0
C
	CALL ST_CLST ( stnplt, '+', ' ', 10, stnarr, nums, ier )
C
C*	Query the current text attributes.
C
	CALL GQTEXT  ( itxfn, itxhw, sztext, itxwid, ibrdr, irrotn, 
     +		       ijust, iret )
C
	DO  i = 1, nums
C
	  stnplx = stnarr(i)
C
C*	  Parse STNPLT.
C
	  CALL ST_CLST ( stnplx, '|', ' ', 3, starr, num, ier )
C
C*	  Parse station table plot column if available
C
	  CALL ST_CLST ( starr (3), '#', ' ', 2, pltarr, num, ier)
	  IF  ( num .gt. 1 ) THEN
	      starr (3) = pltarr (1)
	      CALL ST_LSTR ( pltarr (2), lens, ier )
	      CALL ST_INTG ( pltarr (2)(1:lens), ipltcol, ierr )
	      IF  ( ierr .ne. 0 )  ipltcol = 1
	      IF  ( ( ipltcol .gt. 10 ) .or. ( ipltcol .lt. 1 ) ) THEN
		  ipltcol = 1
	      END IF
	    ELSE
	      ipltcol = 1
	  END IF
C
C*	  Open the station table file.
C
	  CALL FL_TBOP ( starr (3), 'stns', lun, ier )
	  IF  ( ier .eq. 0 )  THEN
C
C*	      Process color, text, and marker information.
C
	      CALL ST_CLST ( starr (1), '/', ' ', 1, txts, numc, ier )
	      IF  ( ier .gt. 0 )  THEN
		ipos = INDEX ( starr (1), '/' ) 
		CALL IN_TEXT ( starr (1) (ipos+1: ), ier )
	      END IF
	      CALL IN_COLR ( txts, 1, itxclr, ier )
	      CALL IN_MARK ( starr (2), mkcolr, ier )
C
C*	      Check to ensure all items can be plotted.
C
	      IF ( ( itxclr .ne. 0 ) .or. ( mkcolr .ne. 0 ) )  THEN
C
C*	        Read the open station table file.
C
	        done = .false.
	        DO WHILE ( .not. done )
	          CALL TB_RSTN ( lun, stid, stnnam, istnm, stat, coun,
     +				 rlat, rlon, relv, ispri, tbchrs, iret )
	          IF  ( iret .ne. 0 )  THEN
		      done = .true.
		    ELSE
C
C*		      Plot markers, if not disabled.
C
		      IF ( mkcolr .ne. 0 ) THEN
			CALL GSCOLR ( mkcolr, iret )
			CALL GMARK  ( 'M', 1, rlat, rlon, iret )
		      END IF
C
C*		      Plot station ID's, if not disabled.
C
		      IF ( itxclr .ne. 0 ) THEN
			CALL GSCOLR ( itxclr, iret )
			rot   = 0.0
			iyoff = 0
C
C*			If the station id is blank, encode the station
C*			number as a string. If the station number is
C*			not available, use "9999" as the string value.
C
			IF  ( stid .eq. ' ' )  THEN
			    CALL ST_INCH ( istnm, stid, ierr )
			    IF  ( ierr .ne. 0 )  stid = '9999'
			END IF
C
C*			Get the info from the selected column.
C
			IF  ( ipltcol .eq. 1 ) THEN
			    cstnplt = stid
			  ELSE IF  ( ipltcol .eq. 2 ) THEN
			    CALL ST_INCH ( istnm, cstnplt, ier )
			  ELSE IF  ( ipltcol .eq. 3 ) THEN
			    cstnplt = stnnam
			  ELSE IF  ( ipltcol .eq. 4 ) THEN
			    cstnplt = stat
			  ELSE IF  ( ipltcol .eq. 5 ) THEN
			    cstnplt = coun
			  ELSE IF  ( ipltcol .eq. 6 ) THEN
			    CALL ST_RLCH ( rlat, 2, cstnplt, ier )
			  ELSE IF  ( ipltcol .eq. 7 ) THEN
			    CALL ST_RLCH ( rlon, 2, cstnplt, ier )
			  ELSE IF  ( ipltcol .eq. 8 ) THEN
			    CALL ST_RLCH ( relv, 0, cstnplt, ier )
			  ELSE IF  ( ipltcol .eq. 9 ) THEN
			    CALL ST_INCH ( ispri, cstnplt, ier )
			  ELSE IF  ( ipltcol .eq. 10 ) THEN
			    cstnplt = tbchrs
			  ELSE
			    cstnplt = stid
			END IF
C
C*			Set the x direction offset.
C
			IF  ( mkcolr .ne. 0 )  THEN
			    ixoff = 3
			  ELSE
			    CALL ST_LSTR ( cstnplt, lens, ier )
			    ixoff = -(lens)
			END IF
C
C*			Draw the text.
C
			CALL GTEXT ( 'M', rlat, rlon, cstnplt, rot,
     +				     ixoff, iyoff, iret )
		      END IF
		  END IF
		END DO
C
C*		Close the station table file.
C
		CALL FL_CLOS ( lun, iret )
	      END IF
C
	    ELSE
	      CALL ER_WMSG ( 'FL', ier, starr (3), iret )
	  END IF 
C
	END DO
C
C*	Set the original text attributes back.
C
	CALL GSTEXT  ( itxfn, itxhw, sztext, itxwid, ibrdr, irrotn, 
     +		       ijust, iret )
C*
	RETURN
	END
