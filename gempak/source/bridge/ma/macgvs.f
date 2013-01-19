	SUBROUTINE MA_CGVS ( string, len, iret )
C************************************************************************
C* MA_CGVS                                                              *
C*                                                                      *
C* This subroutine decodes the visibility data in one report.           *
C* Check to see if visisbility is reported in sm or nm.  Value is	*
C* stored in common /rintfv/.						*
C*                                                                      *
C* MA_CGVS  ( STRING, LEN, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      STRING          CHAR*           String containing visibility    *
C*                                      data                            *
C*      LEN             INTEGER         Length of string                *
C*                                                                      *
C* Output parameters:                                                   *
C*      RIVALS(IRVSBY)  REAL            Visibility in statute mi.       *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = Normal Return             *
C**                                                                     *
C* Log:                                                                 *
C* C. Caruso Magee/NCEP	 4/01	Original Author                         *
C* F. J. Yen/NCEP	 4/01	Cleaned up and renamed from CG_GTVS.	*
C*				Changed prologue regarding common parm.	*
C************************************************************************
	INCLUDE		'macmn.cmn'
	CHARACTER*(*)	string
C*
	CHARACTER*8	newfield
	LOGICAL		fndvis
C------------------------------------------------------------------------
	iret = 0
	CALL MA_CGBG ( string(1:len), iret )
	i = 1
	fndvis = .false.
	DO WHILE ((.not. fndvis) .and. (i .le. nflds ))
	    IF ( itypsf(i) .eq. NMR ) THEN
		fndvis = .true.
		ivisst = i
	      ELSE IF ( itypsf(i) .eq. NALNMR ) THEN
		IF ( fields(i) .eq. '.' ) THEN
		    fndvis = .true.
		    ivisst = i
		  ELSE
		    i = i + 1
		END IF
	      ELSE
	        i = i + 1
	    END IF
	END DO

	IF ( fndvis ) THEN
	    nfldlf = nflds - ivisst
	    IF ( itypsf(ivisst) .eq. NMR ) THEN
		IF ( nfldlf .eq. 0 ) THEN
C
C*		    If nfldlf is 0, number is last field in wxvs string.
C
		    CALL ST_INTG ( fields(ivisst)(1:lensf(ivisst)),
     +				   inum, jret )
		    IF ( jret .ne. 0 ) RETURN
		    rivals(irvsby) = float(inum)
		  ELSE IF ( nfldlf .eq. 1 ) THEN
C
C*		    If nfldlf is 1, number is next to last field
C*		    in wxvs string.
C
		    IF ( itypsf(ivisst+1) .eq. ALPHA .and.
     +			 index(fields(ivisst+1),'YD') .ne. 0 ) THEN
C
C*			Visibility is in yards.  Convert to sm
C*			(1760 yds = 1 sm).
C
			CALL ST_INTG ( fields(ivisst)(1:lensf(ivisst)), 
     +				       inum, jret )
			IF ( jret .ne. 0 ) RETURN
			    rivals(irvsby) = float(inum)/1760. 
			  ELSE
C
C*			    Field which follows is not YD or YDS so
C*			    just save number into visibility.
C
			    CALL ST_INTG ( fields(ivisst)
     +					 (1:lensf(ivisst)), inum, jret )
			    IF ( jret .ne. 0 ) RETURN
			    rivals(irvsby) = float(inum)
			END IF 
		      ELSE
C
C*			More than one field left.  Check to see if
C*			next field is '.' or '/'.
C
			IF ( itypsf(ivisst+1) .eq. NALNMR ) THEN
			    IF ( fields(ivisst+1) .eq. '.' ) THEN
C
C*				Next field is a '.'. check to see if
C*				followed by number.
C
                 		IF ( itypsf(ivisst+2) .eq. NMR ) THEN
				  newfield = fields(ivisst)
     +			           (1:lensf(ivisst))//
     +				   fields(ivisst+1)(1:lensf(ivisst+1))//
     +				   fields(ivisst+2)(1:lensf(ivisst+2))
				  CALL ST_CRNM ( newfield, realnum,
     +						 kret )
				  IF ( kret .eq. 0 ) rivals(irvsby) =
     +								realnum

			        END IF
			      ELSE IF ( fields(ivisst+1) .eq. '/' ) THEN
C
C*				Next field is a '/'. check to see if
C*				followed by number.
C
				IF ( itypsf(ivisst+2) .eq. NMR ) THEN
				  CALL ST_INTG ( fields(ivisst)
     +						 (1:lensf(ivisst)),
     +						 inumer, jret )
				  IF ( jret .ne. 0 ) RETURN
				  CALL ST_INTG ( fields(ivisst+2)
     +						 (1:lensf(ivisst+2)),
     +						 idenom, jret )
				  IF ( jret .ne. 0 ) RETURN
				  rivals(irvsby) = float(inumer)/
     +						   float(idenom)
			    END IF
			  ELSE
C
C*			    Next field is non-alphanum and not part of
C*			    vis so just save number into vis
C               
			    CALL ST_INTG ( fields(ivisst)
     +				    (1:lensf(ivisst)), inum, jret )
			    IF ( jret .ne. 0 ) RETURN
				rivals(irvsby) = float(inum)
			END IF
		      ELSE
C
C*			Next field is a character.
C
			IF ( index(fields(ivisst+1),'YD') .ne. 0 ) THEN
C
C*			    Visibility is in yards.  Convert to sm
C*			    (1760 yds = 1 sm).
C
			    CALL ST_INTG ( fields(ivisst)
     +					   (1:lensf(ivisst)),
     +				          inum, jret )
			    IF ( jret .ne. 0 ) RETURN
			    rivals(irvsby) = float(inum)/1760. 
			  ELSE
			    CALL ST_INTG ( fields(ivisst)
     +				   (1:lensf(ivisst)), inum, jret )
			    IF ( jret .ne. 0 ) RETURN
				rivals(irvsby) = float(inum)
			END IF
		    END IF
		END IF
	      ELSE IF ( itypsf(ivisst) .eq. NALNMR ) THEN
C
C*		Start of vis field is a '.'.  Check the next field.
C*		If '.' is last field in wxvs string, return.
C
		IF ( nfldlf .eq. 0 ) THEN
		    RETURN
		  ELSE IF ( nfldlf .ge. 1 ) THEN
C
C*		    If nfldlf is 1 or more, check to see if next
C*		    field is number.  If so, cat the number to the
C*		    decimal and convert to a real.
C
		    IF ( itypsf(ivisst+1) .eq. NMR ) THEN
			    newfield = fields(ivisst)(1:lensf(ivisst))//
     +                      fields(ivisst+1)(1:lensf(ivisst+1))
		        CALL ST_CRNM ( newfield, realnum, jret )
			IF ( jret .ne. 0 ) RETURN
			    rivals(irvsby) = realnum
		    END IF
		END IF
	    END IF
	  ELSE
	    logmsg = ' No valid visibility string found--' // string
	    CALL DC_WLOG  ( 2, 'MA', 1, logmsg, ierwlg )
	END IF
C*
	RETURN
	END
