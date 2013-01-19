	SUBROUTINE TEXT_OUTPUT (nlun, luns, outstr)

	CHARACTER*(*)	outstr
	INTEGER		nlun, luns(*)

	DO i=1,nlun
	   write(luns(i),*) outstr
	END DO

	RETURN
	END
