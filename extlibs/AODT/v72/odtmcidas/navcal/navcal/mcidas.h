/*
 * Copyright(c) 1997, Space Science and Engineering Center, UW-Madison
 * Refer to "McIDAS Software Acquisition and Distribution Policies"
 * in the file  mcidas/data/license.txt
 */

/**** $Id: mcidas.h,v 1.4 2000/06/28 21:27:58 gad Exp $ ****/

/*******************************************************************\
          McIDAS Library Header File
\*******************************************************************/

#ifndef _MCIDAS_H
#define _MCIDAS_H


#include <sys/types.h>
#include <stdio.h>
#include <time.h>
#include "servacct.h"

/* SunOS 4.1.3 isn't in sync with POSIX */

#ifdef sparc
#ifndef _SSIZE_T	/* macro _SSIZE_T introduced in Solaris */
typedef int	ssize_t;
#endif
#endif

/* IRIX 4.0 isn't in sync with POSIX */

#ifdef __sgi
#ifndef _SSIZE_T	/* macro _SSIZE_T introduced in IRIX 5 */
typedef int	ssize_t;
#endif
#endif

/* emx compiler for OS/2 isn't in sync with POSIX */

#ifdef __EMX__
#ifndef _PID_T
typedef unsigned long     pid_t;
#endif
#ifndef _SSIZE_T	/* macro _SSIZE_T introduced in EMX 0.9a */
typedef int	ssize_t;
#endif
#endif


#ifndef    MAX
#  define  MAX(x, y)  (((x) > (y)) ? (x) : (y))    /* use greater of 2 values */
#endif  /* MAX */

#ifndef    MIN
#  define  MIN(x, y)  (((x) < (y)) ? (x) : (y))    /* use lesser of 2 values  */
#endif  /* MIN */


/* SPACE is a symbolic name for the ASCII space character */
#define SPACE ((int)0x20)

/* MCMISSING is a symbolic name for the McIDAS missing value byte */
#define MCMISSING ((int)0x80)

/* MCMISSING4 is a symbolic name for four McIDAS missing value bytes */
#define MCMISSING4 ((Fint)0x80808080)

/* MCCODE_RECOVERABLE is a symbolic name for the Mccodeset() code for a
 * recoverable application error */
#define MCCODE_RECOVERABLE (2)

/* MCCODE_UNRECOVERABLE is a symbolic name for the Mccodeset() code for an
 * unrecoverable application error */
#define MCCODE_UNRECOVERABLE (1)

/* MCCODE_SUCCESS is a symbolic name for the Mccodeset() code for successful
 * completion of an application */
#define MCCODE_SUCCESS (0)


/*
 * Various time macros for converting back and forth between integer time
 * format hhmmss (ihr), and fractional double time format hh.fffff (dhr).
 */
#define  MCHH(ihr)              ((ihr) / 10000)
#define  MCMM(ihr)              ((ihr) / 100 % 100)
#define  MCSS(ihr)              ((ihr) % 100)
/*
 * Convert from hours, minutes, seconds to fractional double hours (hh.fffff).
 */
#define  MCHMS2DHR(hh, mm, ss)  ((hh) + ((mm) * 60 + (ss)) / 3600.0)
/*
 * Convert from integer hours (hhmmss) to double hours (hh.fffff).
 */
#define  MCIHR2DHR(ihr)         MCHMS2DHR(MCHH(ihr), MCMM(ihr), MCSS(ihr))


/*
 * FsLen is the size of the "string length argument" used in
 * FORTRAN interfaces.
 */
#ifndef _FsLen
#define _FsLen
    typedef int FsLen;
#endif


/* Fint is the C equivalent of Fortran INTEGER */
#ifndef _Fint
#define _Fint
    typedef int Fint;
#endif


/* Freal is the C equivalent of Fortran REAL */
#ifndef _Freal
#define _Freal
    typedef float Freal;
#endif


/* Fdouble is the C equivalent of Fortran DOUBLE PRECISION */
#ifndef _Fdouble
#define _Fdouble
    typedef double Fdouble;
#endif

/* Fint2 is the C equivalent of Fortran INTEGER*2 */
#ifndef _Fint2
#define _Fint2
    typedef short Fint2;
#endif


/* Fint4 is the C equivalent of Fortran INTEGER*4 */
#ifndef _Fint4
#define _Fint4
    typedef int Fint4;
#endif


/* Freal4 is the C equivalent of Fortran REAL*4 */
#ifndef _Freal4
#define _Freal4
    typedef float Freal4;
#endif


/* Freal8 is the C equivalent of Fortran REAL*8 */
#ifndef _Freal8
#define _Freal8
    typedef double Freal8;
#endif


/* Mcuint2 is an unsigned 2-byte value */
#ifndef _Mcuint2
#define _Mcuint2
    typedef unsigned short Mcuint2;
#endif

/* Mcint2 is a signed 2-byte value */
#ifndef _Mcint2
#define _Mcint2
    typedef short Mcint2;
#endif

/* Mcuint4 is an unsigned 4-byte value */
#ifndef _Mcuint4
#define _Mcuint4
    typedef unsigned int Mcuint4;
#endif

/* Mcint4 is a signed 4-byte value */
#ifndef _Mcint4
#define _Mcint4
    typedef int Mcint4;
#endif

/*******************************************************************
 *            GROUP: misc byte and word manipulation routines
 *
 * blka_() - Write blanks into a buffer.
 *
 * crack_() - Unpack bytes into integer *4 words.
 *
 * ic_() - Extract a specified byte from a buffer.
 *
 * icw_() - Extract specified number of 2-byte groups from a buffer.
 *
 * ischar_() - Returns non-zero if all four characters in word are printable.
 *
 * maaatb_() -
 *
 * mgvatb_() -
 *
 * mavhtb_() -
 *
 * movb_() - Move bytes with offset.
 *
 * movblk_() - Move blocks with offsets and increments.
 *
 * movbmem_() - Move bytes from memory to memory with offset.
 *
 * movc_() - Move bytes with offsets for both source and destination.
 *
 * movpix_() - Move bytes with increments and offsets.
 *
 * movw_() - Move words (4 byte groups).
 *
 * mpixel_() - Move pixels with packing and unpacking.
 *
 * mpixtb_() - Move data with a table look-up replacement.
 *
 * mvastb_() - In-place VAS (Mode AA) pixel cracker/packer with table lookup.
 *
 * pack_() - Pack words (4 bytes hunks) into bytes in output buffer.
 *
 * pack2_() - Pack shorts (2 byte hunks) into bytes in output buffer.
 *
 * stc_() - Put a byte into a buffer.
 *
 * swbyt2_() - Swap int*2 array byte order if host byte order is not "big endian".
 *
 * swbyt4_() - Swap int*4 array byte order if host byte order is not "big endian".
 *
 * fbyte2_() - Swap int*2 array byte order.
 *
 * fbyte4_() - Swap int*4 array byte order.
 *
 * zeros_() - Zero out bytes in a buffer.
 *
 * M0swbyt4() - flips 4 bytes into network byte order
 *
 * McIsMissingFlt - does a floating point value contain the missing value
 *
 * McIsMissingDbl - does a double value contain the missing value
 *
 * McGetMissingFlt - returns the missing value for a float
 *
 * McGetMissingDbl - returns the missing value for a double
 *
 * M0FStringToCString - moves a FORTRAN string into a C string
 *
 * M0CompareStringToList() - tell whether a given string occurs in a list
 *                           of strings
 */

void
zeros_(void *buf, Fint4 *nbytes);

extern void
swbyt2_(void *buf, Fint *n);

extern void
swbyt4_(void *buf, Fint *n);

extern void
stc_(Fint4 *val, void *buffer, Fint4 *offset);

extern void
pack2_(Fint *n, Fint2 *sou, void *destination);

extern void
pack_(int n, int *sou, void *destination);

extern void
mvastb_(Fint *n, Fint *isou, Fint *ides, void *buf, Fint itab[], Fint *ysubz);

extern void
mpixtb_(Fint *n, Fint *isou, Fint *ides, void *buffer, Fint itab[]);

extern void
mpixel_(Fint *n, Fint *isou, Fint *ides, void *buffer);

extern void 
mpixel(long n, long isou, long ides, void *buf);

extern void
movw_(Fint *num, void *inbuf, void *outbuf);

extern void
movpix_(Fint *n, void *source, Fint *soff, Fint *sinc, void *destination, Fint *doff, Fint *dinc);

extern void
movc_(Fint4 *num, unsigned char inbuf[], Fint4 *soff, unsigned char outbuf[], Fint4 *doff);

extern void
movbmem_(Fint4 *num, Fint4 *inaddr, Fint4 *outaddr, Fint4 *offset);

extern void
movblk_(Fint *n, Fint *ssiz, void *source, Fint *soff, Fint *sinc, void *destination, Fint *doff, Fint *dinc);

extern void
movb_(Fint *number, void *inbuffer, void *outbuffer, Fint *offset);

void MOVB(int num, void *inbuffer, void *outbuffer, long *offset);

extern void
mavhtb_(Fint *n, Fint *isou, Fint *ides, void *buf, Fint itab[]);

extern void
maaatb_(Fint *n, Fint *isou, Fint *ides, void *buf, Fint itab[]);

extern void
mgvatb_(Fint *n, Fint *isou, Fint *ides, void *buf, Fint itab[]);

extern Fint4
ischar_(void *value);

extern Fint4
ic_(void *buf, Fint4 *offset);

extern Fint
icw_(void *buf, Fint *offset);

extern void
blka_(Fint4 *nwords, void *buf);

extern void
crack_(Fint4 *n, void *source, Fint4 *des);

extern void
fbyte2_(void *buffer, Fint *n);

extern void
fbyte4_(void *buffer, Fint *n);

extern int
M0swbyt4 ( void *, int);

extern int
M0FStringToCString ( char *, void *, int, FsLen);

extern int
M0CompareStringToList ( char *, int, char **);

extern int
McIsMissingFlt (float);

extern int
McIsMissingDbl (double);

extern float
McGetMissingFlt (void);

extern double
McGetMissingDbl (void);

/*******************************************************************
 *              GROUP: Low-level file I/O
 */


/*******************************************************************
 *               GROUP: misc McIDAS system functions
 *
 * fsalloc() - Returns pointer to a string with a 0 at end.
 *
 * fslen() - Returns logical length of a Fortran string
 *
 * strtofs() - Copies NUL-terminated C string to space-padded
 *      Fortran string.
 *
 * mcsleep_() - Sleep for milliseconds duration specified.
 *
 * Mcsleep() - Sleep for milliseconds duration specified.
 *
 * stralloc() - Dynamically allocate string from the concatenation
 *      of the given strings.
 *
 * eaccess() - Indicate whether the given pathname is accessible.
 *
 * nodename() - Get the name of the machine.
 */

extern char *
stralloc(const char *s, ...);

extern char *
fsalloc(const char *string, FsLen length);

extern FsLen
fslen(const char *string, FsLen length);

extern void
strtofs(char *dst, const char *src, FsLen ndst);

extern void
mcsleep_(Fint4 *millsecs);

extern void
Mcsleep(int millsecs);

extern int
eaccess(const char *path, int amode, int type);

extern char *
nodename(void);


/*******************************************************************
 *               GROUP: communication utilities
 */


/*******************************************************************
 *              GROUP: McPATH-related functions
 *
 * Mcpathname() - determines the system pathname of a McIDAS file
 */

#define MAXPATHLENGTH (512)

extern const char *
Mcpathname(const char *filename);

/*******************************************************************
 *              GROUP: compatibility functions
 *
 *      MODIFY YOUR CODE NOT TO USE THESE ROUTINES!
 *
 * crack2_() - Unpack bytes into integer*2 words.
 *
 * delfil_() - Delete the named file.
 *
 * filbuf_() - Fill buffer with a constant value.
 *
 * opnasync_() - Open the serial port for Unidata broadcast ingest.
 *
 * recvs_() - Read from a socket.
 *            (use m0sysread_() instead)
 *
 * sends_() - Write to a socket.
 *            (use m0syswrite_() instead)
 *
 * sleep_() - Sleep for milliseconds duration specified
 *              (use mcsleep_() instead)
 *
 * stcrep() - Store a single value into consequtive bytes of buffer.
 *
 * systyp() - Return pointer to OS name (e.g. "AIX")
 *
 * systyp_() - Set given string to OS name (e.g. "AIX")
 *
 */

extern void
crack2_(Fint4 *n, void *source, Fint2 *des);

extern Fint4
delfil_(char *file, FsLen length);

extern void
filbuf_(Fint4 *nbytes, Fint4 *value, void *buffer, Fint4 *offset);

extern Fint
opnasync_(char *cfile, FsLen length);

extern Fint
recvs_(Fint *fd, char *buf, Fint *i);

extern Fint
sends_(Fint *fd, char *buf, Fint *i);

extern void
sleep_(Fint4 *millsecs);

extern void
stcrep(Fint4 val, void *buffer, Fint4 *offset, Fint4 repfac);

extern char *
systyp(void);

extern void
systyp_(char *ctype, FsLen len);


/**************************************************************************
 *
 *      Group:  ADDE data fetching API
 *              all use FORTRAN ideom for strings and arrays of strings
 *
 *    mcaget_()            Start receipt of a virtual area
 *    mcalin_()            receive next line of data in virtual area
 *    mcapfx_()            receive prefix from line just read
 *    mcacal_()            receive cal block from virtual area
 *    mcanav_()            receive nav block from virtual area
 *
 *    mcadir_()            Start receiving area directory entries
 *    mcadrd_()            Read an area directory entry after mcadir_() call
 *
 *    mcaput_()            Start transmission of an area
 *    mcaout_()            transmit one line of an area
 *
 *    mcmdks_()            Start reading md data by keys
 *    mcmdrd_()            read one record of md data after mcmdks call
 *
 *    mcraob_()            retrieve a complete sounding from 2 mdfiles
 *
 *    mcgget_()            start transaction to receive a grid
 *    mcgrcv_()            recieve a grid in FORTRAN 2d array sequence
 *    mcgrc2_()            recieve a grid in C 2d array sequence
 *
 *    mcaname_()           construct image file data set name and position from
 *                         frame directory entry

 */

typedef char * FstringArray;   /*  FORTRAN: CHARACTER*(*) FOO(*)  */

Fint4
mcaget_( char *name, Fint4 *nsort, FstringArray sort, char *unit, char *form,
         Fint4 *maxbyte, Fint4 *msgflag, Fint4 iadir[], Fint4 *handle,
         FsLen namelen, FsLen sortlen, FsLen unitlen, FsLen formlen);

Fint4
mcalin_( Fint4 *handle, void *data);

Fint4
mcacal_( Fint4 *handle, void *data);

Fint4
mcapfx_( Fint4 *handle, void *data);

Fint4
mcanav_( Fint4 *handle, void *data);

Fint4
mcadir( char *name, Fint4 *nsorts, FstringArray sorts, Fint4 *msgflag,
        FsLen namelen, FsLen sortlen);

Fint4
mcadrd_( Fint4 data[], char *cards);

Fint4
mcaput_( char *name, Fint4 *nsort, FstringArray sort, Fint4 areadir[],
         Fint4 nav[], Fint4 cal[], FsLen namelen, FsLen sortlen);
Fint4
mcaout_( void *data);

Fint4
mcmdks_( char *name, Fint4 nsort, FstringArray sort, Fint4 *nkeys,
         FstringArray keys, FstringArray units, FstringArray form,
         Fint4 scales[], Fint4 *size, Fint4 head[], Fint4 *msgflag,
         FsLen namelen, FsLen sortlen, FsLen keylen, FsLen unitlen,
         FsLen formlen);

Fint4
mcmdrd_( void *data);

void
mcraob_( Fint4 *option, char *md1, char *md2,
         Fint4 *day, Fint4 *obtime, Fint4 *station,
         Fint4 *nlevs, Fint4 stndat[],
         Fint4 lev[], Freal4 pres[], Freal4 temp[],
         Freal4 td[], Freal4 dir[], Freal4 spd[], Freal4 z[], Fint4 *status,
         FsLen md1len, FsLen md2len);

Fint4
mcgget_( char *name, Fint4 *nsort, FstringArray sort,
         char *unit, char *form, Fint4 *maxbyte, Fint4 *msgflag,
         FsLen namelen, FsLen sortlen, FsLen unitlen, FsLen formlen);

Fint4
mcgrcv_( Fint4 grid[], Fint4 header[]);

Fint4
mcgrc2_( Fint4 grid[], Fint4 header[]);

Fint4
mcaname_( Fint4 *frame, char *name, char *pos, FsLen name_len, FsLen pos_len);

/*******************************************************************
 *              Category: CALIBRATION
 *                        (calibration, units conversion)
 *
 */


/*******************************************************************
 *              Category: CONVERTER
 *                        (parsing, byte movers, unit converter)
 *
 * Mcstrrepl()  - do McIDAS string replacement
 *
 * Mclocase()  - Convert a string to all lower case letters.
 *
 * Mcupcase()  - Convert a string to all upper case letters.
 *
 * Mcstricmp()	- Do a case-independent string comparison.
 *
 * Mcstrnicmp()	- Do a case-independent string comparison up to n bytes.
 *
 * Mcargcmd()   - Build and return a McIDAS command line for the given handle.
 *
 * Mcargdbl()   - Fetch an argument in double type format.
 *
 * Mcargdhr()   - Fetch an argument in double fractional hours format hh.fffff.
 *
 * Mcargdll()   - Fetch argument in double fractional lat/lon format ddd.fffff.
 *
 * Mcargdump()  - Display parsed arg-fetching to McIDAS debug destination.
 *
 * Mcargfree()  - Free parsed arg-fetching structure for the given handle.
 *
 * Mcargihr()   - Fetch an argument in integer type time format hhmmss.
 *
 * Mcargill()   - Fetch an argument in integer type lat/lon format dddmmss.
 *
 * Mcargint()   - Fetch an argument in integer type format.
 *
 * Mcargiyd()   - Fetch an argument in integer type date format yyyyddd.
 *
 * Mcargkey()   - Validate arg-fetching keywords, optionally printing errors.
 *
 * Mcargnam()   - Fetch all keyword names within parsed arg-fetching text.
 *
 * Mcargnum()   - Return # args for given keyword in parsed arg-fetching text.
 *
 * Mcargquo()   - Fetch the quote field string argument.
 *
 * Mcargstr()   - Fetch an argument in character form.
 *
 * Mcstrtodbl() - Convert given numeric token to double type format.
 *
 * Mcstrtodhr() - Convert given time token to double fractional hours hh.ffff
 *
 * Mcstrtodll() - Convert given token to double fractional lat/lon ddd.fffff.
 *
 * Mcstrtohex() - Convert given hexadecimal token to integer type format.
 *
 * Mcstrtohms() - Convert given time to integer hours, minutes and seconds.
 *
 * Mcstrtoihr() - Convert given time token to integer time format hhmmss.
 *
 * Mcstrtoill() - Convert given lat/lon token to integer type format dddmmss.
 *
 * Mcstrtoint() - Convert given numeric token to integer type format.
 *
 * Mcstrtoiyd() - Convert given date token to integer date format yyyyddd.
 *
 * M0GetDefaultArgStr() - given a configuration file, command name
 *                        a keyword name and a position number
 *                        this function returns a string containing
 *                        a new default value
 *
 * M0GetDefaultArgInt() - given a configuration file, command name
 *                        a keyword name and a position number
 *                        this function returns a integer containing
 *                        a new default value
 *
 * M0GetDefaultArgDbl() - given a configuration file, command name
 *                        a keyword name and a position number
 *                        this function returns a double containing
 *                        a new default value
 *
 * M0GetDefaultArgILL() - given a configuration file, command name
 *                        a keyword name and a position number
 *                        this function returns a integer containing
 *                        a new default latitude/longitude
 *
 * M0GetDefaultArgDLL() - given a configuration file, command name
 *                        a keyword name and a position number
 *                        this function returns a double containing
 *                        a new default latitude/longitude
 *
 * M0GetDefaultArgIHR() - given a configuration file, command name
 *                        a keyword name and a position number
 *                        this function returns a integer containing
 *                        a new default time value
 *
 * M0GetDefaultArgDHR() - given a configuration file, command name
 *                        a keyword name and a position number
 *                        this function returns a double containing
 *                        a new default time value
 *
 * M0GetDefaultArgIYD() - given a configuration file, command name
 *                        a keyword name and a position number
 *                        this function returns a integer containing
 *                        a new default julian value
 *
 * M0GetDefaultString()  - given a configuration file, command name
 *                         a keyword name this function returns a
 *                         string containing the entire default string
 *
 * M0GetDefaultKeywords() - given a configuration file and command name
 *                          this function returns a list of keywords
 *
 * M0GetDefaultKeyNumParms() - given a configuration file, command name
 *                             and a keyword name, return the number
 *                             of parameters
 *
 * M0GetListOfDefaultFiles()  - gets the list of default files
 *                              based on what was set in M0SetDefaults
 *                              file call.
 *
 * M0GetDefaultConfErrorString() - returns an error string based
 *                                 on an error code from the default
 *                                 configuration subsystem
 *
 * M0SetDefaultFiles() - sets the configuration files based on
 *                       environment variables and defaults
 *
 * M0FCharArrayToCStringArray() - convert a FORTRAN character array
 *                                to a C array of string pointers
 *
 * M0CStringArrayToFCharArray() - converts an array of character string
 *                                pointers to a FORTRAN array of
 *                                characters
 *
 * McUnitCvtDbl() - convert an array of doubles from one unit to another
 *
 * McUnitCvtFlt() - convert an array of floats from one unit to another
 */

extern int
McUnitCvtDbl (int, const char *, double *, const char *, double *, int);

extern int
McUnitCvtFlt (int, const char *, float *, const char *, float *, int);

extern int
Mcstrrepl(const char *source, char **dest);

extern void
Mclocase(char *string);

extern void
Mcupcase(char *string);

extern int
Mcstricmp(const char *s, const char *t);

extern int
Mcstrnicmp(const char *s, const char *t, size_t n);

extern char *
Mcargcmd(int arg_handle);

extern int
Mcargdbl(int arg_handle, const char *keyword, int position, double def, double min, double max, double *value, const char **arg);

extern int
Mcargdhr(int arg_handle, const char *keyword, int position, double def, double min, double max, double *value, const char **arg);

extern int
Mcargdll(int arg_handle, const char *keyword, int position, double def, double min, double max, double *value, const char **arg);

extern void
Mcargdump(int arg_handle);

extern int
Mcargfree(int arg_handle);

extern int
Mcargihr(int arg_handle, const char *keyword, int position, int def, int min, int max, int *value, const char **arg);

extern int
Mcargill(int arg_handle, const char *keyword, int position, int def, int min, int max, int *value, const char **arg);

extern int
Mcargint(int arg_handle, const char *keyword, int position, int def, int min, int max, int *value, const char **arg);

extern int
Mcargiyd(int arg_handle, const char *keyword, int position, int def, int min, int max, int *value, const char **arg);

extern int
Mcargkey(int arg_handle, int numkey, const char * const valid_keywords[], int printflag);

extern int
Mcargnam(int arg_handle, int maxkey, const char *keywords[]);

extern int
Mcargnum(int arg_handle, const char *keyword);

extern int
Mcargquo(int arg_handle, const char **value);

extern int
Mcargstr(int arg_handle, const char *keyword, int position, const char *def, const char **value);

extern int
Mcstrtodbl(const char *token, double *value);

extern int
Mcstrtodhr(const char *token, double *dhr);

extern int
Mcstrtodll(const char *token, double *dll);

extern int
Mcstrtohex(const char *token, int *value);

extern int
Mcstrtohms(const char *token, int *hour, int *min, int *sec);

extern int
Mcstrtoihr(const char *token, int *ihr);

extern int
Mcstrtoill(const char *token, int *ill);

extern int
Mcstrtoint(const char *token, int *value);

extern int
Mcstrtoiyd(const char *token, int *iyd);

extern int
M0GetDefaultArgStr ( char *, char *, int, char **);

extern int
M0GetDefaultArgInt ( char *, char *, int, int *);

extern int
M0GetDefaultArgDbl ( char *, char *, int, double *);

extern int
M0GetDefaultArgILL ( char *, char *, int, int *);

extern int
M0GetDefaultArgDLL ( char *, char *, int, double *);

extern int
M0GetDefaultArgIHR ( char *, char *, int, int *);

extern int
M0GetDefaultArgDHR ( char *, char *, int, double *);

extern int
M0GetDefaultArgIYD ( char *, char *, int, int *);

extern int
M0GetDefaultString ( char *, char *, char **);

extern int
M0GetDefaultKeywords ( char *, int *, char ***);

extern int
M0GetDefaultKeyNumParms ( char *, char *, int *);

extern int
M0GetListOfDefaultFiles ( char ***, int *);

extern int
M0GetDefaultConfErrorString ( int, char **);

extern int
M0SetDefaultFiles ( const char *, const char **, int);

extern int
M0FCharArrayToCStringArray ( char *, int, char ***, FsLen);

extern int
M0CStringArrayToFCharArray ( char **, int, char *, FsLen);

/*******************************************************************
 *              Category: DAY/TIME
 *                        (day, date, time converters, reformatting)
 *
 * Mcdhrtohms() - Convert double hours to hours, minutes and seconds.
 *
 * Mcdhrtoihr() - Convert double hours to integer hours.
 *
 * Mchmstoihr() - Convert time in hours, minutes and seconds to integer.
 *
 * Mcjulday() - Return Julian day for given calendar year, month and day.
 *
 * getday_() - Returns the current day (YYDDD format).
 *
 * gettim_() - Returns the current time (HHMMSS format).
 *
 * idmyyd_() - Converts dd/mm/yy format to Julian format.
 *
 * Mcgetdaytime() - get the current system day and time
 *
 * Mcgetday() - get the current system day in ccyyddd format
 *
 * Mcgettime() - get the current system time in hhmmss format
 *
 * mcincday() - increments a julian day
 *
 * Mcinctime() - increments a julian day and time by hhmmss increment
 *
 * Mcdaytimetosec() - returns the system's absolute seconds since
 *                    00:00:00 on 1 January 1970
 *
 * Mcsectodaytime() - converts number of seconds since 1 January 1970 to
 *                    day/time
 *
 * Mcdmytocyd() - converts day/month/year to ccyyddd
 *
 * Mccydtodmy() - converts ccyyddd to day/month/year
 *
 * Mccydtoyd () - converts ccyyddd to yyddd
 *
 * Mcydtocyd () - converts yyddd to ccyyddd
 *
 * Mccydtostr() - converts ccyyddd to specific output format
 *
 * Mccydtostrform() - converts ccyyddd user defined output format
 *
 * Mccydtodow() - converts ccyyddd day of the week
 *
 * Mciydtocyd - Converts area directory yyyddd format to ccyyddd format
 *
 * Mccydtoiyd - Converts ccyyddd format into area directory yyyddd format
 *
 * Mcgetday() - get the current system day in ccyyddd format
 *
 * Mcgetdaytime() - get the current system day and time
 *
 * Mcisleap - returns a value indicating if a year is a leap year.
 *
 * Mchmsok() - verifies that the hhmmss format is correct
 *
 * Mchmstostr() - converts hhmmss to specific output format
 *
 * Mccydok() - verifies that the ccyyddd format is correct
 *
 * sksecs_() - returns the system's absolute seconds since
 *                    00:00:00 on 1 January 1972
 */

extern int
Mcdhrtohms (double dhr, int *hours, int *minutes, int *seconds);

extern int
Mcdhrtoihr (double dhr, int *ihr);

extern int
Mchmstoihr (int hours, int minutes, int seconds, int *ihr);

extern int
Mcjulday(int year, int month, int day);

extern void
gettim_(Fint4 *itime);

extern void
gettim(long int *itime);

extern void
getday_(Fint4 *iday);

extern Fint
idmyyd_(Fint *, Fint *, Fint *);

extern int
Mcgetdaytime ( int *, int *);

extern int
Mcisleap (int  );

extern int
Mcgetday (int *);

extern int
Mcgettime ( int *);

extern int
Mcincday ( int, int, int *);

extern int
Mcinctime (int, int, int, int *, int *);

extern int
Mcdaytimetosec ( int, int, time_t *);

extern int
Mcsectodaytime ( time_t, int *, int *);

extern int
Mcdmytocyd ( int, int, int, int *);

extern int
Mccydtodmy ( int, int *, int *, int *);

extern int
Mccydtodow (int, int *);

extern int
Mciydtocyd (int yyyddd, int *ccyyddd);

extern int
Mccydtoiyd (int ccyyddd, int *yyyddd);

extern int
Mccydtostrform (int, const char *, char **);

extern int
Mccydtostr (int, int, char **);

extern int
Mchmstostr (int, int, char **);

extern int
Mchmstostrform (int, const char *, char **);

extern int
Mccydtoyd (int, int *);

extern int
Mcydtocyd (int, int *);

extern int
Mchmsok (int);

extern int
McIsTimeOfDay (int);

extern int
Mccydok (int);

extern Fint
sksecs_ (Fint *, Fint *);

/*****************************************************************
 *              Category: DISPLAY
 *                        (xwindows, enhancements, cursor/pointer)
 *
 */


/*******************************************************************
 *              Category: EVENT
 *                        (scheduler)
 *
 */


/*******************************************************************
 *              Category: FILE
 *                        (lw, general file i/o, database i/o)
 *
 * Mcread() - read file (byte oriented)
 *
 * Mcwrite() - write file (byte oriented)
 *
 * Mcremove() - remove a file
 *
 */

extern int
Mcread(const char *file, off_t start, size_t count, void *buf);

extern int
Mcwrite(const char *file, off_t start, size_t count, const void *buf);

extern int
Mcremove(const char *file);


/******************************************************************
 *              Category: GRAPHIC
 *                        (plot package, utilities, color levels)
 *
 * McPlotWind - Plots a wind flag.
 *
 * McPlotSkyWind - Plots sky cover symbol and wind flag.
 *
 * McPlotWindNavLL - Plot wind flag at given latitude/
 *			longitude coordinate.
 *
 * McPlotWindNav - Plot wind flag at given line/element.
 *
 * McPlotSkyWindNavLL - Plot wind flag at given latitude/
 *			longitude coordinate.
 *
 * McPlotSkyWindNav - Plot wind flag and WMO
 *			cover symbol at given line/element.
 *
 * McPlotWMOWeather - Plot Present Weather symbols from WMO code.
 *
 * McPlotWMOSky - Plot sky cover symbols from WMO code.
 *
 * McPlotWMOLow - Plot Low Cloud symbols from WMO code.
 *
 * McPlotWMOMid - Plot Middle Cloud symbols from WMO code.
 *
 * McPlotWMOHigh - Plot High Cloud symbols from WMO code.
 *
 * McPlotWMOPressureTendency - Plot Pressure Tendency symbols
 *		from WMO code.
 *
 * McPlotWMOCalm - Plot calm wind symbol.
 */

extern int
McPlotWind(int, int, int, int, int, double, double );

extern int
McPlotSkyWind(int, int, int, int, int, int, double, double);

extern int
McPlotWindNavLL(int, double, double, int, int, double, double);

extern int
McPlotWindNav(int, int, int, int, int, int, double, double);

extern int
McPlotSkyWindNavLL(int, double, double, int, int, int, double, double);

extern int
McPlotSkyWindNav(int frame, int line, int element, int size,
		int color, int hemisphere, int WMO_Sky,
		double direction, double speed);

extern int
McPlotWMOWeather(int, int, int, int, int);

extern int
McPlotWMOSky(int, int, int, int, int);

extern int
McPlotWMOLow(int, int, int, int, int);

extern int
McPlotWMOMid(int, int, int, int, int);

extern int
McPlotWMOHigh(int, int, int, int, int);

extern int
McPlotWMOPressureTendency(int, int, int, int, int);

extern int
McPlotWMOCalm(int, int, int, int);


/*******************************************************************
 *              Category: GRID
 *              (grid data, decode, store, list, retrieve, manipulate)
 *
 * The defines are the variables that sit inside gridparm.inc
 * Mcglast() - returns the number of the last grid in a grid file
 * mcglast() - returns the number of the last grid in a grid file
 * mcfndgrd() - returns first grid number matching sorting conditions
 * McSubSectGrid() - return a portion of a grid, and change the header
 * McGridLLRow - return row/col bounds in grid that enclose lat/lon bounds
 */

#define MAXGRIDPT (300000)
#define MAXGRIDNUM (999999)
#define MINGRIDNUM (1)
#define MAXGRIDFILE (999999)
#define MINGRIDFILE (1)
#define ILLGRIDFILE (-1)

extern int
Mcglast(char *grid_file);

extern Fint
mcglast_(char *grid_file, FsLen name_len);

extern Fint
mcfndgrd_ (Fint *, Fint *, Fint *, Fint *, Fint *, Fint *, Fint *, Fint *, Fint *, char *, Fint *, char *, Fint *, Fint *, Fint *, FsLen, FsLen);

extern int
McSubSectGrid(int *, int *, int *, int *, double *, double *, int, double *, double *, int, int);

extern int
McGridLLRowCol(int *, double *, double *, double *, double *, int *, int *, int *, int *);

/*******************************************************************
 *              Category: IMAGE
 *   (image/area data, ingest, store, retrieve, display, manipulate)
 *
 *   The defines are variables that sit inside areaparm.inc
 *   rpixel_()    - replicate elements of an array in place
 *
 *   McIsImageBandPresent - Determines if band is present in an image.
 *
 */

#define MAX_BAND (32)
#define MAX_AUXBLOCK_SIZE (10000)
#define MAXOPENAREAS (3)
#define NUMAREAOPTIONS (5)
#define MAXDFELEMENTS (22000)
#define MX_AREA_DIR_WORDS (64)
#define MAX_AREA_NUMBER (9999)

Fint
rpixel_(Fint *num, Fint *rep, Fint *siz, void *buf);

extern int
McIsImageBandPresent(int* image_directory, int band);


/*******************************************************************
 *              Category: INGEST/DECODE
 *                        (ingest and decode of data)
 *
 */


/*******************************************************************
 *              Category: MET/SCIENCE
 *                        (science algorithms, unit converter)
 * McHeatIndex()        - calculate heat index
 * McRelativeHumidity() - calculate relative humidity
 * McWindChill()        - calculate wind chll factor
 * McTheta		- calculate potential temperature
 * McTFromTheta		- compute temperature from theta and pressure
 * McPFromTheta		- compute pressure from theta and temperature
 * McStationPres	- compute station pressure
 * McHypsoP		- integrate hypsometric equation to get new p
 * McHypsoZ             - integrate hypsometric equation to get new z
 * McVirtTemp           - compute virtual temperature
 * McMixing		- compute mixing ratio
 * McThetae		- compute equivalent potential temperature
 * McThetaw		- compute wetbulb potential temperatyre
 * McWetBulb		- compute wetbulb
 * McTdFromMixing	- compute dewpoint from mixing ratio and pressure
 * McTempAtThetae	- c-callable version of mctasa
 * McLiftCLevel		- c-callable version of mclcl (computes LCL)
 * McSatVapor		- c-callable version of mcsatvap (vapor pressure)
 * McSatVaporIce	- c-callable version of mcsatvpi (vapor pres. over ice)
 * McLatentHeat		- c-callable version of mclatvap (computes latent heat)
 * McSndanl             - compute stability parameters from sounding data
 * McCape		- compute convective available potential energy
 * McHelic		- compute helicity
 * McDewpt		- compute dewpoint
 * McMeanD		- compute mean value of double array
 * McStdDevD		- compute standard deviation of double array
 * McMeanI		- compute mean value of integer array
 * McStdDevI		- compute standard deviation of integer array
 * McMeanF		- compute mean value of float array
 * McStdDevF		- compute standard deviation of float array
 * M0xydrv		- compute grid of derivatives in x- or y- directions
 * M02omega		- return twice angular velocity of planet
 * Mccorfor		- compute coriolis force
 * Mcbeta		- compute beta parameter
 * Mccorforgrd		- compute a grid of coriolis force
 * Mcbetagrd		- compute a grid of beta parameter
 * M0meridgrd		- compute grid of meridional term
 * M0merid		- compute meridional term due to converging meridians
 * Mcdiver		- compute grid of divergence given u and v grids
 * Mcvort		- compute grid of vorticity given u and v grids
 * Mcstretd		- compute grid of stretching deformation given u,v grids
 * Mcsheard		- compute grid of shearing deformation given u,v grids
 */

extern int
McHeatIndex(double , double , const char *,double *);

extern int
McRelativeHumidity(double , double , const char *, double *);

extern int
McWindChill(double , double , const char *, const char *, double *);

extern int
McTheta(double,  double, double *);

extern int
McTFromTheta(double, double, double *);

extern int
McPFromTheta(double, double, double *);

extern double
McLatentHeat(double);

extern double
McSatVaporIce(double);

extern double
McSatVapor(double);

extern int
McLiftCLevel(double, double, double, double *, double *);

extern double
McTempAtThetae(double, double);

extern double
McTDFromMixing(double, double);

extern double
McMixing(double, double);

extern int
McStationPres(double, double, double, double *);

extern int
McHypsoP(double, double, double, double *, double, double);

extern int
McHypsoZ(double, double, double, double, double *, double);

extern int
McVirtTemp(double, double, double, double *);

extern int
McThetae(double *, double *, double *, double *);

extern int
McThetaw(double *, double *, double *, double *);

extern int
McWetBulb(double, double, double, double *);

extern int
McSndAnl ( int, double *, double *, double *, double *,
 		double *, double *, double *, int, int);

extern int
McCape (int , double *, double *, double *, double *, double *, double *,
   double *, double *, double *);

extern int
McHelic (double *, double *, double *, int, double *);

extern int
McDewpt (double , double , double * );

extern int
McMeanD ( double *, int, double *, int, double *);

extern int
McStdDevD( double *, int *, double *, int *, double *);

extern int
McMeanF ( float *, int, float *, int, double *);

extern int
McStdDevF( float *, int *, float *, int *, double *);

extern int
McMeanI ( int *, int, int *, int, double *);

extern int
McStdDevI( int *, int *, int *, int *, double *);

extern int
M0xydrv(int *, int *, int *, int *, int, int, int, char *, int);

extern int
Mccorforgrd(int *, int *, int *, int *, char *);

extern double
Mcbeta(double, char *);

extern double
Mccorfor(double, char *);

extern double
M02omega(char *);

extern int
Mcbetagrd(int *, int *, int *, int *, char *);

extern int
M0meridgrd(int *, int *, int *, int *, char *);

extern double
M0merid(double, double, double);

extern int
Mcdiver(int *, int *, int *, int *, int *, int *, char *, int);

extern int
Mcvort(int *, int *, int *, int *, int *, int *, int, char *, int);

extern int
Mcstretd(int *, int *, int *, int *, int *, int *, char *, int);

extern int
Mcsheard(int *, int *, int *, int *, int *, int *, char *, int);

/*******************************************************************
 *              Category: NAVIGATION
 *                        (lat/lon, stations)
 *
 */


/*******************************************************************
 *              Category: PT_SRC
 * (point source/md files data, decode, store, retr ieve, plot, manipulate)
 *  the #defines in here are from ptparm.inc
 *
 * McPtGet()     - c callable version of m0ptget()
 *
 * McPtBufInit() - Initialize point buffer data extraction
 *
 * McPtBufInt()  - extract an int value from a point buffer
 *
 * McPtBufDbl()  - extract a double value from a point buffer
 *
 * McPtBufStr()  - extract a string value from a point buffer
 *
 * McPtRead()    - c callable version of m0ptrd()
 *
 */

#define MINMDFILENO (1)
#define MAXMDFILENO (999999)
#define MAXSORTNUM (256)
#define MAXNUMPARM (402)
#define HEADSIZE (64)
#define BYTESPERPARM (4)
#define MAXPTNAMLEN (64)
#define MAXPTREQLEN (500)
#define MAXSORTLEN (256)

extern int
McPtGet         (const char *, int, char **, int *, char **, char **,
                 char **, int *, int);

extern int
McPtBufInit     (int, char **, int *);

extern int
McPtBufInt      (int, void *, int *);

extern int
McPtBufDbl      (int, void *, double *);

extern int
McPtBufStr      (int, void *, char **);

extern int
McPtRead        (void *);

/*******************************************************************
 *              Category: SYS_CONFIG
 *                        (uc, syskey, mcpath, itrmch)
 *
 * Mcdev2uc()   - Convert given DEV= character value to its numeric uc value.
 *
 * Mcfsize()    - get size of a frame
 *
 * Mcluc()      - Lookup user common for given index.
 *
 * Mcpuc()      - Poke user common for given index.
 *
 * Mcuc2dev()   - Convert given DEV= numeric uc value to its character value.
 *
 */

extern int
Mcdev2uc(char dev);

extern int
Mcfsize(int frame, int *lines, int *elements);

extern Fint
Mcluc(int lindex);

extern void
Mcpuc(Fint  value, int pindex);

extern char
Mcuc2dev(int dev);


/*******************************************************************
 *              Category: SYSTEM
 *                        (communications, servers, OS interfaces)
 *
 * Mcinit()	- initialize McIDAS environment
 *
 * Mciniterr()	- describe Mcinit() error
 *
 * Mckeyin()	- execute a keyin asynchronously
 *
 * Mcskeyin()	- execute a keyin synchronously
 *
 * Mccodeget()	- fetch exit code
 *
 * Mccodeset()	- set exit code
 *
 * Mctrace ()   - provides tracing with priority flag
 *
 * abort_()     - McIDAS abort routine, terminates the execution of a routine.
 *
 * M0IsTraceSet() - sets the trace flag according to the value sent
 *
 * M0sxread() - read data sent by the client to the server
 *
 * M0sxsend() - send data to the client from the server
 *
 * M0sxtrce() - send a string of debug information to the trace file
 *
 * M0split() - split dataset name and lookup alias
 *
 * M0sxdone() - bind off a ADDE tranaction after all data has been sent
 *
 * M0cxread() - read data sent by the server to the client
 *
 * M0cxreq() - send a request for data to the server
 */

extern int
Mcinit(int argc, char **argv);

extern const char *
Mciniterr(void);

extern int
Mckeyin(const char *command);

extern int
Mcskeyin(const char *command);

extern int
Mccodeget(void);

extern void
Mccodeset(int);

extern void
Mctrace (int, const char *, const char *);

extern void
abort_(const Fint *icode);

extern int
M0IsTraceSet ( char *);

extern int
M0sxread ( int, void *);

extern int
M0sxsend ( int, void *);

extern int
M0sxtrce (char *);

extern int
M0split ( char *, char **);

extern int
M0sxdone (servacct *);

extern int
M0cxread ( int, void *);

extern int
M0cxreq ( char *, char *, int, int *, int *);


/*****************************************************************
 *              Category: TEXT
 *                        (textual data, ingest, list, retrieve, manipulate)
 *
 * M0PrintTextFromServer() - prints text from an ascii file sent by
 *                         the adde server
 * M0textgt() - send out a request for ascii text data
 *
 * M0txtread() - read an ascii text request from the server
 *
 * M0wtxget() - send out a request for weather text data
 *
 * M0wtxread() - read a weather text request from the server
 *
 * M0wtxgsort() - retrieves the general purpose weather text
 *
 */

extern int
M0PrintTextFromServer ( char *, int, char **, int);

extern int
M0textgt ( char *, int, char **, int);

extern int
M0txtread ( char *, int);

extern int
M0wtxget ( char *, int, char **, int);

extern int
M0wtxread ( int *, char **);

extern int
M0wtxgsort ( char *, int *, char ***);


/**************************************************************
 *              Category: USER_INTERFACE
 *                        (menus, messages , strings, command_line)
 *
 * Mcdprintf()  - Print to McIDAS debug destination.
 *
 * Mceprintf()  - Print to McIDAS error destination.
 *
 * Mcprintf()   - Print to McIDAS standard destination.
 *
 * Mccmd()      - Build and return the current McIDAS command line.
 *
 * Mccmddbl()   - Fetch a program command line argument in double type format.
 *                Prints diagnostics to edest device for standard errors.
 *
 * Mccmddhr()   - Fetch a program argument in fractional hours format hh.fffff.
 *                Prints diagnostics to edest device for standard errors.
 *
 * Mccmddll()   - Fetch a program argument in fractional lat/lon form dd.fffff.
 *                Prints diagnostics to edest device for standard errors.
 *
 * Mccmdihr()   - Fetch a program argument in integer type time format hhmmss.
 *                Prints diagnostics to edest device for standard errors.
 *
 * Mccmdill()   - Fetch a program argument in integer type lat/lon form ddmmss.
 *                Prints diagnostics to edest device for standard errors.
 *
 * Mccmdint()   - Fetch a program command line argument in integer type format.
 *                Prints diagnostics to edest device for standard errors.
 *
 * Mccmdiyd()   - Fetch a program argument in integer type date format yyyyddd.
 *                Prints diagnostics to edest device for standard errors.
 *
 * Mccmdkey()   - Validate command line keywords, printing errors to edest.
 *
 * Mccmdnam()   - Fetch all keyword names occurring in the command line.
 *
 * Mccmdnum()   - Return # values associated with given command line keyword.
 *
 * Mccmdquo()   - Fetch the quote field string command line argument.
 *
 * Mccmdstr()   - Fetch a program command line argument in character form.
 *
 */

extern void
Mcdprintf(const char *format, ...);

extern void
Mceprintf(const char *format, ...);

extern void
Mcprintf(const char *format, ...);

extern char *
Mccmd(void);

extern int
Mccmddbl(const char *keyword, int position, const char *printmsg, double def, double min, double max, double *value);

extern int
Mccmddhr(const char *keyword, int position, const char *printmsg, double def, double min, double max, double *value);

extern int
Mccmddll(const char *keyword, int position, const char *printmsg, double def, double min, double max, double *value);

extern int
Mccmdihr(const char *keyword, int position, const char *printmsg, int def, int min, int max, int *value);

extern int
Mccmdill(const char *keyword, int position, const char *printmsg, int def, int min, int max, int *value);

extern int
Mccmdint(const char *keyword, int position, const char *printmsg, int def, int min, int max, int *value);

extern int
Mccmdiyd(const char *keyword, int position, const char *printmsg, int def, int min, int max, int *value);

extern int
Mccmdkey(int numkey, const char * const valid_keywords[]);

extern int
Mccmdnam(int maxkey, const char *keywords[]);

extern int
Mccmdnum(const char *keyword);

extern int
Mccmdquo(const char **value);

extern int
Mccmdstr(const char *keyword, int position, const char *def, const char **value);


/*****************************************************************n
 *              Category: UTILITY
 * (functions useful for programming, substringing, byte manipulation, etc)
 *
 * Mcarrtofs( ) - Copy array of C strings to given 2-dimensional char array,
 *                left-justified, blank padded, and non-null-terminated.
 *
 * Mcfreearr()  - Free the given array, including all of the array's pointers.
 *
 * Mcfreestrs() - Free the given array's pointers, without freeing the array.
 *
 * Mcfstoarr()  - Create C string array from a given 2-dimensional char array
 *                that is left-justified, blank-padded, non-null-terminated.
 *
 * Mcitonth()   - Format the given number into string of "nth".
 *
 * McNint()	- return nearest integer for given double value
 *
 * Mcstrtofs( ) - Copy C string to fixed length, blank-padded, char block.
 *
 * VecNew() - Initialize arg vector for vector of pointers to strings.
 *
 * VecAdd() - Add a char* to the end of the given vector.
 *
 * VecOld() - Delete the vector and free allocated space.
 *
 * VecLen() - Returns number of pointers in the vector.
 */

extern int
Mcarrtofs(const char * const *str, int nstr, int nele, int el_size, char *arr);

extern void
Mcfreearr(char **str, int nstr);

extern void
Mcfreestrs(char **str, int nstr);

extern char**
Mcfstoarr(const char *arr, int nele,int el_size);

extern char*
Mcitonth(int i);

extern int
McNint(double);

extern int
Mcstrtofs(char *mem, const char *str, int siz_mem);

extern char **
VecNew(void);

extern char **
VecAdd(char **vec, const char *s);

extern int
VecOld(char **vec);

extern int
VecLen(char **vec);

extern Fint
lwfile_ (const char *, FsLen);

extern void
McInitGraphics (int, int);

extern void
McSetGraphicsPage (int, int, int, int, int);

extern void
McDrawLine (int, int, int);

extern int
McSetDashAttributes (int, int, int);

extern void
McGetDashAttributes (int *, int *, int *);

extern int
McGetGraphicWidth (void);

extern int
McDrawText (char *, int, int, int, int, int, char *, char *, int,
int);

extern void
McDrawLineSegments (int, int *, int *, int);

extern int
McInsidePolygon (int, int, int, int *, int *);

extern void
McFillPolygon (int, int *, int *, int);

extern int
McGetDefaultMapString (const char *, const char *, char **);

extern int
McGetMapMercatorNav (const char *, float *, float *, float *,
float *);

extern int
McGetMapPolarStereoNav (const char *, float *, float *, float *,
float *,
float *);

extern Fint
mcprtblk_ (char *, Fint *, Fint *, Fint *, Fint *, FsLen);

extern int
Mctermch (const char *);

extern int
McGetImageFrameNumberI (void);

extern int
McGetImageFrameNumber (void);

extern int
McGetGraphicsFrameNumber (void);

extern int
McSetGraphicsFrameNumber (int);

extern int
McGetMaxImageFrameNumber (void);

extern int
McGetMaxGraphicsFrameNumber (void);

extern int
McIsImageLooping (void);

extern int
McIsImageFrameOn (void);

extern void
McSetImageFrameOn (void);

extern int
McIsImageConnectedToLoop (void);

extern int
McIsGraphicsLooping (void);

extern int
McIsGraphicsFrameOn (void);

extern void
McSetGraphicsFrameOn (void);

extern int
McIsGraphicsConnectedToLoop (void);

extern int
McGetStdOutputDevice (void);

extern int
McGetStdErrorDevice (void);

extern int
McGetStdDebugDevice (void);

extern int
McSetStdOutputDevice (int);

extern int
McSetStdErrorDevice (int);

extern int
McSetStdDebugDevice (int);

extern Fint
isdgch_ (const char *, FsLen);

extern void
scloff_ (void);

extern void
sclon_ (void);

extern void
enpt_ (void);

extern void
endplt_ (void);

extern int
M0GetIdsForOBSCmd (int *, char ***, int *, char ***, int *, char
***, int *);

extern int McSetImageFrameNumber (int);

extern void
McHatchPolygon (int, int *, int *, int, int, int);

extern void
McDitherPolygon (int, int *, int *, int, int);

extern int
McGetGraphicsFrameNumberI (void);

extern int
McIsMcIDASRunning (void);
extern void
m0postmessage_(char *, char *, char * , FsLen , FsLen , FsLen );
extern void
M0postmessage(char *, char *, char *);

extern Fint
m0waitmessage_(char *, char *, char *, Fint *, char *, FsLen , FsLen , FsLen , FsLen ) ;
extern int
M0waitmessage(char *, char *, char *, int , char *) ;

extern Fint
m0errormessage_(char *, Fint *, char *, FsLen ,  FsLen ) ;
extern int
M0errormessage(char *, int , char *) ;

extern Fint
m0warningmessage_(char *, Fint *, char *, FsLen ,  FsLen ) ;
extern int
M0warningmessage(char *, int , char *);

extern Fint
m0infomessage_(char *, Fint *, char *, FsLen ,  FsLen ) ;
extern int
M0infomessage(char *, int , char *) ;

extern int
M0rtgfil (int, int *, int, int *, int, int *, int, char **, int *, int **);

extern void
M0cydrange(int *, int *);

extern int
M0display_to_earth(int, int, int,
int *, int *, double *, double *, int *);

extern int
M0earth_to_display(int, int, double, double,
int *, int *, int *, int *);

extern int
M0frame_res(int, int, int, double,
double *, double *, double *, double *);

extern int
M0GetPatternFileList(int, const char *, int *, char ***);

extern void
M0gtcircle_point(double, double, double, double, double,
double *, double *);

extern void
M0gtcircle_dist(double, double, double, double, double,
double *, double *);

extern double
Mcd2r(void);

extern double
Mclonrange(double);

extern double
Mcpi(void);

extern int
Mcyear(int, int *);

extern int
McFrameLatLon(int, double *, double *, double *, double * );

extern int
McFindLLFromStateNum (int, char **, int *, int *,
double, double, double, double);

extern int
McGetLineSegs (double *, double *, int *);

extern int
McGetStateFipsFromPO (char *, int *, char **);

extern int
McInitGetLL (void);

extern int
McGetCountyNameFromFips (int, int, char **);

extern int
McGetCountyFipsFromName (char *, int, char *, int *, int *);

extern int
McGetCursorSize ( int *, int *);

#endif  /* _MCIDAS_H  */

