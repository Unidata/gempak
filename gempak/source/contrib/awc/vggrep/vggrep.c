#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "drwids.h"

int obj_match ( VG_DBStruct *el,
		int vg_class, int vg_type, int obj_type );
int DoIt ( char *in_file, int vg_class, int vg_type,
	   int obj_type, int *match );

void Usage ( void );

/************************************************************************
 * VGGREP                                                               *
 *                                                                      *
 * This program 'grep's a VGF file for objects of a given type          *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S.W.Danz/AWC		 8/98	Created					*
 * S. Jacobs/NCEP	11/01	Updated to v5.6.f			*
 ***********************************************************************/

int
obj_match(
    VG_DBStruct *el,
    int     vg_class,
    int     vg_type,
    int     obj_type
) {
    int status;

    if (obj_type == -1) {
        status = 1;
    } else {
        switch ( el->hdr.vg_class ) {
            case  CLASS_FRONTS:
                status = (el->elem.frt.info.fcode == obj_type);
            break;
            
            case CLASS_WATCHES:
                status = (el->elem.wbx.info.w_type == obj_type);
            break;

            case CLASS_LINES:
                switch (el->hdr.vg_type) {
                    case SPLN_ELM:
                        status = (el->elem.spl.info.spltyp == obj_type);
                    break;

                    case LINE_ELM:
                        status = (el->elem.lin.info.lintyp == obj_type);
                    break;
                }
            break;

            case CLASS_SYMBOLS:
                status = (el->elem.sym.data.code[0] == obj_type);
            break;

            case CLASS_TEXT:
                switch (el->hdr.vg_type) {
                    case TEXT_ELM:
                    case TEXTC_ELM:
                        status = 1;
                    break;

                    case SPTX_ELM:
                        status = (el->elem.spt.info.sptxtyp == obj_type);
                    break;
                }
            break;

            case CLASS_WINDS:
                status = (el->elem.wnd.info.wndtyp  == obj_type);
            break;

            case CLASS_ANY:
                status = 1;
            break;

            case CLASS_COMSYM:
                status = 1;
            break;

            case CLASS_PRODUCTS:
                status = 1;
            break;

            case CLASS_TRACKS:
                status = 1;
            break;

            case CLASS_CIRCLE:
                status = 1;
            break;
        }
    }

    status = (status && 
                ((vg_class == -1) || (el->hdr.vg_class == vg_class)) &&
                ((vg_type == -1) || (el->hdr.vg_type == vg_type)));

    return status;
}

int
DoIt(
    char    *in_file,
    int     vg_class,
    int     vg_type,
    int     obj_type,
    int     *match
) {
    int         iret;
    int         readpos;
    long        in_size;
    char        tmp_name[PATH_MAX];
    VG_DBStruct el;

    readpos = 0;
    *match  = 0;
    cfl_inqr(in_file, NULL, &in_size, tmp_name, &iret);
    while (!*match && !iret && (readpos < in_size)) {
        cvg_rdrec(in_file, readpos, &el, &iret);
        if (!iret) {
            readpos += el.hdr.recsz;
            *match = obj_match(&el, vg_class, vg_type, obj_type);
        }
    }

    return iret;
}

void
Usage( void
) {
    printf("Usage: vggrep -o vg_class:vg_type:obj_type file [file...] ...\n" );
    printf("       -o : Object to find.  The : are required, however a\n");
    printf("            blank field matches all objects.\n" );
    printf("\n");
}


int main ( int argc, char *argv[] )
{
    int     opt = 0;
    int     vg_class;
    int     vg_type;
    int     obj_type;
    int     action;
    int     result;
    int     match;
    char    *arg;
    char    *sep;

    action = 0;
    vg_class = vg_type = obj_type = -1;                
    optind = 1;
    while (optind < argc) {
        opt = getopt(argc, argv, ":o:h");
        switch (opt) {
            case 'o':
                vg_class = vg_type = obj_type = -1;                
                arg = optarg;

                sep = strchr(arg, ':');
                if (sep) *sep = '\0';
                sscanf(arg, "%d", &vg_class);
                if (sep) {
                    arg = sep+1;
                    sep = strchr(arg, ':');
                    if (sep) *sep = '\0';
                    sscanf(arg, "%d", &vg_type);
                }
                if (sep) {
                    arg = sep+1;
                    sscanf(arg, "%d", &obj_type);
                }
            break;

            case ':':
                fprintf(stderr, "ERROR: Missing argument for -%c\n", optopt);
                Usage();
                exit (-1);
            break;

            case 'h':
                Usage();
                exit (-1);
            break;

            default:
                action = 1;
                result = DoIt(
                        argv[optind],
                        vg_class,
                        vg_type,
                        obj_type,
                        &match
                    );
                if (result) {
                    fprintf(stderr, 
                        "ERROR: Received GEMPAK error %d processing %s, exiting...\n", 
                        result, argv[optind]
                    );

                    exit (-1);
                }
                printf("%d\n", match);
               optind++;

            break;
        }
    }

    if (!action) {
        Usage();
    }

    return(0);
}
