#include "geminc.h"
#include "gemprm.h"

void dump_objects(CMDObjectSet objects, PlacementSet placements);
void err_msg(const char *format, ...);
char *parse_delete(CMDObjectSet objects, PlacementSet placing, const char *delims);
void parse_input(char *infile, CMDObjectSet objects, PlacementSet placing);
char *parse_object(CMDObjectSet objects, PlacementSet placing, int polygon,
        const char *delims);
char *parse_vertex(const char *delims, float *x, float *y);
void print_object(PlacementSet placements, CMDObject object);
void print_place(Placement place);
void usage(void);


void err_msg(const char *format, ...)
/*****************************************************************************
 * err_msg
 *  Send error message to stderr
 *
 * Input parameters:
 *  const char  *format     Format string for the message
 *              ...         va_list of parameter for the message
 *
 * Output parameters:
 *  None
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    va_list ap;
/*---------------------------------------------------------------------*/

    va_start(ap, format);
    vfprintf(stderr, format, ap);
    va_end(ap);
}


void print_place(Placement place)
/*****************************************************************************
 * print_place
 *  Print information from the given Placement object to stdout
 *
 * Input parameters:
 *  Placement   place       Placement object to print
 *
 * Output parameters:
 *  None
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    Handle          id, reference;
    PlacementMode   mode;
    float           arrow_x[2], arrow_y[2], dist_incr, dist_offset, offset_x;
    float           offset_y;
    int             allow_center, in_center, both_sides_of_line, max_attempts;
    int             iret, placed;
/*---------------------------------------------------------------------*/

    if (place) {
        cap_plgetid(place, &id, &iret);
        cap_plgetref(place, &reference, &iret);
        cap_plgetcenter(place, &allow_center, &iret);
        cap_plgetincntr(place, &in_center, &iret);
        cap_plgetsides(place, &both_sides_of_line, &iret);
        cap_plgettries(place, &max_attempts, &iret);
        cap_plgetdistincr(place, &dist_incr, &iret);
        cap_plgetdistoffset(place, &dist_offset, &iret);
        cap_plgetmode(place, &mode, &iret);
        cap_plgetplaced(place, &placed, &iret);
        cap_plgetoffset(place, &offset_x, &offset_y, &iret);
        cap_plgetline(place, arrow_x, arrow_y, &iret);

        printf("\tID to place: %d\n", id);
        printf("\tReference ID: %d\n", reference);
        printf("\tCenter flag: %d\n", allow_center);
        printf("\tPlaced in center: %d\n", in_center);
        printf("\tLine side flag: %d\n", both_sides_of_line);
        printf("\tMax attempts: %d\n", max_attempts);
        printf("\tDistance incr: %.4f\n", dist_incr);
        printf("\tDistance offset: %.4f\n", dist_offset);
        switch (mode) {
            case IMMEDIATE:
                printf("\tPlacement mode: immediate\n");
            break;

            case ONESHOT:
                printf("\tPlacement mode: one-shot\n");
            break;

            case DELAYED:
                printf("\tPlacement mode: delayed\n");
            break;

            default:
                printf("\tPlacement mode: undefined\n");
            break;
        }
        printf("\tPlaced indicator: %d\n", placed);
        printf("\tOffset: (%.4f,%.4f)\n", offset_x, offset_y);
        printf("\tArrow endpoints: from (%.4f,%.4f) to (%.4f,%.4f)\n",
                arrow_x[0],
                arrow_y[0],
                arrow_x[1],
                arrow_y[1]
            );
    } else {
        printf("\tNothing given to print\n");
    }
}

/*======================================================================*/

char *parse_vertex(const char *delims, float *x, float *y)
/*****************************************************************************
 * parse_vertex
 *  Parse vertex information from stdin
 *
 * Input parameters:
 *  const char  *delims     Delimeter between strings
 *
 * Output parameters:
 *  float       *x          Array for X coordinates
 *  float       *y          Array for Y coordinates
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    char    *token;
    int     set;
    float   f;
/*---------------------------------------------------------------------*/

    set = 0;
    while ((token = strtok(NULL, delims)) && set < 2) {
        if (set == 0) {
            sscanf(token, "%f", &f);
            *x = f;
            set = 1;
        } else if (set == 1) {
            sscanf(token, "%f", &f);
            *y = f;
            set = 2;
        } else {
            break;
        }
    }

    return token;
}

/*======================================================================*/

char *parse_object(CMDObjectSet objects, PlacementSet placing, int polygon,
        const char *delims)
/*****************************************************************************
 * parse_object
 *  Parse object information from stdin and insert the resulting object
 *  into the object set.  If the object is marked to be placed include it in
 *  the placement set.
 *
 * Input parameters:
 *  CMDObjectSet    objects     Set to add object to
 *  PlacementSet    placing     Set to add object to for placement
 *  int             polygon     Flag, 1 = object is a polygon, 0 object is a 
 *                              line
 *  const char      *delims     Delimeter between strings
 *
 * Output parameters:
 *  None
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    static float    x[256];
    static float    y[256];

    Handle          ref, id;
    PlacementMode   pmode;
    char            *token;
    float           dist, offset;
    int             iret, points, placecenter, pointcenter, sides , max_tries;
/*---------------------------------------------------------------------*/

    points = 0;
    placecenter = 0;
    pointcenter = 0;
    sides  = 0;
    max_tries = 4;
    dist = 0;
    offset = 0;
    pmode = IMMEDIATE;
    ref = -1;
    id = -1;
    token = strtok(NULL, delims);
    while (token) {
        if (strcmp(token, "vertex") == 0) {
            token = parse_vertex(delims, &(x[points]), &(y[points]));
            points++;
        } else if (strcmp(token, "pointcenter") == 0) {
            token = strtok(NULL, delims);
            pointcenter = atoi(token);
            token = strtok(NULL, delims);
        } else if (strcmp(token, "dist") == 0) {
            token = strtok(NULL, delims);
            dist = atof(token);
            token = strtok(NULL, delims);
        } else if (strcmp(token, "offset") == 0) {
            token = strtok(NULL, delims);
            offset = atof(token);
            token = strtok(NULL, delims);
        } else if (strcmp(token, "tries") == 0) {
            token = strtok(NULL, delims);
            max_tries = atoi(token);
            token = strtok(NULL, delims);
        } else if (strcmp(token, "sides") == 0) {
            token = strtok(NULL, delims);
            sides = atoi(token);
            token = strtok(NULL, delims);
        } else if (strcmp(token, "placecenter") == 0) {
            token = strtok(NULL, delims);
            placecenter = atoi(token);
            token = strtok(NULL, delims);
        } else if (strcmp(token, "ref") == 0) {
            token = strtok(NULL, delims);
            ref = atoi(token);
            token = strtok(NULL, delims);
        } else if (strcmp(token, "id") == 0) {
            token = strtok(NULL, delims);
            id = atoi(token);
            token = strtok(NULL, delims);
        } else {
            break;
        }
    }

    if (polygon) {
        if ((x[0] != x[points-1]) || (y[0] != y[points-1])) {
            x[points] = x[0];
            y[points] = y[0];
            points++;
        }
    }

    if (id != -1) {
        cmd_osaddob(objects, id, x, y, points, &iret);
        if (iret == 0) {
            if (ref != -1) {
                cap_psaddpl(placing, id, ref, placecenter, sides, max_tries, 
                        dist, offset, pmode, pointcenter, &iret);
            }
        } else {
            fprintf(stderr, "Problem adding an object, status: %d\n", iret);
        }
    } else {
        fprintf(stderr, "OOPS, have an object without an id (was it defined before the type?)\n");
    }

    return token;
}

/*======================================================================*/

char *parse_delete(CMDObjectSet objects, PlacementSet placing, const char *delims)
/*****************************************************************************
 * parse_delete
 *  Parse the info from stdin related to deleting an element from the object
 *  set and placement set.
 *
 * Input parameters:
 *  CMDObjectSet    objects     Set of objects to delete from
 *  PlacementSet    placing     Set of placement objects to delete from
 *  const char      *delims     Delimeter between strings
 *
 * Output parameters:
 *  None
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    char    *token;
    int     id, iret;
/*---------------------------------------------------------------------*/

    token = strtok(NULL, delims);
    while (token) {
        if (strcmp(token, "id") == 0) {
            token = strtok(NULL, delims);
            id = atoi(token);
            cmd_osdelob(objects, id, &iret);
            cap_psdelpl(placing, id, &iret);
            token = strtok(NULL, delims);
        } else {
            break;
        }
    }

    return token;
}

/*======================================================================*/

void parse_input(char *infile, CMDObjectSet objects, PlacementSet placing)
/*****************************************************************************
 * parse_input
 *  Parse information from stdin a manipulate the object set and placement
 *  set given.
 *
 * Input parameters:
 *  char            *infile         Name of input file
 *  CMDObjectSet    objects         Set of objects to manipulate
 *  PlacementSet    placing         Set of placement objects to manipulate
 *
 * Output parameters:
 *  None
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    FILE        *in;
    char        *data, *token;
    const char  delims[] = "{} \n";
    float       dist, incr, xmin, xmax, ymin, ymax;
    int         result;
    struct stat statbuf;
/*---------------------------------------------------------------------*/

    in = fopen(infile, "r");
    if (in) {
        stat(infile,&statbuf);
        data = (char *)malloc((statbuf.st_size+1) * sizeof(char));
        if (data) {
            result = fread(data, sizeof(char), statbuf.st_size, in);
            fclose(in);
            if (result == statbuf.st_size) {
                token = strtok(data, delims);
                while (token) {
                    if (strcmp(token,"type") == 0) {
                        token = strtok(NULL, delims);
                    }

                    if (strcmp(token,"polygon") == 0) {
                        token = parse_object(objects, placing, 1, delims);
                    } else if (strcmp(token,"line") == 0) {
                        token = parse_object(objects, placing, 0, delims);
                    } else if (strcmp(token,"delete") == 0) {
                        token = parse_delete(objects, placing, delims);
                    } else if (strcmp(token,"dist") == 0) {
                        token = strtok(NULL, delims);
                        sscanf(token, "%f", &dist);
                        cap_pssetdist(placing, dist, &result);
                    } else if (strcmp(token,"incr") == 0) {
                        token = strtok(NULL, delims);
                        sscanf(token, "%f", &incr);
                        cap_pssetincr(placing, incr, &result);
                    } else if (strcmp(token,"area") == 0) {
                        token = strtok(NULL, delims);
                        sscanf(token, "%f", &xmin);
                        token = strtok(NULL, delims);
                        sscanf(token, "%f", &xmax);
                        token = strtok(NULL, delims);
                        sscanf(token, "%f", &ymin);
                        token = strtok(NULL, delims);
                        sscanf(token, "%f", &ymax);
                        cap_pssetarea(placing, xmin, xmax, ymin, ymax, &result);
                        cmd_ossetarea(objects, xmin, xmax, ymin, ymax, &result);
                    } else {
                        fprintf(stderr, "What is >%s<?\n", token);
                    }

                    token = strtok(NULL, delims);
                }
                free(data);
            } else {
                err_msg("Could not read entire input file\n");
            }
        } else {
            err_msg("Could not allocate space to read data set\n");
        }
    } else {
        err_msg("Could not open %s\n", infile);
    }
}

/*======================================================================*/

void print_object(PlacementSet placements, CMDObject object)
/*****************************************************************************
 * print_object
 *  Print the information on the given meta-data object to stdout
 *
 * Input parameters:
 *  PlacementSet    placements      Set of placement data in case object
 *                                  was placed and that info is needed
 *  CMDObject       object          CMDObject to be printed
 *
 * Output parameters:
 *  None
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    Handle      id, ref;
    Placement   place;
    const float *xpts, *ypts;
    float       arrow_x[2], arrow_y[2], cntr_x, cntr_y, offset_x, offset_y;
    int         in_center, iret, ispoly, placed, points, vertex;
/*---------------------------------------------------------------------*/

    if (object) {
        cmd_obgetid(object, &id, &iret);
        cmd_obgetpoints(object, &xpts, &ypts, &points, &iret);
        cmd_obgetcntr(object, &cntr_x, &cntr_y, &iret);
        cmd_obispoly(object, &ispoly, &iret);
        if (ispoly) {
            printf("{\n\t{type polygon}\n");
        } else {
            printf("{\n\t{type line}\n");
        }
        printf("\t{id %d}\n", id);

        cap_psgetpl(placements, id, &place, &iret);
        placed = 0;
        in_center = 0;
        offset_x = 0;
        offset_y = 0;
        if (place) {
            cap_plgetref(place, &ref, &iret);
            printf("\t{ref %d}\n", ref);
            cap_plgetplaced(place, &placed, &iret);
            if (placed) {
                cap_plgetincntr(place, &in_center, &iret);
                cap_plgetoffset(place, &offset_x, &offset_y, &iret);
                cap_plgetline(place, arrow_x, arrow_y, &iret);
            } else {
                printf("\t{notplaced}\n");
            }
        }
        for (vertex = 0; vertex < points; vertex++) {
            printf("\t{vertex %.4f %.4f}\n", xpts[vertex] + offset_x, 
                    ypts[vertex] + offset_y
                );
        }

        if (placed && !in_center) {
            printf("\t{arrow %.4f %.4f %.4f %.4f}\n", arrow_x[0], arrow_y[0], 
                    arrow_x[1], arrow_y[1]
                );
        }
        printf("}\n");
    }
}

/*======================================================================*/

void dump_objects(CMDObjectSet objects, PlacementSet placements)
/*****************************************************************************
 * dump_objects
 *  Iterate over all the objects in the object set and print them out
 *
 * Input parameters:
 *  CMDObjectSet    objects     Set of objects to print
 *  PlacementSet    placements  Placement information for the objects
 *
 * Output parameters:
 *  None
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    CMDObject   object;
    float       minx, miny, maxx, maxy;
    int         iret;
/*---------------------------------------------------------------------*/

    cap_psgetarea(placements, &minx, &maxx, &miny, &maxy, &iret);
    printf("{\n");
    printf("\t{area %.4f %.4f %.4f %.4f}\n",minx, maxx, miny, maxy);
    printf("}\n");

    cmd_ositerinit(objects, &iret);
    cmd_ositernext(objects, &object, &iret);
    while (object) {
        print_object(placements, object);
        cmd_ositernext(objects, &object, &iret);
    }
}

/*======================================================================*/

void usage(void)
/*****************************************************************************
 * usage
 *  Print usage statement on how to operate the program
 *
 * Input parameters:
 *  None
 *
 * Output parameters:
 *  None
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ***********************************************************************/
{
    fprintf(stderr, "-i <input>\n");
    exit(-1);
}


int main(int argc, char *argv[])
/************************************************************************
 * main
 *  Mainline routine for the test program
 *
 * Input parameters:
 *  int     argc        Count of the number of arguments on the command line
 *  char    *argv[]     String for each command line argument
 *
 * Output parameters:
 *  None
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 * T. Piper/SAIC	10/06	Increased errmsg to 513			*
 ***********************************************************************/
{
    CMDObjectSet    objects;
    PlacementSet    placements;
    char            *input, *libname, errmsg[513];
    int             ebuff, eflag, elevel, error, errors, ier;
    int             iret, libname_len;
/*---------------------------------------------------------------------*/

    libname = "CAP";
    ebuff = 1;
    elevel = 2;
    libname_len = strlen(libname);
    input = NULL;
    eflag = 0;
    ier = 0;

    switch(getopt(argc, argv, "i:h")) {
        case 'i':
            input = optarg;
        break;

        case 'h':
            usage();
        break;

        default:
        break;
    }

    if (input) {
        cmd_osnew(&objects, &iret);
        cap_psnew(&placements, &iret);
        parse_input(input, objects, placements);

        cap_psplace(placements, objects, &iret);

        if (iret != 0) {
            er_stat(&elevel, &ebuff, &eflag, &ier);
                    er_wmsg ( libname, &iret, " ", &ier, libname_len, 1);       
        }

        dump_objects(objects,placements);

        cmd_osdel(objects, &iret);
        objects = NULL;
        cap_psdel(placements, &iret);
        placements = NULL;

        er_gnumerr ( &errors, &iret );
        for (error = 0; error < errors; error++) {
            er_gerrmsg ( &error, errmsg, &ier );
            fprintf(stderr,"%s\n", errmsg);
        }
    }
    return (0);
}
