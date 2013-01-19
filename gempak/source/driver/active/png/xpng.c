#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <png.h>

int WritePNG (  FILE            *fp,
                unsigned char   *pic,
                int              w,
                int              h,
                unsigned char   *rmap,
                unsigned char   *gmap,
                unsigned char   *bmap,
                int              numcols)
{
png_structp	write_ptr;
png_infop	winfo_ptr;
int bit_depth 	= 8;
int color_type 	= PNG_COLOR_TYPE_PALETTE;
int interlace_type = PNG_INTERLACE_NONE;
int compression_type = PNG_COMPRESSION_TYPE_DEFAULT;
int filter_method = PNG_FILTER_TYPE_DEFAULT;
png_colorp	palette;
int i;

write_ptr =  png_create_write_struct (PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);

if (!write_ptr) return (-1);

winfo_ptr = png_create_info_struct(write_ptr);

if (!winfo_ptr)
    {
    png_destroy_write_struct(&write_ptr,
        (png_infopp)NULL);
    return (-1);
    }

if (setjmp(write_ptr->jmpbuf))
    {
    png_destroy_write_struct(&write_ptr, &winfo_ptr);
    return (-2);
    }

png_init_io (write_ptr, fp);

png_set_IHDR(write_ptr, winfo_ptr, w, h,
            bit_depth, color_type, interlace_type,
            compression_type, filter_method);

palette = (png_colorp)png_malloc(write_ptr, numcols * sizeof (png_color));

for(i=0;i<numcols;i++)
   {
   palette[i].red = (png_byte)rmap[i];
   palette[i].green = (png_byte)gmap[i];
   palette[i].blue = (png_byte)bmap[i];
   }

png_set_PLTE(write_ptr, winfo_ptr, palette, numcols);

png_write_info(write_ptr, winfo_ptr);


for(i=0;i<h;i++)
   png_write_row(write_ptr, (png_bytep)(pic + (w*i)) );


png_write_end(write_ptr, NULL);


png_free(write_ptr, palette);
png_destroy_write_struct  (&write_ptr, &winfo_ptr);
return(0);
}
