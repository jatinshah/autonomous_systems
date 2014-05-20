/*                                                      -*-c++-*-
    Copyright (C) 2000 Gregory D. Hager and Darius Burschka (JHU
    Lab for Computational Interaction with Physical Systems (CIPS))

Permission is granted to any individual or institution to use, copy,
modify, and distribute this software, provided that this complete
copyright and permission notice is maintained, intact, in all copies and
supporting documentation.  Authors of papers that describe software
systems using this software package are asked to acknowledge such use by
a brief statement in the paper.

We provide this software "as is" without express or implied warranty.
*/

#include "config.h"
#include "XVWindowX.h"
#include "Mpeg.h"
#include "XVImageRGB.h"

void help();

int main (int argc, char **argv) {

  XVImageRGB<XV_RGBA32> new_color_im;
  MPEG<XV_RGBA32>  grabber("mpeg_file.mpg");
  XVWindowX<XV_RGBA32> window(&(grabber.frame(0)), 0, 0);
  window.map();

  for (;;) {
    
    new_color_im = grabber.next_frame_continuous();
    window.CopySubImage(&new_color_im);     
    window.swap_buffers();
    window.flush();
  }
  return 0;
}
