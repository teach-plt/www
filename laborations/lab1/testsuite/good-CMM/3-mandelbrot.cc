// This program computes a part of the Mandelbrot set.
//
// It outputs a PGM (portable gray map) graphics on stdout
// except for the magic header "P2" which has to be added manually.

#include <stdio.h>
#define printInt(i)    printf("%d\n",i)
#define printDouble(d) printf("%f\n",d)

// From wikipedia, https://en.wikipedia.org/wiki/Mandelbrot_set#Escape_time_algorithm
//
// For each pixel (Px, Py) on the screen, do:
// {
//   x0 = scaled x coordinate of pixel (scaled to lie in the Mandelbrot X scale (-2.5, 1))
//   y0 = scaled y coordinate of pixel (scaled to lie in the Mandelbrot Y scale (-1, 1))
//   x = 0.0
//   y = 0.0
//   iteration = 0
//   max_iteration = 1000
//   // Unoptimized:
//   // while (x*x + y*y <= 2*2  AND  iteration < max_iteration) {
//   //   xtemp = x*x - y*y + x0
//   //   y = 2*x*y + y0
//   //   x = xtemp
//   //   iteration = iteration + 1
//   // }
//   // Optimized:
//   rsquare = 0
//   isquare = 0
//   zsquare = 0
//   while (rsquare + isquare <= 4  AND  iteration < max_iteration) {
//     x = rsquare - isquare + x0
//     y = zsquare - rsquare - isquare + y0
//     rsquare = x*x
//     isquare = y*y
//     zsquare = (x + y)*(x + y)
//     iteration = iteration + 1
//   }
//   color = palette[iteration]
//   plot(Px, Py, color)
// }

// Compute the color value of the given point in the complex plane.
//
int pixel
  ( int maxcolor   // maximal color value
  , double x0      // real coordinate of c
  , double y0      // imaginary coordinate of c
  )
{
  int iteration = 0;
  double x2   = 0.0;      // real part x of z squared
  double y2   = 0.0;      // imaginary part y of z squared
  double xy2  = 0.0;      // (x+y)²
  double x2y2 = 0.0;      // x² + y² : square of length of z

  while (x2y2 <= 4.0 && iteration < maxcolor) {

    // z ← z² + c
    double x   = x2  - y2   + x0;  // next x ← x² - y² + x0
    double y   = xy2 - x2y2 + y0;  // next y ← 2xy + y0

    // recompute intermediate values
    double xy  = x + y;            // x+y
    xy2  = xy * xy;                // (x+y) squared
    x2   = x * x;
    y2   = y * y;
    x2y2 = x2 + y2;

    // Next iteration
    iteration++;
  }
  return maxcolor - iteration;
}

// Output the color values of an image of size height * width
// displaying the Mandelbrot set within the given complex rectangle.
//
void mandelbrot
  ( int maxcolor               // maximal color value
  , int width   , int height   // image size
  , double left , double right // real interval
  , double lower, double upper // imaginary interval
  )
{
  // PPM header
  printInt(width); printInt(height); printInt(maxcolor);

  // Step-width calculation
  double xstep = (right - left)  / width;  // width of a pixel in the complex plane
  double ystep = (upper - lower) / height; // height of a pixel

  // Outer loop py over rows
  double y = upper; int py = 0;
  while (py < height) {

    // Inner loop px over columns
    double x = left; int px = 0;
    while (px < width) {

      // Output color at (px,py)
      printInt (pixel (maxcolor, x, y));

      // Next column
      x = x + xstep; px++;
    }
    // Next row
    y = y - ystep; py++;
  }
}

int main() {
  mandelbrot (50, 1280, 960, 0.0-2.1, 1.1, 0.0-1.2, 1.2);
}
