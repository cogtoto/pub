using System;
using System.Drawing;

class Mandelbrot
{
    static void Main()
    {
        // Define image size and maximum iterations
        int width = 800;
        int height = 600;
        int maxIterations = 1000;

        // Create a new bitmap
        Bitmap bitmap = new Bitmap(width, height);

        // Define the range of complex numbers to consider
        double xmin = -2.0;
        double xmax = 1.0;
        double ymin = -1.5;
        double ymax = 1.5;

        // Iterate over each pixel and determine its color
        for (int x = 0; x < width; x++)
        {
            for (int y = 0; y < height; y++)
            {
                // Map pixel coordinates to complex plane
                double a = xmin + (xmax - xmin) * x / (double)width;
                double b = ymin + (ymax - ymin) * y / (double)height;

                // Calculate whether the point belongs to the Mandelbrot set
                int iterations = MandelbrotIterations(a, b, maxIterations);

                // Determine color based on number of iterations
                Color color = iterations == maxIterations ? Color.Black : Color.FromArgb(iterations % 256, iterations % 256, iterations % 256);

                // Set pixel color in the bitmap
                bitmap.SetPixel(x, y, color);
            }
        }

        // Save the bitmap to a file
        bitmap.Save("mandelbrot.png");
    }

    // Mandelbrot set iteration function
    static int MandelbrotIterations(double a, double b, int maxIterations)
    {
        double ca = a;
        double cb = b;
        int n = 0;

        while (n < maxIterations)
        {
            double aa = a * a - b * b;
            double bb = 2 * a * b;
            a = aa + ca;
            b = bb + cb;

            if (a * a + b * b > 4.0)
            {
                break;
            }

            n++;
        }

        return n;
    }
}