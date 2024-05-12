
using System;


class MandelbrotText
{
	public  struct Complexe 
	{ public double Re ;
	  public  double Im ;
	}

	public static Complexe Complexe_build (double reel, double imaginaire)
	{
	Complexe res ; res.Re = reel ; res.Im = imaginaire ;
	return res ;
	}

	public static Complexe Add (Complexe a, Complexe b)
	{ 
		Complexe res ;
		res = Complexe_build(a.Re + b.Re, a.Im + b.Im) ;
		return res ;
	}
    public static Complexe Mul(Complexe a, Complexe b)
    {
	Complexe res ;
        double realPart = a.Re * b.Re - a.Im * b.Im;
        double imaginaryPart = a.Re * b.Im + a.Im * b.Re;
	res = Complexe_build(realPart, imaginaryPart);
	return res ;
    }

    public static double module(Complexe c)
    {
        return c.Re * c.Re + c.Im * c.Im;
    }

public static bool appartient(Complexe c)
{
 int n = 1 ;
 Complexe z ; z=Complexe_build(0,0) ;
 while (n <= 300 && module(z) <= 4)
 {
	z = Add(c, Mul(z,z));
	n++ ;
 }
 if  (n == 301) return true ;
 else return false ;
}
    static void Main()
    {

        char[] asciiChars = { ' ', '.', ',', '-', '+', '*', '%', '#', '@' };

        for (int i = -20; i < 5 ; i++)
        {
            for (int j = -12; j < 12 ; j++)
            {
		Complexe c ; c.Re=i/10d ; c.Im =j/10d ;
                if (appartient(c)) Console.Write("1") ; 
		else Console.Write(" ") ;
            }
            Console.WriteLine();
        }
    }

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