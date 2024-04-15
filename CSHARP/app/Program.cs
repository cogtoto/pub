using System;

//static int Recherche(string mot,string pattern)
        //{
            ////int result=0;
            //bool continu= true;

            //for (int i = 0; i < mot.Length; i++)
            //{
                //for (int j = 0; j < pattern.Length && continu; j++)
                //{
                    //if (mot[i+j] == pattern[j])
                        //continu = true;
                    //else
      ////                  continu = false;
                //}
//
                //if (continu)
                ////{
                    ////Console.WriteLine("gagné");
		    //result=i;
                    //return result;
                //}
		//else
		    //continu=true ;
            //}
            //Console.WriteLine("perdu");
            //return result;
        //}

static string multi (int n)
	{
	        string res ="";
		for (int i=0; i<=10; i++)
		{
			res=res + " " + (i*n).ToString();
		}
	return res ;
	};

static int diviseur (int n)
     { 
	int res =0 ;
	for (int i=1; i<=n;i++)
	{
		if (n%i==0) res=res+i ;
	}
     return res ;
     } ;

static void AfficheTriangle(int n)
   {
	for (int i=1; i<=n; i++)
	{
		string s = "";
		for (int j=1; j<=i; j++)
		  s = s + "  "+j ;
		Console.WriteLine(s);
	}
   }

Console.WriteLine("Hello, World!");
AfficheTriangle(4);