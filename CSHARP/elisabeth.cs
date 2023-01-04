using System;

static int Recherche(string s1,string mot)
        {
            int result=0;
            bool continu= true;

            for (int i = 0; i < s1.Length; i++)
            {
                for (int j = 0; j < mot.Length && continu; j++)
                {
                    if (mot[j] == s1[j + i])
                        continu = true;
                    else
                        continu = false;
                }

                if (continu)
                {
                    Console.WriteLine(result);
                    return result;
                }
            }
            Console.WriteLine(result);
            return result;
        }