"""
fonctions pour calculer le nombre d'hexaflexagones.
"""

def liste_catalan(n):
    l = [1]*n
    c=1
    for i in range(1,n):
        c = 2*(2*i-1)*c//(i+1)
        l[i]=c
    return l

L = liste_catalan(50)

def C(n):
    if n-int(n)==0:
        return L[int(n)]
    return 0


def flex(n):
    return int(C(n-2)/(2*n)+C(n/2-1)/4 + (C((n-1)/2)+C(n/2-1))/2 + C(n/3-1)/3)