# -*- coding: utf-8 -*-
"""
Created on Wed Dec 28 19:59:34 2016

HEXAFLEXAGONES

Module de gestion d'hexaflexagones
Représentation pure graphe d'adjacence
"""

def croisement(A,B):
    a,b=A
    c,d=B
    if a>b:
        a,b=b,a
    return ((a<=c<=b and (d<=a or b<=d)) or (a<=d<=b and (c<=a or b<=c)))
    


def for_all(l,f):
    i=0
    while f(l[i]):
        i+=1
    return (i==len(l))

def verif(aretes,a,b):
    if a>b:
        a,b=b,a
    n=len(aretes)
    res=True
    for i in range(n):
        for j in aretes[i]:
            if croisement((i,j),(a,b)):
                res=False
                break
        if not res:
            break
    return res



class IntersectError(Exception):
    """Exception levée lors d'une construction de flexagone fallacieuse"""
    def __init__(self,message=""):
        if message=="":
            self.message="Deux aretes de votre flexagone se croisent !"
        else:
            self.message = message
    def __str__(self):
        return self.message




class flexagone(list):
    def __init__(self,n,liens):
        self.aretes = [[] for i in range(n)]
        for x in liens:
            (a,b)=x
            if verif(self.aretes,a,b):
                self.aretes[a].append(b)
                self.aretes[b].append(a)
            else:
                raise IntersectError("Croisement entre deux arêtes lors de la construction")
        
        
