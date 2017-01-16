"""
PROGRAMME PERMETTANT L'AFFICHAGE DE FLEXAGONES,
A PAUFINER MAIS STYLÉ !!!

REPRÉSENTER LES FLEXAGONES PAR UNE LISTE DÉCRIVANT LE CHEMIN DES CENTRE DES TRIANGLES !!!
"""



class Vect(object):
    """
        Vecteurs à 2 dimensions.
    """
    
    def __init__(self, a, b):
        self.x = a
        self.y = b
        self.length = (a*a + b*b)**(1/2)
    
    def __add__(self, v):
        return Vect(self.x + v.x, self.y + v.y)
    
    def __mul__(self, r):
        return Vect(r*self.x, r*self.y)

    def __rmul__(self, r):
        return Vect(r*self.x, r*self.y)

    def __sub__(self, v):
        return self + (-1)*v
    
    def __truediv__(self, r):
        return self*(1/r)
    
    def __repr__(self):
        return "("+str(self.x) + ", "+(str(self.y))+")"

    def __neg__(self):
        return Vect(-self.x, -self.y)


class Flexagone(list):
    """
        Représentation du flexagone F d'ordre k:
            n = len(F) = 3*k + 1
            F[0] = (x_0, y_0, d) est le centre (x_0, y_O) du premier triangle et d son orientation (d**2 = 1)
            Pour tout i, 0 < i < n,
                F[i] = (v_i, c_i) où 
                    v_i est le vecteur indiquant le sens dans lequel évolue le patron
                    c_i est la couleur du triangle.
    """
    
    def __init___(self):
        return
        
        
from math import sqrt as root
from tkinter import *

class Dessin(Canvas):
    
    
    def __init__(self, master, R, *args, **kwargs):
        Canvas.__init__(self, master=master, *args, **kwargs)
        self.R = R
        self.NORD = Vect(0, R/root(3))  #Pour aller en haut
        self.OSO = Vect(-R/2, -R/(2*root(3))) # Ouest-Sud-Ouest, c'est à dire en bas gauche
        self.ESE = -self.NORD - self.OSO# Est-Sud-Est, en bas à droite
        self.COULEURS = ["red","blue","green","yellow"]
    
    
    
    def dessine_triangle_equi(self, centre, d, couleur):
            A = centre + d*self.NORD
            B = centre + d*self.OSO
            C = centre + d*self.ESE
            self.create_polygon(A.x,A.y,B.x ,B.y,C.x,C.y , fill = self.COULEURS[couleur], outline="black")
            
    
    

    def create_flexagone(self, F):
        centre = Vect(F[0][0], F[0][1])
        d = F[0][2]
        for i in range(1, len(F)):
            v = F[i][0]
            couleur = F[i][1]
            self.dessine_triangle_equi(centre, d, couleur)
            d *= -1
            centre += v
    
    
    
        
A = Tk()
A.geometry("850x800+50+50")
T = Dessin(A, R=75, height=800, width=800)
T.grid(row=1,column=1)
F = [
        [100,100,1],
        [-T.OSO, 0],
        [T.ESE, 1],
        [-T.OSO, 1],
        [T.ESE, 2],
        [-T.OSO, 2],
        [T.ESE, 0],
        [-T.OSO, 0],
        [T.ESE, 1],
        [-T.OSO, 1],
    ]

T.create_flexagone(F)
A.mainloop()