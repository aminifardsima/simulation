Function Umrbf(x,y,xi,yi)  !ødefine  Function U(Ri)

use      Var     
Implicit None

Integer::j
Real(8)::x,y,xi,yi,Ri,fi
Real(8)::Umrbf

!@@@@@@@@@@@@@@@@@@@@@@@@@@@

Ri=Dsqrt((x-xi)**2+(y-yi)**2)


!If (Ri*AA>=0.AND.Ri*AA <= 1 ) fi=1D0-(Ri*AA)
!If (Ri*AA >1)   fi=0D0

!umrbf=BB*(BB-1D0)*(AA**2)*(fi)**BB-2D0

umrbf=0D0
Do j=0,m_rbf

umrbf=umrbf+((Ri**(j+2))/(j+2)**2)

End Do

End Function Umrbf

