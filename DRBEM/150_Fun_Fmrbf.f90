Function Fmrbf(x,y,xi,yi)  !Define Rbf Function F(Ri)

use      Var     
Implicit None

Integer::j
Real(8)::x,y,xi,yi,Ri,fi
Real(8)::Fmrbf


!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

Ri=Dsqrt((x-xi)**2+(y-yi)**2)




!If (Ri*AA>=0.AND.Ri*AA <= 1 ) fi=1D0-(Ri*AA)
!If (Ri*AA >1)   fi=0D0
!Fmrbf=fi**BB

Fmrbf=0D0
Do j=0,m_rbf

Fmrbf=Fmrbf+(Ri**j)

End Do

End Function Fmrbf
