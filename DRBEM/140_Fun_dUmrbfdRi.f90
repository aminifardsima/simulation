Function dUmrbfdRi(Ri)  
use      Var     
Implicit None

Integer::j
Real(8)::Ri,fi
Real(8)::dUmrbfdRi

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


!If (Ri*AA>=0.AND.Ri*AA <= 1 ) fi=1D0-(Ri*AA)
!If (Ri*AA >1)   fi=0D0

!dUmrbfdRi=-1D0*BB*(AA**3)*(BB-1D0)*(BB-2D0)*(fi)**BB-3D0

dUmrbfdRi=0D0
Do j=0,m_rbf
dUmrbfdRi=dUmrbfdRi+((Ri**(j+1))/(j+2))
End Do 

End function dUmrbfdRi