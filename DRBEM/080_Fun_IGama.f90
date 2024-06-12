Function IG(xx,yy,xi,yi)  

use      Var     
Implicit None
Integer::i,j
Real(8)::x,y,xx,yy,xi,yi,R,Ri,GRN,dGRNdn,teta,dteta,dRdro,dRidro,dUmrbfdn
Real(8)::IG,Umrbf,dUmrbfdRi

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


IG=0D0
Do i=1,n_teta
Do j=1,n_dteta

teta=tetaij(i,j)

x=Rad*Dcos(teta)
y=Rad*Dsin(teta)

R=Dsqrt((x-xx)**2+(y-yy)**2)
Ri=Dsqrt((x-xi)**2+(y-yi)**2)

GRN=-1D0/(2D0*Pi)*Dlog(R)

dRdro=((x-xx)*Dcos(teta)+(y-yy)*Dsin(teta))/R
dRidro=((x-xi)*Dcos(teta)+(y-yi)*Dsin(teta))/Ri

dGRNdn=-1D0/(2D0*Pi*R)*dRdro
dUmrbfdn=dUmrbfdRi(Ri)*dRidro

IG=IG+(GRN*dUmrbfdn-Umrbf(x,y,xi,yi)*dGRNdn)*WI(j)

End DO !j

End Do !i 

End Function IG