Function     LGamaI(i,xx,yy)
use      Var     
Implicit None
integer::i,j
Real(8)::x,y,xx,yy,R,GRN,dGRNdn,teta,dRdro
Real(8)::LGamaI

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@



LGamaI=0D0
Do j=1,n_dteta


teta=tetaij(i,j)

x=Rad*Dcos(teta)
y=Rad*Dsin(teta)

R=Dsqrt((x-xx)**2+(y-yy)**2)

GRN=-1D0/(2D0*Pi)*Dlog(R)

dRdro=((x-xx)*Dcos(teta)+(y-yy)*Dsin(teta))/R

dGRNdn=-1D0/(2D0*Pi*R)*dRdro

LGamaI=LGamaI-((h_cnv/k_cnd)*GRN+dGRNdn)*WI(j)

End DO !j

End Function LGamaI
