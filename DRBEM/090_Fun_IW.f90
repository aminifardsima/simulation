Function IW(xx,yy,time)  

use      Var     
Implicit None
Integer::i,j
Real(8)::x,y,xx,yy,R,GRN,dGRNdn,teta,dRdro,time
Real(8)::IW,TW

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


IW=0D0
Do i=1,n_teta
Do j=1,n_dteta
teta=tetaij(i,j)

x=Rad*Dcos(teta)
y=Rad*Dsin(teta)

R=Dsqrt((x-xx)**2+(y-yy)**2)

GRN=-1D0/(2D0*Pi)*Dlog(R)

IW=IW+h_cnv/k_cnd*GRN*TW(teta,time)*WI(j)

End DO !j

End Do !i 

End Function IW