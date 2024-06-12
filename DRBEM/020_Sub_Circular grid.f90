Subroutine	Circular_Grid
Use	Var
Include 'link_fnl_static.h'
Implicit	None

Integer::i,j,count
Real(8)::t,x1,x2,x3,x4,y1,y2,y3,y4,dx1,dy1,dx2,dy2
Real(8)::COR_V(1:M_grid-1,1:4),COR_H(1:M_grid-1,1:4)	!COR(i,1)=x1,COR(i,2)=y1,COR(i,3)=x2,COR(i,4)=y2
Real(8)::AMAT(2,2),BVECT(2),XVECT(2)

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

c_g=1D0

count=0                                            !Count N node on boundary 
Do j=1,n_teta
count=count+1
ro_g(count)=Rad
teta_g(count)=(j-0.5D0)*(2D0*Pi)/n_teta
x_g(count)=ro_g(count)*Dcos(teta_g(count))	!gerehaye marzi
y_g(count)=ro_g(count)*Dsin(teta_g(count))	!gerehaye marzi
c_g(count)=0.5D0                            !Coefficient C=1/2 when put in the boundary
End Do !J    


Do i=-M_grid+1,M_grid-1
count=count+1
x_g(count)=i*Rad/M_grid
y_g(count)=0D0
End Do
Do i=-M_grid+1,M_grid-1
If (i==0) Cycle
count=count+1
x_g(count)=0D0
y_g(count)=i*Rad/M_grid
End Do

Do i=1,M_grid-1
COR_V(i,1)=(M_grid-i)*Rad/M_grid
COR_V(i,2)=0D0
t=i*(Pi/2D0)/M_grid
COR_V(i,3)=Rad*DCos(t)
COR_V(i,4)=Rad*DSin(t)
COR_H(i,1)=0D0
COR_H(i,2)=(M_grid-i)*Rad/M_grid
t=(M_grid-i)*(Pi/2D0)/M_grid
COR_H(i,3)=Rad*DCos(t)
COR_H(i,4)=Rad*DSin(t)
End Do

Do i=2,M_grid-1
Do j=M_grid-1,M_grid+1-i,-1
count=count+1
x1=COR_V(i,1);y1=COR_V(i,2)
x2=COR_V(i,3);y2=COR_V(i,4)
x3=COR_H(j,1);y3=COR_H(j,2)
x4=COR_H(j,3);y4=COR_H(j,4)
dx1=x2-x1;dx2=x4-x3
dy1=y2-y1;dy2=y4-y3
AMAT(1,1)=+dy1
AMAT(1,2)=-dx1
AMAT(2,1)=+dy2
AMAT(2,2)=-dx2
BVECT(1)=x1*dy1-y1*dx1
BVECT(2)=x3*dy2-y3*dx2
CALL DLSARG(2,AMAT,2,BVECT,1,XVECT)
x_g(count)=XVECT(1)
y_g(count)=XVECT(2)
End Do
End Do


Do i=2,M_grid-1
Do j=M_grid-1,M_grid+1-i,-1
count=count+1
x_g(count)=-x_g(count-((M_grid-2)*(M_grid-1)/2))
y_g(count)=+y_g(count-((M_grid-2)*(M_grid-1)/2))
End Do
End Do

Do i=2,M_grid-1
Do j=M_grid-1,M_grid+1-i,-1
count=count+1
x_g(count)=+x_g(count-((M_grid-2)*(M_grid-1)/2))
y_g(count)=-y_g(count-((M_grid-2)*(M_grid-1)/2))
End Do
End Do

Do i=2,M_grid-1
Do j=M_grid-1,M_grid+1-i,-1
count=count+1
x_g(count)=-x_g(count-((M_grid-2)*(M_grid-1)/2))
y_g(count)=+y_g(count-((M_grid-2)*(M_grid-1)/2))
End Do
End Do
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!output

Open(unit=10,File="XY_grid.TXT")
Do i=1,N+L
Write(10,*)x_g(i),y_g(i)
End Do
Close(10)
Write(*,*)"Count=",Count,N+L

End subroutine       !Circular_Grid