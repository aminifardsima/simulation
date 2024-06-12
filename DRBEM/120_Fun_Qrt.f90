Function d(x,y,t)
Use Var
Implicit None
Real(8)::x,y,t
Real(8)::d

d=Dsqrt(Dabs(Rad**2-(x*Dsin(t)+y*Dcos(t))**2))-x*Dcos(t)+y*Dsin(t)

End Function

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

Function f(x,y,t)
use Var 
Implicit None
Real(8),Parameter::alfa=450D0,w0=Rad/4D0
Real(8)::x,y,t
Real(8)::f,d

f=Dexp(-2D0*((x*Dsin(t)+y*Dcos(t))/w0)**2)*Dexp(-alfa*d(x,y,t))

End Function

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
Function Qrt(x,y,time) 
use Var 
Implicit    None 
Real(8)::x,y,time,t0,tp,Q0
Real(8)::Qrt,f

Q0=6D8
t0=1D0
tp=0.3D0


if ((x**2+y**2)>=((Rad-(Rad/1000D0))**2)) then 
Qrt=0D0
Else
!Qrt=Q0*DExp(-(time-t0)**2/tp**2)*(f(x,y,0)+f(x,y,((1D0/2D0)*Pi))+f(x,y,Pi)+f(x,y,(3D0/2D0)*Pi))

If (time>100D-6.AND.time < 5000D-6 ) Q0=0D0
if (time>5100D-6.AND.time < time_e) Q0=0D0

Qrt=Q0*(f(x,y,1D0/4D0*pi)+f(x,y,11D0/12D0*pi)+f(x,y,19D0/12D0*pi))

End if 


End function 


