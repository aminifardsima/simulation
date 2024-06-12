Program Drbem
Include 'link_fnl_static.h'
Use	Var



Implicit None

Real(8)::time
Integer::j,cont,i
Integer(4)::s


Real(8)::T0

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
100 Format(50000(F25.16))

If(Way==3)   Then
nj=n_dteta/2
Call	Ch_Gussian
End if
Call	Ch_Gussian
Allocate(WI(1:n_dteta),Aj(1:n_dteta),tetaij(1:n_teta,1:n_dteta))  

Vct_Q=0D0;Vct_IW=0D0;Vct_UFinvTp=0D0;Vct_B=0D0  
Vct_UFinvQ=0D0;Vct_IGFinvTp=0D0;Vct_IGFinvQ=0D0
Vct_T=0D0


! @@@@@@@@@@@@@@@@@@@@@@@@
Vct_Tp=0D0
Do j=1,N+L
Vct_Tp(j)=T0(x_g(j),y_g(j))                          !Make Vect_Tp   (N+L  *  1)
End Do   !j

! @@@@@@@@@@@@@@@@@@@@@@@@



Call	Integral_Computing

Call	Circular_Grid

Call	Matrix


Open(unit=5400,file="CW.txt")
    
    i=0 
	cont=0
	Do time=time_s+deltat,time_e,deltat
	cont=cont+1
		
		Call	MatrixTime(time)

		Call	DLSARG(N+L,Mat_A,N+L,Vct_B,1,Vct_T) 

!   ********************************************************************************** 
!   *DLSARG(N,A,LDA,B,Ipath,x)      Solve Ax=B                                       *
!   *N=Number of Equations                       A=Coefficients Matrix               *
!   *LDA=Leading Dimension of input Array A      B=Known Matrix                      *
!   *Ipath=1 if Ax=B                             Ipath=2 if A^Tx=B                   * 
!   **********************************************************************************  
     Call Output
   
   
    If (cont==Step_W) Then
	cont=0
	
	
    i=i+1        
   Open(unit=i,file=ch_txt(i))
   Do j=1,N+L
   Write(i,*)XYT_Final(j,:)
   End Do
   close(i)
		
	write(*,*)time,time_e		
    Write(5400,100)time,XYT_Final(10,3)
	End If
	
	
	

	Vct_Tp=Vct_T
	

	End Do !time
    Close(5400)

DeAllocate(WI,Aj,tetaij)!   Be care full use this at the end of program!

End Program Drbem