Subroutine	Matrix
Use			Var
Include 'link_fnl_static.h'              !Link to Imsl  7
!Use LINRG_INT                            !Use LINRG library to inverse Matrix
!Use MRRRR_INT                            !Use MRRRR library to Multiple two Matrix

Implicit	None
Integer::	i,j
Real(8)::	LGamaI,Umrbf,Fmrbf,IG !Function Var

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

Mat_LG=0D0;Mat_IG=0D0;Mat_U=0D0;Mat_F=0D0;Mat_I=0D0;Mat_Finv=0D0
Mat_UFinv=0D0;Mat_IGFinv=0D0;Mat_A=0D0    

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

Do i=1,N                                        !Make elements as columns of matrix
    Do j=1,N+L                                  !Make nodes as row of matrix
    Mat_LG(j,i)=LGamaI(i,x_g(j),y_g(j))            !LGji=LGi(rj)  (N+l *  N )

    End Do !j
End Do !i

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

Do i=1,N+L

	Mat_I(i,i)=c_g(i)                                      !Make single matrix c(rj)I (N+L *  N+L)

    Do j=1,N+L
    
    Mat_U(j,i)=c_g(j)*Umrbf(x_g(j),y_g(j),x_g(i),y_g(i))   !Make Uji=c(rj)*u(rj,ri)   (N+L *  N+L)
    Mat_IG(j,i)=IG(x_g(j),y_g(j),x_g(i),y_g(i))            !MAke IGji=IG(rj,ri)       (N+L *  N+L)
    Mat_F(j,i)=Fmrbf(x_g(j),y_g(j),x_g(i),y_g(i))          !Make Fji=F(rj,ri)         (N+L *  N+L)
       
    End Do !j

Write(*,*)i,N+L

End Do !i 
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

Call DLINRG (N+L,Mat_F,N+L,Mat_Finv,N+L)                                         !Make Finv 


Call DMRRRR(N+L,N+L,Mat_U,N+L,N+L,N+L,Mat_Finv,N+L,N+L,N+L,Mat_UFinv,N+L)        !Make U*Finv
                                                                                                                   
Call DMRRRR(N+L,N+L,Mat_IG,N+L,N+L,N+L,Mat_Finv,N+L,N+L,N+L,Mat_IGFinv,N+L)      !MAke IG*Finv 

                                                                   
Mat_A=Mat_I-Alpha*Mat_UFinv+Alpha*Mat_IGFinv- Mat_LG							 !Make MatA					
                                                            


End   Subroutine	!Matrix

    
    
    
    
    



    