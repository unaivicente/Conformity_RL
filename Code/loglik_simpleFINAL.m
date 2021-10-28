function LL=loglik_simpleFINAL(alpha,U,UU)
        EST(1)=0.5;
        EE2(1)=abs(U(1,1)-U(1,3));
        
        for i=2:length(U)
            

                DD1=abs(UU(i-1,1)-UU(i-1,3));
                
                R(i)= DD1 - EST(i-1);
                
                EST(i)=EST(i-1)+alpha*R(i);                
                EE2(i)=1-abs(U(i,1)-U(i,3));

        end;
        DD=abs(EE2-EST);
        DD=DD;
        L=find(DD<0.001);
        DD(L)=0.001;
        LL=sum(log(DD));
end
