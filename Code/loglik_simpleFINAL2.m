function LL=loglik_simpleFINAL2(alpha,U,UU)
        EST(1)=0.5;
        EE2(1)=abs(U(1,1)-U(1,3));
        
        for i=2:length(U)
            if ~(UU(i-1,1) == U(i-1,1));

                DD1=abs(UU(i-1,1)-UU(i-1,3));
                
                R(i)= DD1 - EST(i-1);
                
                EST(i)=EST(i-1)+alpha*R(i);                
                EE2(i)=1-abs(U(i,1)-U(i,3));
            else
                EST(i)=EST(i-1);              
                EE2(i)=999;
            end;
        end;
        % REMOVING EQUAL AT FIRST REPETITION
        EST(EE2==999) = []; %take the index of EE2
        EE2(EE2==999) = [];
            
        DD=abs(EE2-EST);
        DD=DD;
        L=find(DD<0.001);
        DD(L)=0.001;
        LL=-sum(log(DD));
end
