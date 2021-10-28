S=dir('/Volumes/GoogleDrive/My Drive/Doctorado Social Neuroscience/Exp 1 - Social Conformity/Results/Originals Phase 1/Cooperative/*.csv');


   
for suje=1:length(S)
    
    TT = readtable([S(suje).folder '/' S(suje).name]);
 
    T=table2array(TT(:,2:7));
    for i=1:size(T,1)
        
        U((T(i,1)*40)+T(i,2)+1,T(i,3))=T(i,4);
        UU((T(i,1)*40)+T(i,2)+1,T(i,3))=T(i,5);
    end;
    
    R=U;
    RR=UU;
    
      % Relative change scalation within users (U)
    for i=1:length(U)
        A=[U(i,:) UU(i,:)];
        mx=max(A);
        mn=min(A);
        rng=mx-mn;
        if rng==0
            U(i,:)=0.5;UU(i,:)=0.5;
        else
            U(i,:)=(U(i,:)-mn)/rng;
            UU(i,:)=(UU(i,:)-mn)/rng;
        end
        clear A;
    end
         
%     for i=1:length(U) %Number of total trials
%         d=abs(U(i,1)-UU(i,1));
%         md=min([U(i,1) UU(i,1)]);
%         if d==0
%             U(i,:)=999;UU(i,:)=999;
%         else
%             U(i,:)=(U(i,:)-md)/d; 
%             UU(i,:)=(UU(i,:)-md)/d;
%         end
%     end;
%     
%     
%      %Take out the bad trials
%      U(U(:, 1)== 999, :)= [];
%      UU(UU(:, 1)== 999, :)= [];
    
    hh = @(alpha)loglik_simpleFINAL(alpha,U,UU);
    aa(suje) = fminbnd(hh,0,1);
    LL(suje)=loglik_simpleFINAL(aa(suje),U,UU);
    
    
    hh2 = @(alpha)loglik_simpleFINAL(alpha,UU,U);
    aa2(suje) = fminbnd(hh2,0,1);
    LL2(suje)=loglik_simpleFINAL(aa2(suje),UU,U);
    
    
       
    clear T;clear TT; clear U; clear UU
end;


COP=[aa aa2];%matrix with alphas of both users per dyad (higher is more dependent of learning rate, less the rest of the model)
LCOP=[LL LL2];%matrix with loglikelihoods of both users per dyad (model adjustment, more negative meaning a higher fit)

clear aa; clear aa2; clear LL; clear LL2;
S=dir('//Volumes/GoogleDrive/My Drive/Doctorado Social Neuroscience/Exp 1 - Social Conformity/Results/Originals Phase 1/Individual/*.csv');

for suje=1:length(S)
    
    TT = readtable([S(suje).folder '/' S(suje).name]);
 
    T=table2array(TT(:,2:7));
    for i=1:size(T,1);
        
        U((T(i,1)*40)+T(i,2)+1,T(i,3))=T(i,4);
        UU((T(i,1)*40)+T(i,2)+1,T(i,3))=T(i,5);
    end;
    
    R=U;
    RR=UU;
       
        % Relative change scalation within users (U)
    for i=1:length(U)
        A=[U(i,:) UU(i,:)];
        mx=max(A);
        mn=min(A);
        rng=mx-mn;
        if rng==0
            U(i,:)=0.5;UU(i,:)=0.5;
        else
            U(i,:)=(U(i,:)-mn)/rng;
            UU(i,:)=(UU(i,:)-mn)/rng;
        end
        clear A;
    end
    
    hh = @(alpha)loglik_simpleFINAL(alpha,U,UU);
    aa(suje) = fminbnd(hh,0,1);
    LL(suje)=loglik_simpleFINAL(aa(suje),U,UU);
    
    
    hh2 = @(alpha)loglik_simpleFINAL(alpha,UU,U);
    aa2(suje) = fminbnd(hh2,0,1);
    LL2(suje)=loglik_simpleFINAL(aa2(suje),UU,U);
    
    
    clear T;clear TT; clear U; clear UU
end;

IND=[aa aa2];
LIND=[LL LL2];

LP=[LCOP LIND];%loglike
ALP=[COP IND];%alpha

type=[];%add label
for i=1:40;type=[type;'cop'];end;
for i=1:40;type=[type;'ind'];end;

figure;boxplot(ALP,type);%alpha
figure;boxplot(LP,type);%loglike

[palpha,h,stats] = ranksum(COP,IND)
[plogl,h,stats] = ranksum(LCOP,LIND)

al=[COP IND];
ll= [LCOP LIND];
std(al)/mean(al)
std(ll)/mean(ll)

DAT= [ALP' LP']

DAT = array2table(DAT)
T.Properties.VariableNames(1:2) = {'ALP','LL'}
writetable(DAT,'first_expdata.csv')

