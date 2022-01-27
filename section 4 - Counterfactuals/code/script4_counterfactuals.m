%This script computes welfare effects of Russian embargo (with IO linkages)
%% Setting technical parameters
close all;
vfactor  = -0.2;    %adjustment factor
tol      = 1E-07;   %tolerance
maxit    = 1E+10;   %max number of iterations

%% Loading of variables
% initial equilibrium (2013 before embargo) without trade deficit
load initial_condition_2013_noS
J_nt=13;                                                                   % Number of non-traded sectors

% Loading trade flows and tariffs
tau2013 = importdata('..\data\Tariffs.mat');                         % Matrix of bilateral tariffs
tau2013_emb=importdata('..\data\Trade_costs.mat');                   % Tariffs and embargo
tau=[1+tau2013/100; ones(J_nt*N,N)];                                       % Actual tariff vector
taup=[1+tau2013_emb/100; ones(J_nt*N,N)];                                  % Counterfactual tariff vector
tau_hat=taup./tau;                                                         % Change in tariffs 

%% Principal counterfactuals
VAn_u=VAn./400000;
Sn_u=Sn./400000;

% Computing total income in country n
In = (VAn+sum(X0.*(1-F))'-Sn);                               

% Solving for the counterfactual
[wf0_all,pf0_all,PQ_all,Fp_all,Dinp_all,ZW,Snp_all,c_all] = equilibrium_LC(tau_hat,taup,alphas,T,B,G,Din,J,N,maxit,tol,VAn_u,Sn_u,vfactor); 

% Calculating welfare, terms of trade and trade volume effects 
[Welfare_all,ToTL_all,VoTL_all,ToTEjn_all,ToTMjn_all,VoTXjn_all,VotXjni_all,Xjni_dc_all] = Welfarelineal(PQ_all,Dinp_all,c_all,tau,taup,xbilattau,In,J,N);  

% Computing price index for embargoed products
ind_emb=[3 11 16 17 19];
pf0_emb=pf0_all(ind_emb,:);
alphas_emb=alphas(ind_emb,:);
alphas_emb_n=alphas_emb./sum(alphas_emb);
P_emb=prod(pf0_emb.^(alphas_emb_n));
% Getting number for Russia: P_emb(33) = 1.065517101588728

% Computing price index for all non-embargoed products
pf0_non_emb_all=pf0_all(setdiff(1:end,ind_emb),:);  %all non-embargoed products
alphas_non_emb_all=alphas(setdiff(1:end,ind_emb),:);
alphas_non_emb_n=alphas_non_emb_all./sum(alphas_non_emb_all);
P_non_emb=prod(pf0_non_emb_all.^(alphas_non_emb_n));
% Getting number for Russia: P_non_emb(33) = 1.00032811759120

% Computing price index for non-embargoed food products
ind_non_emb_food = [8 22];                            %non-embargoed food
pf0_non_emb_food=pf0_all(ind_non_emb_food,:);
alphas_non_emb_food=alphas(ind_non_emb_food,:);

%alphas_non_emb_food_n=alphas_emb./sum(alphas_non_emb_food);
alphas_non_emb_food_n=alphas_non_emb_food./sum(alphas_non_emb_food);
P_non_emb_food=prod(pf0_non_emb_food.^(alphas_non_emb_food_n));
% Getting number for Russia: P_non_emb_food(33) = 1.0084

%%% 
%Simple average price changes
%all embargoed = (19.82 + 6.27 + 5.85 + 5.42 + 2.22)/5 = 7.9%
%all non-embargoed = 0.27 % (mean of 33 products)
%non-embargoed food = {other food products, other animal products} = 
% (0.87 + 0.63)/2 = 0.75%
%%%

% Computing overall price index
P=prod(pf0_all.^(alphas));
% Getting number for Russia: P(33) = 1.0033

% Computing real wages
W_all=wf0_all./P';  
% Expenditures Xji in a long vector
PQ_vec   = reshape(PQ_all',1,J*N)';

% Computing bilateral trade flows in the counterfactual equilibrium
Dinp_om = Dinp_all./taup;                       
xbilattau_all = (PQ_vec*ones(1,N)).*Dinp_om; 

% Computing the export flows matrices in the base year and counterfactual equilibrium
xbilattau_exp=zeros(J*N,N);
for j=1:J
    xbilattau_exp(1+(j-1)*N:j*N,:)=xbilattau(1+(j-1)*N:j*N,:)-diag(diag(xbilattau(1+(j-1)*N:j*N,:)));
end

xbilattau_all_exp=zeros(J*N,N);
for j=1:J
    xbilattau_all_exp(1+(j-1)*N:j*N,:)=xbilattau_all(1+(j-1)*N:j*N,:)-diag(diag(xbilattau_all(1+(j-1)*N:j*N,:)));
end

% Computing total exports by country and sector in the counterfactual equilibrium
Ejnp=zeros(J,N);
for j=1:J
    Ejnp(j,:)=sum(xbilattau_all_exp(1+(j-1)*N:j*N,:));
end
 
% Computing export shares by sectors in the counterfactual equilibrium
shareEjnp=Ejnp./(ones(J,1)*sum(Ejnp)); 
shareEjnp_all=shareEjnp;

% Computing total exports by country and sector in the base year
Ejn=zeros(J,N);
for j=1:J
    Ejn(j,:)=sum(xbilattau_exp(1+(j-1)*N:j*N,:));
end
 
% Computing export shares by sectors in the base year
shareEjn=Ejn./(ones(J,1)*sum(Ejn)); 
shareEjn_all=shareEjn;

% Computing import growth
xbilattau_imp=zeros(N,N);

for i=1:N
    xbilattau_imp(i,:)=sum(xbilattau_exp(i:N:J*N,:));
end

xbilattau_all_imp=zeros(N,N);

for i=1:N
    xbilattau_all_imp(i,:)=sum(xbilattau_all_exp(i:N:J*N,:));
end
gimp_all=100*(xbilattau_all_imp./xbilattau_imp-1);