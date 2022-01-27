%This script computes the equilibrium in the base year with trade deficits (with IO linkages)
%% Setting technical parameters
close all;
vfactor  = -0.2;    %adjustment factor
tol      = 1E-07;   %tolerance
maxit    = 1E+10;   %max number of iterations

%% Loading of variables
% Loading countries and sectors
mapping = readtable('..\data\embargo.csv');
countries = mapping.origins(~cellfun(@isempty, mapping.origins));
tradable_sectors = mapping.sectors(~cellfun(@isempty, mapping.sectors));

% Counting sectors and countries
N=length(countries);                                                       % Total number of countries (38)
J_t = length(tradable_sectors);                                            % Number of tradable sectors (39)
J_nt=13;                                                                   % Number of non-traded sectors (13)
J=J_t+J_nt;                                                                % Total number of sectors (52)

% Loading trade flows and tariffs
xbilat2013=importdata('..\data\TradeMatrix.mat');                    % Trade flows in the base year
xbilat2013=xbilat2013*1000;                                                % Converting thousand dollars (BACI) to dollars
xbilat2013_nt=[xbilat2013;zeros(J_nt*N,N)];                                % Adding non-tradable sectors
tau2013=importdata('..\data\Tariffs.mat');                           % Matrix of bilateral tariffs
tau2013_emb=importdata('..\data\Trade_costs.mat');                   % Tariffs and embargo
tau=[1+tau2013/100; ones(J_nt*N,N)];                                       % Actual tariff vector
taup=[1+tau2013_emb/100; ones(J_nt*N,N)];                                  % Counterfactual tariff vector
taup=tau;                                                                  % Counterfactual tariffs equal actual tariffs in the base year
tau_hat=taup./tau;                                                         % Change in tariffs 

% Loading parameters
G=importdata('..\data\IO.mat');                                      % IO coefficients
B=importdata('..\data\B.mat');                                       % Share of value added
GO=importdata('..\data\GO.mat');                                     % Gross Output
GO=GO*1000000;                                                             % Converting from millions of dollars (GTAP) to dollars
T=importdata('..\data\T.mat');                                       % Dispersion of productivity (Thetas)
T=[1./T; ones(J_nt,1)*(1/8.22)];                                           % Productivity of non-tradable sectors: theta=8.22

%% Calculations
% Calculating Expenditures = trade flows*tariffs
xbilat=xbilat2013_nt.*tau;                     

% Calculating Domestic sales
x=zeros(J,N);
xbilat_domestic=xbilat./tau;         
for i=1:J
    x(i,:)=sum(xbilat_domestic(1+(i-1)*N:i*N,:));
end
GO=max(GO,x); 
domsales=GO-x;

% Bilateral trade matrix
domsales_aux=domsales';
aux2=zeros(J*N,N);
for i=1:J
     aux2(1+(i-1)*N:i*N,:)=diag(domsales_aux(:,i));
end
xbilat=aux2+xbilat;

% Calculating X0 Expenditure
A=sum(xbilat');
for j      = 1:1:J
    X0(j,:) = A(:,1+N*(j-1):N*j);
end

% Calculating Expenditure shares
Xjn = sum(xbilat')'*ones(1,N);
Din = xbilat./Xjn;

% Calculating surpluses
xbilattau = xbilat./tau;
for j      = 1:1:J
    % Imports
    M(j,:) = (sum(xbilattau(1+N*(j-1):N*j,:)'))';
    for n  = 1:1:N
    % Exports
    E(j,n) = sum(xbilattau(1+N*(j-1):N*j,n))';
    end
end

Sn=sum(E)'-sum(M)';

% Calculating Value Added 
VAjn=GO.*B;
VAn=sum(VAjn)';

 for n=1:N
    irow=1+J*(n-1):J*n;
    num(:,n)=X0(:,n)-G(irow,:)*((1-B(:,n)).*E(:,n));
end

for j   = 1:1:J
    irow    = 1+N*(j-1):1:N*j;
    F(j,:) = sum((Din(irow,:)./tau(irow,:))');
end

alphas=num./(ones(J,1)*(VAn+sum(X0.*(1-F))'-Sn)');
 
for j=1:J
    for n=1:N
        if alphas(j,n)<0
            alphas(j,n)=0;
        end
    end
end
alphas=alphas./(ones(J,1)*(sum(alphas)));
save('alphas', 'alphas');

%% Principal counterfactuals
VAn_u=VAn./400000;
Sn_u=Sn./400000;

[wf0, pf0, PQ, Fp, Dinp, ZW, Snp2] = equilibrium_LC(tau_hat,taup,alphas,T,B,G,Din,J,N,maxit,tol,VAn_u,Sn_u,vfactor);

% Computing Expenditures Xji (long vector)
PQ_vec   = reshape(PQ',1,J*N)';
Dinp_om = Dinp./taup;
xbilattau = (PQ_vec*ones(1,N)).*Dinp_om;
xbilatp = xbilattau.*taup;

% Calculating Gross output
for j=1:J
    GO(j,:) = sum(xbilattau(1+(j-1)*N : j*N,:));
end
save('GO', 'GO');

for j=1:J
    for n = 1:N
        xbilatp(n+(j-1)*N,n) = 0;
    end   
end

save('xbilat_base_year', 'xbilatp','Dinp','xbilattau');