%This script computes constructs the base year without aggregate trade deficits
%% Setting technical parameters
close all;
vfactor  = -0.2;    %adjustment factor
tol      = 1E-07;   %tolerance
maxit    = 1E+10;   %max number of iterations

%% Loading of variables
% Loading countries and sectors
mapping = readtable('..\data\embargo.csv');
countries = mapping.origins(~cellfun(@isempty, mapping.origins));
tradable_sectors =  mapping.sectors(~cellfun(@isempty, mapping.sectors));

% Counting sectors and countries
N=length(countries);                                                       % Total number of countries (38)
J_t = length(tradable_sectors);                                            % Number of tradable sectors (39)
J_nt=13;                                                                   % Number of non-traded sectors (13)
J=J_t+J_nt;                                                                % Total number of sectors (52)

% Loading trade flows and tariffs
tau2013=importdata('..\data\Tariffs.mat');                           % Matrix of bilateral tariffs
tau2013_emb=importdata('..\data\Trade_costs.mat');                   % Tariffs and embargo
tau=[1+tau2013/100; ones(J_nt*N,N)];                                       % Actual tariff vector
taup=[1+tau2013_emb/100; ones(13*N,N)];                                    % Counterfactual tariff vector
taup=tau;                                                                  % Counterfactual tariffs equal actual tariffs in the base year
tau_hat=taup./tau;                                                         % Change in tariffs 

% Loading parameters
G=importdata('..\data\IO.mat');                                      % IO coefficients
B=importdata('..\data\B.mat');                                       % Share of value added
load GO_zeroS                                                              % Gross Output 
T=importdata('..\data\T.mat');                                       % Dispersion of productivity (Thetas)
T=[1./T; ones(13,1)*(1/8.22)];                                             % Productivity of non-tradable sectors: theta=8.22
load alphas
load xbilat_base_zeroS                                                     % Trade flows
xbilat = xbilatp;

%% Calculations
% Domestic sales
x=zeros(J,N);
xbilat_domestic=xbilat./tau;         
for i=1:J
x(i,:)=sum(xbilat_domestic(1+(i-1)*N:i*N,:));
end
GO=max(GO,x); domsales=GO-x;

% Bilateral trade matrix
domsales_aux=domsales';
aux2=zeros(J*N,N);
for i=1:J
     aux2(1+(i-1)*N:i*N,:)=diag(domsales_aux(:,i));
end
xbilat=aux2+xbilat;

% Calculating expenditure shares
Xjn = sum(xbilat')'*ones(1,N);
Din=xbilat./Xjn;

% Calculating surpluses
xbilattau=xbilat./tau;

% Calculating X0 Expenditure
A = sum(xbilat');
for j = 1:1:J
    X0(j,:) = A(:,1+N*(j-1):N*j);
end

for j      = 1:1:J
    % Imports
    M(j,:) = (sum(xbilattau(1+N*(j-1):N*j,:)'))';
    for n  = 1:1:N
    % Exports
    E(j,n) = sum(xbilattau(1+N*(j-1):N*j,n))';
    end
end

Sn=sum(E)'-sum(M)';
Sn = Sn*0;

% Calculating Value Added 
VAjn=GO.*B;
VAn=sum(VAjn)';

%% Principal counterfactuals
VAn_u=VAn./400000;
Sn_u=Sn./400000;

[wf0,pf0,PQ,Fp,Dinp,ZW] = equilibrium_LC(tau_hat,taup,alphas,T,B,G,Din,J,N,maxit,tol,VAn_u,Sn_u,vfactor);

for j   = 1:1:J
    irow    = 1+N*(j-1):1:N*j;
    F(j,:) = sum((Din(irow,:)./tau(irow,:))');
end

save('initial_condition_2013_noS','alphas','T','B','G','Din','J','N','VAn','Sn','X0','F','M','E','xbilattau','tau')