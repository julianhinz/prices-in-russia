%This script computes welfare effects of Russian embargo (without IO linkages)
%% Setting technical parameters
close all;
vfactor  = -0.2;    %adjustment factor
tol      = 1E-07;   %tolerance
maxit    = 1E+10;   %max number of iterations

%% Loading of variables
% Loading initial equilibrium (2013 before embargo) without trade deficit
load initial_condition_2013_noS_NO_IO 

% Loading countries and sectors
mapping = readtable('..\data\embargo.csv');
countries = mapping.origins(~cellfun(@isempty, mapping.origins));
tradable_sectors = mapping.sectors(~cellfun(@isempty, mapping.sectors));

% Counting sectors and countries
N=length(countries);                                                       % Total number of countries (38)
J_t = length(tradable_sectors);                                            % Number of tradable sectors (39)
J_nt=13;                                                                   % Number of non-traded sectors (13)
J=J_t+J_nt;                                                                % Total number of sectors (52)                                                            % Number of non-traded sectors

% Loading trade flows and tariffs
tau2013=importdata('..\data\Tariffs.mat');                           % Matrix of bilateral tariffs
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
[wf0_all_oN,pf0_all_oN,PQ_all_oN,Fp_all_oN,Dinp_all_oN,ZW,Snp_all,c_all_oN] = equilibrium_LC(tau_hat,taup,alphas,T,B,G,Din,J,N,maxit,tol,VAn,Sn,vfactor);

% Calculating welfare, terms of trade and trade volume effects 
[Welfare_all_oN,ToTL_all_oN,VoTL_all_oN,ToTEjn_all_oN,ToTMjn_all_oN,VoTXjn_all_oN,votXjni,Xjni_dc_all_oN] = Welfarelineal(PQ_all_oN,Dinp_all_oN,c_all_oN,tau,taup,xbilattau,In,J,N);

% Expenditures Xji in a long vector
PQ_vec   = reshape(PQ_all_oN',1,J*N)';

% Computing bilateral trade flows in the counterfactual equilibrium
Dinp_om_oN = Dinp_all_oN./taup;
xbilattau_all_oN = (PQ_vec*ones(1,N)).*Dinp_om_oN;

%% Computing the welfare effects of embargo (without IO linkages) 
% Table D2 (Appendix)
disp('______________________________________')
disp('             Table D2               ')
disp('   Welfare effects from embargo      ')
disp('Country          Total Welfare effect')
disp('______________________________________')

countries = char(mapping.origins(~cellfun(@isempty, mapping.origins)));

to_save1=cell(length(countries), 2);
for k=1:N

disp([countries(k,:) ' ' num2str(((Welfare_all_oN(k)))*100,3) '%    ' ] )
to_save1(k)=num2cell(((Welfare_all_oN(k)))*100,3);
end

% Save welfare outcomes to the Excel file
filename = '../results/welfare_effects_no_io.xlsx';
delete(filename)
header_no_io={'Country','Welfare effects'};
xlswrite(filename,header_no_io,'Sheet1','A1');                             % Write names of columns
xlswrite(filename,cellstr(countries),'Sheet1','A2');                       % Write names of countries (rows)
xlswrite(filename,to_save1,'Sheet1','B2');                                 % Write data
disp('______________________________________')
disp('>> The welfare outcomes (model without IO linkages) for each country are saved to results/welfare_effects_no_io.xlsx <<');