% This function computes welfare effects and produces output tables.
function [] = welfare_stats(ToTL_all,VoTL_all,ToTEjn_all, ToTMjn_all,...
    VoTXjn_all,P,pf0_all)

ToTL=ToTL_all; VoTL=VoTL_all;
Welfare = ToTL+VoTL; ToTEjn=ToTEjn_all; ToTMjn=ToTMjn_all;
VoTXjn=VoTXjn_all;
Prices=P';

% Loading countries and sectors
mapping = readtable('..\data\embargo.csv');
countries = char(mapping.origins(~cellfun(@isempty, mapping.origins)));
sectors = mapping.sectors(~cellfun(@isempty, mapping.sectors));

% Counting sectors and countries
N=length(countries);                                                       % Total number of countries (38)
J_t = length(sectors);                                                     % Number of tradable sectors (39)
J_nt=13;                                                                   % Number of non-traded sectors (13)
J=J_t+J_nt;                                                                % Total number of sectors (52)

%% Computing the welfare effects of embargo
% Tables 9-10
disp('_________________________________________________________________');
disp('                             Tables 9-10                         ');
disp('                   Welfare effects from embargo                  ');
disp('Country          Total     ToT      VoT   Price index            ');
disp('_________________________________________________________________');
to_save1=cell(N, 4);
for k=1:N
    disp([countries(k,:) ' ' num2str(((Welfare(k)))*100,3) '%    ' num2str((ToTL(k))*100,3) '%    ' num2str((VoTL(k))*100,3) '%  ' num2str((Prices(k)-1)*100,2) '%'] );
    to_save1(k,1)=num2cell(((Welfare(k)))*100,3);
    to_save1(k,2)=num2cell((ToTL(k))*100,3);
    to_save1(k,3)=num2cell((VoTL(k))*100,3);
    to_save1(k,4)=num2cell((Prices(k)-1)*100,2);
end
disp('_________________________________________________________________');
disp('>> The welfare outcomes for each country are saved to results/welfare_effects.xlsx <<');

% Save welfare outcomes to the Excel file
effects_type_header={'Countries','Total','ToT','VoT','Price index'};
filename = '../results/welfare_effects.xlsx';
delete(filename)
xlswrite(filename,to_save1,'Sheet1','B2');                                  % Write data
xlswrite(filename,effects_type_header,'Sheet1','A1');                       % Write names of columns
xlswrite(filename,cellstr(countries),'Sheet1','A2');                        % Write names of countries (rows)

%% Compute contribution of each sector (by country) to welfare outcomes
disp('_________________________________________________________________');
disp('  Sectoral contributions to welfare effects from Russian embargo ');
to_save2='';
for k=1:N
    to_save2_add=[num2cell(((ToTEjn(1:J,k)-ToTMjn(1:J,k)+VoTXjn(1:J,k))./nansum((ToTEjn(1:J,k)-ToTMjn(1:J,k)+VoTXjn(1:J,k))))*100,3) ...    
              num2cell(((ToTEjn(1:J,k)-ToTMjn(1:J,k))./nansum((ToTEjn(1:J,k)-ToTMjn(1:J,k))))*100,3) ...
              num2cell(((VoTXjn(1:J,k))./nansum((VoTXjn(1:J,k))))*100,3)];
    to_save2=cat(2,to_save2,to_save2_add);
end

% Save sectoral contributions to welfare changes to the Excel file
countries_header='';
countries = mapping.origins(~cellfun(@isempty, mapping.origins));
for k=1:N
    add=[countries(k) countries(k) countries(k)];
    countries_header=cat(2,countries_header,add);
end

add_type = cell(1, 3*N);
add_type(:)={'Welfare'};
add_type(2:3:end)={'ToT'};                                                  % Replace every second element
add_type(3:3:end)={'VoT'};                                                  % Replace every third element

filename = '../results/sectoral_contribution.xlsx';
delete(filename)
xlswrite(filename,cellstr(sectors),'Sheet1','A3');                          % Write row header (sectors)
xlswrite(filename,countries_header,'Sheet1','B1');                          % Write column header (countries)
xlswrite(filename,add_type,'Sheet1','B2');                                  % Write column header (Welfare, ToT and VoT)
xlswrite(filename,to_save2,'Sheet1','B3');                                  % Write data
disp('>> Contribution of each sector to welfare outcomes (by country) is saved to results/sectoral_contribution.xlsx <<');
disp('_________________________________________________________________');

%% Save changes in prices for each sector (by country) to the Excel file
countries_header='';
for k=1:N
    add=countries(k);
    countries_header=cat(2,countries_header,add);
end

filename = '../results/prices_countries_sectors.xlsx';
delete(filename)
xlswrite(filename,cellstr(sectors),'Sheet1','A2');                          % Write row header (sectors)
xlswrite(filename,countries_header,'Sheet1','B1');                          % Write column header (countries)
xlswrite(filename,pf0_all(1:J,1:N),'Sheet1','B2');                          % Save prices to file

disp('_________________________________________________________________');
disp('  Changes in prices for each sector (by country)    ');
disp('>> Changes in prices for each sector (by country) are saved to results/prices_countries_sectors.xlsx <<');
disp('_________________________________________________________________');