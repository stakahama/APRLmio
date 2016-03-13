
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% file: read_opc.m
%% author: S. Takahama
%% date:May 2015
%% prepared for Radon project
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [averagedmatrix, status] = ...
	 ave_discrete_meas(Rexe, Rscript, time1, time2, X)

  sepcol = ',';
  seprow = 'x';

  formatvec = @(x,sep) strjoin(strtrim(cellstr(num2str(x))),sep);
  dtime = @(dt) [dt;dt(end)];
  endtime = @(start) start+dtime(diff(start));

  start1 = formatvec(time1,seprow);
  start2 = formatvec(time2,seprow);
  end1 = formatvec(endtime(time1),seprow);
  end2 = formatvec(endtime(time2),seprow);
  datamatrix = {};
  for i = 1:size(X,1),
    datamatrix{i} = formatvec(X(i,:)',sepcol);
  end
  datamatrix = strjoin(datamatrix,seprow);

  syscommand = strjoin({Rexe, Rscript,...
			start1,end1,start2,end2,datamatrix},' ');

  [status, cmdout] = system(syscommand);

  averagedmatrix = cell2mat(textscan(cmdout,repmat('%f',1,size(X,2)),...
				     'Delimiter',','));

  return 
