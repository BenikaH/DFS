clear; clc

% start stopwatch timer
tic;

% week number
week = 8;
% number of lineups
nLineups = 200;

% read player pool file
[num, txt, raw] = xlsread(['C:/Users/Matt/Documents/R/DFS/Data/Week ', ...
                           num2str(week), '/playerPool.xlsx']);
% read correlation matrix
rho = xlsread('C:/Users/Matt/Documents/R/DFS/Scripts/correlations.xlsx', ...
              'B2:J10');

% player info
pInfo = txt(2:end, 1:5);
% team
team = pInfo(:, 2);
% position
pos = pInfo(:, 3);
% number of players
m = size(pInfo, 1);
% player cost
pCost = num(:, 1);
% mean projection
pMean = num(:, 2);
% projection standard deviation
pSD = num(:, 3);

% covariance matrix
sigma = diag(pSD .^ 2);

posString = {'QB', 'RB1', 'RB2', 'WR1', 'WR2', 'WR3', 'TE', 'K', 'DST'};
teamString = {'ARI', 'ATL', 'BAL', 'BUF', 'CAR', 'CHI', 'CIN', 'CLE', ...
              'DAL', 'DEN', 'DET', 'GB', 'HOU', 'IND', 'JAC', 'KC', ...
              'MIA', 'MIN', 'NE', 'NO', 'NYG', 'NYJ', 'OAK', 'PHI', ...
              'PIT', 'STL', 'SD', 'SF', 'SEA', 'TB', 'TEN', 'WAS'};
nTeam = length(teamString);
iRB = ones(nTeam, 1);
iWR = ones(nTeam, 1);
jRB = 1;
jWR = 1;
for ii = 1:m
    iPos = strmatch(pos{ii}, posString);
    iTeam = strmatch(team{ii}, teamString);
    switch pos{ii}
        case 'RB'
            iPos = iPos(iRB(iTeam));
            iRB(iTeam) = iRB(iTeam) + 1;
        case 'WR'
            iPos = iPos(iWR(iTeam));
            iWR(iTeam) = iWR(iTeam) + 1;
    end
    jTeam = strmatch(team{ii}, team)';
    for jj = jTeam
        jPos = strmatch(pos{jj}, posString);
        switch pos{jj}
            case 'RB'
                jPos = jPos(jRB);
                jRB = jRB + 1;
            case 'WR'
                jPos = jPos(jWR);
                jWR = jWR + 1;
        end
        sigma(ii, jj) = rho(iPos, jPos) * pSD(ii) * pSD(jj);
        sigma(jj, ii) = sigma(ii, jj);
    end
    jRB = 1;
    jWR = 1;
end

% equality constraints
Aeq = [strncmp('QB', pos, m)';
       strncmp('RB', pos, m)';
       strncmp('WR', pos, m)';
       strncmp('TE', pos, m)';
       strncmp('K', pos, m)';
       strncmp('DST', pos, m)'];
Aeq = Aeq + 0;
beq = [1; 2; 3; 1; 1; 1];
% inequality constraints
A = pCost';
b = 6e4;
% binary variable constraint
xtype = repmat('B', 1, m);

% lineup with the maximum possible points
opt = opti('qp', zeros(m), -pMean, 'eq', Aeq, beq, 'ineq', A, b, ...
           'xtype', xtype);
% solve the MIQP problem
[x, fval, exitflag, info] = solve(opt);
x(x < 0.5) = 0;
% lineup points
pts = pMean' * x;
% lineup risk
risk = x' * sigma * x;
% lineup cost
cost = pCost' * x;
% lineup binary
xMat = x;
% lineup list
playerPos = pInfo(x > 0, [1 3]);
qb = playerPos(strmatch('QB', playerPos(:, 2)), 1);
rb = playerPos(strmatch('RB', playerPos(:, 2)), 1);
wr = playerPos(strmatch('WR', playerPos(:, 2)), 1);
te = playerPos(strmatch('TE', playerPos(:, 2)), 1);
kick = playerPos(strmatch('K', playerPos(:, 2)), 1);
dst = playerPos(strmatch('DST', playerPos(:, 2)), 1);
lineups(1, :) = [qb rb' wr' te kick dst];

% lineup with the minimum possible risk
opt = opti('qp', 2 * sigma, zeros(m, 1), 'eq', Aeq, beq, 'ineq', ...
           A, b, 'xtype', xtype);
% solve the MIQP problem
[x, minRisk, exitflag, info] = solve(opt);

% lineup with the maximum possible risk
% opt = opti('qp', -2 * sigma, zeros(m, 1), 'eq', Aeq, beq, 'ineq', ...
%            [A; -pMean'], [b; -110], 'xtype', xtype);
opt = opti('qp', -2 * sigma, zeros(m, 1), 'eq', Aeq, beq, 'ineq', ...
           A, b, 'xtype', xtype);
% solve the MIQP problem
[x, fval, exitflag, info] = solve(opt);
maxRisk = -fval;

% initialize loop parameters
rStep = (maxRisk - minRisk) / (nLineups - 1);
% optimization loop
for k = 1:nLineups
    disp(k);
%     r = risk(1) + ((maxRisk - rStep) - risk(1)) * rand;
    r = minRisk + rStep * (k - 1);
    % create OPTI object
    opt = opti('qp', zeros(m), -pMean, 'eq', Aeq, beq, 'ineq', A, b, ...
               'qcrow', sigma, zeros(m, 1), r, r + rStep, 'xtype', xtype);
    % solve the MIQP problem
    [x, fval, exitflag, info] = solve(opt);
    x(x < 0.5) = 0;
    if exitflag == 1
        % lineup points
        pts(k + 1) = pMean' * x;
        % lineup risk
        risk(k + 1) = x' * sigma * x;
        % lineup cost
        cost(k + 1) = pCost' * x;
        % lineup binary
        xMat(:, k + 1) = x;
%         tempExposure = sum(xMat, 2);
%         indExposure = find(tempExposure > 0.5 * nLineups);
%         if ~isempty(indExposure)
%             pMean(indExposure) = 0;
%         end
        % lineup list
        playerPos = pInfo(x > 0, [1 3]);
        qb = playerPos(strmatch('QB', playerPos(:, 2)), 1);
        rb = playerPos(strmatch('RB', playerPos(:, 2)), 1);
        wr = playerPos(strmatch('WR', playerPos(:, 2)), 1);
        te = playerPos(strmatch('TE', playerPos(:, 2)), 1);
        kick = playerPos(strmatch('K', playerPos(:, 2)), 1);
        dst = playerPos(strmatch('DST', playerPos(:, 2)), 1);
        lineups(k + 1, :) = [qb rb' wr' te kick dst];
    end
end

% only save unique lineups
[pts, ia, ic] = unique(pts);
risk = risk(ia);
cost = cost(ia);
xMat = xMat(:, ia);
lineups = lineups(ia, :);

% player exposure
exposure = sortrows([pInfo num2cell(sum(xMat, 2) / length(pts))], -6);
% write player exposure to file
cell2csv(['C:/Users/Matt/Documents/R/DFS/Data/Week ', num2str(week), ...
          '/exposure.csv'], exposure);
cell2csv('C:/Users/Matt/Dropbox/FanDuel/exposure.csv', exposure);
% write lineup list to file
cell2csv(['C:/Users/Matt/Documents/R/DFS/Data/Week ', num2str(week), ...
          '/bestLineups.csv'], lineups);
cell2csv('C:/Users/Matt/Dropbox/FanDuel/bestLineups.csv', lineups); 

% plot efficient frontier
plot(risk, pts, '.');
% set(gca, 'XLim', [floor(min(risk)) ceil(max(risk))]);
xlabel('Lineup variance');
ylabel('Lineup expected points');

% stop stopwatch timer
elapsedTime = toc;