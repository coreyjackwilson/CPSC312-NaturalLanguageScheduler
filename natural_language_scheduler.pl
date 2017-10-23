% Natural Language Shift Scheduler
% Corey Wilson
% 17400110

% Rules for the natural language parsing
% ======================================

% Example Questions
% -----------------
% When does X work?
% Who works today?
% Who works in department X?
% Who manages department X?








% Props for the data design
% =========================

% Shifts
% ------
shift(morning, 800, 1600, electronics).
shift(afternoon, 1200, 2000, electronics).

% Employees
% ---------
employee('Corey', 12345, 1, 40, morning, downtown_vancouver, clerk).
employee('Lyndon', 12346, 1, 40, morning, downtown_vancouver, clerk).

% Departments
% -----------
department(electronics, 1234).

% Stores
% ------
store(1, downtown_vancouver, '123 Howe St, Vancouver').

% Jobs
% ----
job(1, clerk, 12).
