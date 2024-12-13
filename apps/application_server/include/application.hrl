%% 


-define(RepoGit,"https://github.com/joq62/application_specs.git").
-define(SpecsDir,"application_specs").
-define(CheckRepoInterval,60*1000).


%
-define(GetISA,string:chomp(os:cmd("uname -m"))).
-define(Daemon(ApplicationDir,ApplicationName),"./"++ApplicationDir++"/bin/"++ApplicationName++" "++"daemon").
-define(Foreground(ApplicationDir,ApplicationName),"./"++ApplicationDir++"/bin/"++ApplicationName++" "++"foreground &").
-define(FileExt,".application").
-define(DirExt,"container").

