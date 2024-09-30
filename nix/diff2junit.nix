{ buildDunePackage }:
buildDunePackage {
 pname = "diff2junit";
 version = "dev";
 duneVersion = "3";
 src = fetchGit {
 url = "https://gitlab-ci-token:${builtins.getEnv "CI_JOB_TOKEN"}@git.frama-c.com/codex/diff2junit";
 rev = "ac3d90b47e8f00b9736d82d590fce956275adfb0";
 };

nativeBuildInputs = [];
buildInputs = [];
checkInputs = [];
doCheck = true;
}