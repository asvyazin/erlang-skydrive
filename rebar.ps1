$basedir = Split-Path -Parent $MyInvocation.MyCommand.Path
$rebar = Join-Path $basedir "rebar"
escript.exe $rebar $args
