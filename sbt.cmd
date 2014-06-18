@echo off
set SCRIPT_DIR=%~dp0
java %SBT_OPTS% -Xmx1024m -Xss16M -jar "%SCRIPT_DIR%sbt-launch.jar" %*
