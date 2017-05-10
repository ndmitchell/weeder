$VAR=Invoke-WebRequest https://raw.github.com/ndmitchell/neil/master/misc/appveyor.ps1
$TEMP=New-TemporaryFile
$FILE=$TEMP.FullName + ".ps1"
$VAR.Content | Out-File $FILE
& $FILE weeder $args
