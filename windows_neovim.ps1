if ($env:TEMP == "")
{
    $env:TEMP = "/tmp"
}
Function New-TempFolder
{
    $TEMPFILE = New-TemporaryFile
    Remove-Item -Force $TEMPFILE
    return Join-Path $env:TEMP $TEMPFILE.Name
}

$DownloadLoc  = New-TemporaryFile
$ExtractFolder = New-TempFolder
$OutputFile = Join-Path $HOME "neovim"
$BinFolder = Join-Path "$OutputFile" "bin"
if (!$env:PATH.Split(';').Contains($BinFolder)) 
{
    
}


Invoke-WebRequest "https://github.com/neovim/neovim/releases/download/nightly/nvim-win64.zip" -OutFile $DownloadLoc
Expand-Archive -Path $DownloadLoc -DestinationPath $ExtractFolder

Copy-Item -Recurse -Path "$ExtractFolder\Neovim\*" -Destination $OutputFile

