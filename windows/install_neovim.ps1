$ErrorActionPreference = "Stop"

Function New-TempFolder
{
    $TEMPFILE = New-TemporaryFile
    Remove-Item -Force $TEMPFILE

    return Join-Path $env:TEMP $TEMPFILE.Name
}

$DownloadLoc  = New-TemporaryFile
$RenamedFile = $DownloadLoc.BaseName + ".zip"
$ExtractFolder = New-TempFolder
$OutputFolder = Join-Path $HOME "AppData\Local\Programs\neovim"
$BinFolder = Join-Path "$OutputFolder" "bin"

Invoke-WebRequest "https://github.com/neovim/neovim/releases/download/nightly/nvim-win64.zip" -OutFile $DownloadLoc
Write-Output "Downloading neovim"
Rename-Item $DownloadLoc $RenamedFile
Expand-Archive -Path "$(Join-Path $DownloadLoc.Directory $RenamedFile)" -DestinationPath $ExtractFolder

mkdir -ErrorAction SilentlyContinue -Force !$ExtractFolder

Remove-Item -Recurse -Force $OutputFolder
Copy-Item -Recurse -Force -Path "$ExtractFolder\Neovim" -Destination $OutputFolder

$UserPath = [Environment]::GetEnvironmentVariable("Path",[System.EnvironmentVariableTarget]::User)

if (!$UserPath.Split(";").Contains($BinFolder))
{

    Write-Output Updating Path

    [Environment]::SetEnvironmentVariable("Path","$UserPath;$BinFolder",[System.EnvironmentVariableTarget]::User)

}





