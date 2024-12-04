param([string]$outputpath="~/.local/share/nvim/libsqlite.so")
$ErrorActionPreference = "Stop"


$URL="https://www.sqlite.org/download.html"
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12 <# using TLS 1.2 is vitally important #>


function parse-html {
    param (
        [string] $content
    )
    $HTML = New-Object -Com "HTMLFile"
    [string]$htmlBody = $Content
    $HTML.write([ref]$htmlBody)
    return $HTML
}

$request = Invoke-WebRequest -Uri $URL -UseBasicParsing
$parsed=parse-html $request.Content
$node = ($parsed.all | Where-Object tagName -eq "!" | Where-Object nodeValue -like "*Download Product Data for scripts to read*" ).nodeValue -split "`n"
$map= $value[1..$value.Length] | Join-String -Separator "`n" | ConvertFrom-csv
$downloadurl=($map |  Where-Object RELATIVE-URL -match "\d{4}\/sqlite-dll-win64-x64.*" )."RELATIVE-URL"

Function New-TempFolder
{

    $TEMPFILE = New-TemporaryFile

    Remove-Item -Force $TEMPFILE

    return Join-Path $env:TEMP $TEMPFILE.Name

}

$DOWNLOADLOC=New-TemporaryFile
$extractfolder = New-TempFolder

Invoke-WebRequest $downloadurl -OutFile $DOWNLOADLOC

Expand-Archive $DOWNLOADLOC -DestinationPath $extractfolder
ls $extract-folder

# [IDOMNodeIterator]$i= $HTML.createNodeIterator($HTML.body,0x80,[IDOMNodeFilter]::FILTER_ACCEPT,$false)


