$BaseDirectory = $PSScriptRoot
$ModelsDirName = "models"
$ModelsFullPath = Join-Path -Path $BaseDirectory -ChildPath $ModelsDirName

if (Test-Path -Path $ModelsFullPath -PathType Container) {
    try {
        Remove-Item -Path $ModelsFullPath -Recurse -Force -ErrorAction Stop
        Write-Host "Dir '$ModelsFullPath' was deleted"
    } catch {
        Write-Error "Couldn't delete '$ModelsFullPath'. Error: $($_.Exception.Message)"
        exit 1
    }
}

Write-Host "Creating directory '$ModelsDirName'..."
try {
    New-Item -ItemType Directory -Path $ModelsFullPath -ErrorAction Stop | Out-Null
    Write-Host "Directory '$ModelsFullPath' was created."
} catch {
    Write-Error "Couldn't create '$ModelsFullPath'. Error: $($_.Exception.Message)"
    exit 1
}

try {
    Set-Location -Path $ModelsFullPath -ErrorAction Stop
    Write-Host "Changed to '$ModelsFullPath'."
} catch {
    Write-Error "Couldn't change to $ModelsFullPath'. Error: $($_.Exception.Message)"
    exit 1
}

$urls = @(
    "https://www.minizinc.org/challenge/2024/mznc2024_probs.tar.gz",
    "https://www.minizinc.org/challenge/2023/mznc2023_probs.tar.gz",
    "https://www.minizinc.org/challenge/2022/mznc2022_probs.tar.gz",
    "https://www.minizinc.org/challenge/2021/mznc2021_probs.tar.gz",
    "https://www.minizinc.org/challenge/2020/mznc2020-probs.tar.gz",
    "https://www.minizinc.org/challenge/2019/mznc2019-probs.tar.gz",
    "https://www.minizinc.org/challenge/2018/mznc2018-probs.tar.gz",
    "https://www.minizinc.org/challenge/2017/mznc2017-probs.tar.gz",
    "https://www.minizinc.org/challenge/2016/mznc2016-probs.tar.gz",
    "https://www.minizinc.org/challenge/2015/mznc2015-probs.tar.gz",
    "https://www.minizinc.org/challenge/2014/mznc2014-probs.tar.gz",
    "https://www.minizinc.org/challenge/2013/mznc2013-probs.tar.gz"
)

Write-Host "Starting downloading..."
foreach ($url in $urls) {
    $fileName = $url.Split('/')[-1]
    $outputFile = Join-Path -Path $ModelsFullPath -ChildPath $fileName
    Write-Host "Downloading $fileName..."
    try {
        Invoke-WebRequest -Uri $url -OutFile $outputFile -ErrorAction Stop
        Write-Host "Downloaded $fileName successfully."
    } catch {
        Write-Error "Couldn't download $fileName. Error: $($_.Exception.Message)"
    }
}

Write-Host "Decompressing files"
Get-ChildItem -Path $ModelsFullPath -Filter "mznc*.tar.gz" | ForEach-Object {
    $gzFile = $_.FullName
    $tarFile = $gzFile -replace '\.gz$', ''
    
    Write-Host "Decompressing $gzFile to $tarFile..."
    try {
        $fileStream = New-Object System.IO.FileStream $gzFile, ([IO.FileMode]::Open), ([IO.FileAccess]::Read), ([IO.FileShare]::Read)
        $gzipStream = New-Object System.IO.Compression.GZipStream $fileStream, ([IO.Compression.CompressionMode]::Decompress)
        $outputFileStream = New-Object System.IO.FileStream $tarFile, ([IO.FileMode]::Create), ([IO.FileAccess]::Write), ([IO.FileShare]::None)
        
        $gzipStream.CopyTo($outputFileStream)
        
        $gzipStream.Close()
        $fileStream.Close()
        $outputFileStream.Close()
        
        Remove-Item -Path $gzFile -Force -ErrorAction Stop
        Write-Host "Decompressed and deleted $gzFile."
    } catch {
        Write-Error "Couldn't decompress $gzFile. Error: $($_.Exception.Message)"
        if ($gzipStream) { $gzipStream.Dispose() }
        if ($fileStream) { $fileStream.Dispose() }
        if ($outputFileStream) { $outputFileStream.Dispose() }
    }
}

Write-Host "Extracting files..."
Get-ChildItem -Path $ModelsFullPath -Filter "*.tar" | ForEach-Object {
    $tarFile = $_.FullName
    Write-Host "Extracting $tarFile..."
    try {
        tar.exe xf $tarFile -C $ModelsFullPath
        Write-Host "Extracted $tarFile."
    } catch {
        Write-Error "Couldn't extract $tarFile using tar.exe. Make sure tar.exe is in PATH. Error: $($_.Exception.Message)"
    }
}

Write-Host "Removing files..."
try {
    Remove-Item -Path (Join-Path -Path $ModelsFullPath -ChildPath "*.tar") -Force -ErrorAction Stop
    Write-Host "Tar files has been deleted"
} catch {
    Write-Error "Couldn't remove. Error: $($_.Exception.Message)"
}

Write-Host "Changing dir names..."
$renameMap = @{
    "mznc2014_problems" = "mznc2014-probs"
    "mznc13-problems"   = "mznc2013-probs"
}

foreach ($entry in $renameMap.GetEnumerator()) {
    $oldNamePath = Join-Path -Path $ModelsFullPath -ChildPath $entry.Key
    $newName = $entry.Value
    
    if (Test-Path -Path $oldNamePath -PathType Container) {
        Write-Host "Changing name '$($entry.Key)' to '$newName'..."
        try {
            Rename-Item -Path $oldNamePath -NewName $newName -ErrorAction Stop
            Write-Host "Changed name '$($entry.Key)' to '$newName' successfully."
        } catch {
             Write-Error "Couldn't' change name '$($entry.Key)'. Error: $($_.Exception.Message)"
        }
    } else {
        Write-Warning "Dir '$($entry.Key)' was not found in '$ModelsFullPath'"
    }
}

Set-Location -Path $BaseDirectory

