<#
.SYNOPSIS
    init.ps1 -- initializer script for my dotfiles (Windows)

    PowerShell port of init.sh. Reads the same config.txt / external.csv
    files and creates the same symlinks, so both scripts stay in sync.

.NOTES
    Copyright 2019 (c) Cj-bc
    This software is released under MIT License

    Creating a symlink on Windows requires either:
      - Running this script from an elevated (Administrator) PowerShell, or
      - Enabling "Developer Mode" (Settings > Update & Security > For developers)
#>

$ErrorActionPreference = 'Stop'

$RepoRoot = $PSScriptRoot
$DotfilePath = Join-Path $RepoRoot 'dotfiles'

function Expand-DotfilePath {
    param([string]$Path)
    # config.txt / external.csv use "$HOME/..." notation; $HOME is already
    # a builtin PowerShell variable, so ExpandString resolves it for us.
    return $ExecutionContext.InvokeCommand.ExpandString($Path)
}

function Install-Symlink {
    param(
        [string]$Target,
        [string]$Dst
    )

    $dstParent = Split-Path -Parent $Dst
    if ($dstParent -and -not (Test-Path -LiteralPath $dstParent)) {
        New-Item -ItemType Directory -Path $dstParent -Force | Out-Null
        Write-Host "[Log] directory " -NoNewline
        Write-Host $dstParent -NoNewline -ForegroundColor Yellow
        Write-Host " was created"
    }

    Write-Host "linking: $Target -> $Dst..." -NoNewline

    $srcPath = Join-Path $DotfilePath $Target
    if (-not (Test-Path -LiteralPath $srcPath)) {
        Write-Host "skip" -NoNewline -ForegroundColor Yellow
        Write-Host "(target not found)"
        return
    }

    if (Test-Path -LiteralPath $Dst) {
        Write-Host "skip" -NoNewline -ForegroundColor Yellow
        Write-Host "(dst already exist)"
        return
    }

    try {
        New-Item -ItemType SymbolicLink -Path $Dst -Target $srcPath -ErrorAction Stop | Out-Null
        Write-Host "ok" -ForegroundColor Green
    } catch {
        Write-Host "failed" -ForegroundColor Red
        Write-Host "  -> $($_.Exception.Message)" -ForegroundColor Red
        Write-Host "  -> try running this script as Administrator, or enable Developer Mode" -ForegroundColor Red
    }
}

# Reading and placing local config files
Get-Content (Join-Path $RepoRoot 'config.txt') | ForEach-Object {
    $line = $_.Trim()
    if (-not $line -or $line.StartsWith('#')) { return }

    $parts = $line -split ',', 2
    if ($parts.Count -lt 2) { return }

    $target = $parts[0].Trim()
    $dst = Expand-DotfilePath $parts[1].Trim()

    Install-Symlink -Target $target -Dst $dst
}

# External programs to install
Get-Content (Join-Path $RepoRoot 'external.csv') | ForEach-Object {
    $line = $_.Trim()
    if (-not $line -or $line.StartsWith('#')) { return }

    $parts = $line -split ',', 3
    if ($parts.Count -lt 3) { return }

    $utility = $parts[0].Trim()
    $target = $parts[1].Trim()
    $dst = Expand-DotfilePath $parts[2].Trim()

    if (Test-Path -LiteralPath $dst) {
        Write-Host "linking: $target -> $dst..." -NoNewline
        Write-Host "skip" -NoNewline -ForegroundColor Yellow
        Write-Host "(dst already exist)"
        return
    }

    $dstParent = Split-Path -Parent $dst
    if ($dstParent -and -not (Test-Path -LiteralPath $dstParent)) {
        New-Item -ItemType Directory -Path $dstParent -Force | Out-Null
    }

    switch ($utility) {
        'git' {
            Write-Host "[Git] installing: $target -> $dst..." -NoNewline
            git clone $target $dst
            Write-Host "ok" -ForegroundColor Green
        }
        default {
            Write-Host "Unknown utility: $utility; Skipping $target -> $dst"
        }
    }
}
