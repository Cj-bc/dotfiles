<#
.SYNOPSIS
    uninstall.ps1 -- uninstall script for my dotfiles (Windows)

    PowerShell port of uninstall.sh. Removes the symlinks created by init.ps1
    according to config.txt.

.NOTES
    Copyright 2019 (c) Cj-bc
    This software is released under MIT License
#>

$ErrorActionPreference = 'Stop'

$RepoRoot = $PSScriptRoot

function Expand-DotfilePath {
    param([string]$Path)
    return $ExecutionContext.InvokeCommand.ExpandString($Path)
}

Get-Content (Join-Path $RepoRoot 'config.txt') | ForEach-Object {
    $line = $_.Trim()
    if (-not $line -or $line.StartsWith('#')) { return }

    $parts = $line -split ',', 2
    if ($parts.Count -lt 2) { return }

    $dst = Expand-DotfilePath $parts[1].Trim()

    Write-Host "Unlinking: $dst..." -NoNewline

    $item = Get-Item -LiteralPath $dst -Force -ErrorAction SilentlyContinue
    if (-not $item) {
        Write-Host "skip" -NoNewline -ForegroundColor Yellow
        Write-Host "(dst does not exist)"
        return
    }

    try {
        # Remove-Item without -Recurse only deletes the reparse point itself,
        # even for a symlinked directory, leaving the real target untouched.
        Remove-Item -LiteralPath $dst -Force -ErrorAction Stop
        Write-Host "ok" -ForegroundColor Green
    } catch {
        Write-Host "failed" -ForegroundColor Red
        Write-Host "  -> $($_.Exception.Message)" -ForegroundColor Red
    }
}
