#!/usr/bin/env bash
#
# init.sh -- initializer script for my dotfiles
#
# Copyright 2019 (c) Cj-bc
# This software is released under MIT License
#
# @(#) version -

DOTFILE_PATH=$(realpath "${BASH_SOURCE[0]%%/*}/dotfiles")

while IFS=',' read -r target dst;do
  [[ ! -a "${DOTFILE_PATH}/${target}" ]] && continue
  [[ -f "$dst" ]] && echo "aborting ${target}: ${dst} already exist" && continue

  echo -n "linking: ${target} -> ${dst}..."
  eval "ln -s "${DOTFILE_PATH}/${target}" "${dst}" 2>/dev/null" &&
    echo 'ok' ||
    echo 'failed'
done < ./config.txt
