#!/usr/bin/env bash
#
# init.sh -- initializer script for my dotfiles
#
# Copyright 2019 (c) Cj-bc
# This software is released under MIT License
#
# @(#) version -

DOTFILE_PATH=$(realpath "${BASH_SOURCE[0]%%/*}")

while IFS=',' read -r target dst;do
  [[ ! -f "$target" ]] && continue
  [[ -f "$dst" ]] && echo "aborting ${target}: ${dst} already exist" && continue

  echo -n "linking: ${target} -> ${dst}..."
  ln -s "${DOTFILE_PATH}/${target}" "${dst}" &&
    echo 'ok' ||
    echo 'failed'
done < ./config.txt
