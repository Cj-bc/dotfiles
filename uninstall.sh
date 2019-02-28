#!/usr/bin/env bash
#
# uninstall.sh -- uninstall script for my dotfiles
#
# Copyright 2019 (c) Cj-bc
# This software is released under MIT License
#
# @(#) version -

color_red="$(tput setaf 1)"
color_green="$(tput setaf 2)"
color_yellow="$(tput setaf 3)"
color_default="$(tput sgr0)"

while IFS=',' read -r target dst;do
  echo -n "Unlinking: ${dst}..."
  [[ ! -a "$(eval echo $dst)" ]] &&
    echo "${color_yellow}skip${color_default}(dst does not exist)" && continue

  eval "unlink $dst" &&
    echo "${color_green}ok${color_default}" ||
    echo "${color_red}failed${color_default}"
done < ./config.txt
