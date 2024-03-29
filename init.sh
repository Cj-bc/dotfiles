#!/usr/bin/env bash
#
# init.sh -- initializer script for my dotfiles
#
# Copyright 2019 (c) Cj-bc
# This software is released under MIT License
#
# @(#) version -

DOTFILE_PATH=$(realpath "${BASH_SOURCE[0]%%/*}/dotfiles")
color_red="$(tput setaf 1)"
color_green="$(tput setaf 2)"
color_yellow="$(tput setaf 3)"
color_default="$(tput sgr0)"

# Reading and placing local config files
while IFS=',' read -r target dst;do
  [[ ! -d "$(eval echo ${dst%/*})" ]] && {
    eval "mkdir -p ${dst%/*}" && \
    echo "[${color_yellow}Log${color_default}] directory ${color_yellow}${dst%/*}${color_default} was created" || \
    continue
  }

  echo -n "linking: ${target} -> ${dst}..."
  [[ ! -a "${DOTFILE_PATH}/${target}" ]] &&
    echo "${color_yellow}skip${color_default}(target not found)" && continue
  [[ -a "$(eval echo $dst)" ]] &&
    echo "${color_yellow}skip${color_default}(dst already exist)" && continue

  eval "ln -s "${DOTFILE_PATH}/${target}" "${dst}" 2>/dev/null" &&
    echo "${color_green}ok${color_default}" ||
    echo "${color_red}failed${color_default}"
done < ./config.txt

# EXternal programs to install
while IFS=',' read -r utility target dst; do
  # Line start with '#' is comment, so I ignore them
  [[ ${utility} =~ ^# ]] && continue

  [[ -a "$(eval echo $dst)" ]] &&
    echo "${color_yellow}skip${color_default}(dst already exist)" && continue

  eval "mkdir -p ${dst%/*}"
  case $utility in
    "git")
      echo -n "[Git] installing: ${target} -> ${dst}..."
      eval "git clone "${target}" "${dst}""
      echo "${color_green}ok${color_default}"
      ;;
    *)
      echo "Unknown utlity: $utility; Skipping ${target} -> ${dst}" && continue
      ;;
    esac
done < ./external.csv
