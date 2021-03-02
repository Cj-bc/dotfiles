vim9script

export var xdgDefault = {
          "XDG_DATA_HOME":   expand("$HOME/.local/share"),
          "XDG_CONFIG_HOME": expand("$HOME/.config"),
          "XDG_DATA_DIRS":   "/usr/local/share/:/usr/share/",
          "XDG_CONFIG_DIRS": "/etc/xdg",
          "XDG_CACHE_HOME": expand("$HOME/.cache"),
          "XDG_RUNTIME_DIR": "",
                     }

# GetPaths: Convert XDG variables to real path
export def GetPaths(xdgName: string): list<string>
  const envValue = getenv(xdgName)
  if has_key(environ(), xdgName) && envValue != ""
    return split(envValue, ":")
  endif

  # If that variable is either not defined or empty,
  # use default value
  return split(xdgDefault[xdgName], ":")
enddef


