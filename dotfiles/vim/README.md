# vim configuration files

here's all vim config files.

| file                 | deploy to                           | description                                                    |
|----------------------|-------------------------------------|----------------------------------------------------------------|
| .vim                 | ~/.vim                              | .vim file, which is nomaly exist.                              |
| vimrc                | ~/.vimrc                            | exist to set .vimrc. just source other conf files              |
| vimrc.core           | ~/.vim/profiles/vimrc.core          | main config. settings etc are in here.                         |
| plug.min             | ~/.vim/profiles/plug.min            | list of plugins for minimum version. contains only necessaries |
| plug.full            | ~/.vim/profiles/plug.full           | list of plugins for full version. contains toys, etc           |
| vimrc.plug.min       | ~/.vim/profiles/vimrc.plug.min      | settings for plug.min                                          |
| vimrc.plug.full      | ~/.vim/profiles/vimrc.plug.full     | settings for plug.full                                         |
| vimrc.plug.installer | ~/.vim/plugin/vimrc.plug.installer  | plugin installer script                                        |


# relation between files

Those config files has strange relations.

         source
        +------> vimrc.core
 vimrc -+------> vimrc.plug.min
        +------> vimrc.plug.full        use to install
        +------> vimrc.plug.installer -+---------------> plug.min
                                       +---------------> plug.full
