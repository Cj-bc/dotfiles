# vim configuration files

My vim configuration files are based on [mattn's ぼくがかんがえたさいきょうの Vim のこうせい 2019年 年末版](https://mattn.kaoriya.net/software/vim/20191231001537.htm)

| file                 | deploy to                          | description                                                           |
|----------------------|------------------------------------|-----------------------------------------------------------------------|
| .vim                 | ~/.vim                             | .vim file, which is nomaly exist.                                     |
| vimrc                | ~/.vimrc                           | previous 'vimrc.core'. Configure all global settings and load plugins |
| vimrc.plug.installer | ~/.vim/plugin/vimrc.plug.installer | plugin installer script                                               |
| UltiSnips            |                                    | Snippets for UltiSnips                                                |
|                      |                                    |                                                                       |


# relation between files

Those config files has strange relations.

         source
        +------> vimrc.core
 vimrc -+------> vimrc.plug.min
        +------> vimrc.plug.full        use to install
        +------> vimrc.plug.installer -+---------------> plug.min
                                       +---------------> plug.full
