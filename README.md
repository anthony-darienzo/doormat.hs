# doormat.hs
A Brick-based ncurses ssh greeter/tmux menu.

This program does two things. First, it is a TUI greeter which allows one to select/make/delete a TMUX session. It also allows you to set the DARKMODE 
environment variable. I use this for configuring colorschemes in e.g. neovim. Second, it allows calling `doormat --lookup VAR` to look up an environment 
variable in the TMUX local environment, falling back to the shell environment. I actually poll the DARKMODE var in neovim by calling `doormat --lookup 
DARKMODE`.
