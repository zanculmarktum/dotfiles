## Description

This is the ideal dotfiles repository that doesn't clutter up the working tree with unwanted/untracked dotfiles.

Untracked dotfiles will never appear in `git status` ever, because git will only track the files that you specified in `.gitignore`.

How is that possible? The magic behind this is done by the `.gitignore` himself.

First, we exclude everything:

    *

Then, include the `.gitignore`:

    !.gitignore

To include the desired dotfiles, prefix it with `!`. For example:

    !.xinitrc

To include a folder, first add the folder then add that folder again but with `**` appended. Note that this will include everything inside the folder.

    !.vim
    !.vim/**

To include a folder but track only a specific files inside it:

(this will only track `mpv.conf`)

    !.config
    !.config/mpv
    !.config/mpv/mpv.conf

## Test

Now, let's compare this to normal `.gitignore`-less dotfiles repository:

    $ git status
    On branch master
    Untracked files:
      (use "git add <file>..." to include in what will be committed)

            .Xmodmap
            .Xresources
            .aria2/
            .bash_aliases
            .bash_completion
            .bash_functions
            .bash_miscs
            .bash_profile
            .bashrc
            .config/
            .conkerorrc
            .dwm/
            .gbp.conf
            .gitconfig
            .gitignore
            .gnupg/
            .gvimrc
            .infokey
            .inputrc
            .local/
            .nzbget
            .pystartup
            .quiltrc-dpkg
            .termux/
            .tmux.conf
            .urxvt/
            .vim/
            .vimrc
            .xbindkeysrc
            .xinitrc

    nothing added to commit but untracked files present (use "git add" to track)

You see that it's full of config files that you don't wish to be put in the repository.

The repository with `.gitignore` on the other hand though is clean:

    $ git status
    On branch master
    Your branch is up to date with 'origin/master'.

    nothing to commit, working tree clean

So, no matter how much programs you'd install in the future, their config files won't register ever in the repository. No more `git add .` by accident that will put unwanted files in the repository.

## Installation

    $ git clone --recurse-submodules https://github.com/zanculmarktum/dotfiles.git
    $ rm -rf ~/.git
    $ mv dotfiles/.git ~
    $ cd && git checkout .
    $ rm -rf dotfiles
