# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

export CXX='g++ -std=c++0x'

# User specific aliases and functions
alias emacs='emacs -nw'
alias cmr='rm -f CMakeCache.txt && cmake .'
alias m='mysql -h 127.0.0.1'
export LD_LIBRARY_PATH=/usr/local/lib
alias m='mysql -h 127.0.0.1 -u root --prompt=memsql\> '
alias cmd='rm -f CMakeCache.txt && cmake -DCMAKE_BUILD_TYPE=Debug .'
alias gd='git diff --ignore-space-at-eol'
export PATH=/usr/lib64/ccache/:$PATH:~/arcanist/bin
export EDITOR='emacs -nw'
export GIT_EDITOR='emacs -nw'
alias t='~/memsql/memsqltest/t'
alias i='killall -TRAP mysqld'
alias e='emacs'
alias f='find . | xargs grep -i'
alias ll='ls -al'
