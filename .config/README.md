# TAKEN FROM
https://www.atlassian.com/git/tutorials/dotfiles
# Starting from scratch
## Start a git bare repo at home
this will start a git repo at home.
`git init --bare $HOME/<repoName> ` 
## Define alias in current shell scope
so you can use dotfiles as if it were git for your dotfiles.
`alias dotfiles='/usr/bin/git --git-dir=$HOME/<repoName>/ --work-tree=$HOME'`
## Add local git config to repo
this will hide any untracked file from git log, etc...
`dotfiles config --local status.showUntrackedFiles no`
## Add alias to your shell 
i use zsh so it's in ~/.zshrc, other usefull aliases :
```
alias dotfiles='/usr/bin/git --git-dir=$HOME/<repwName>/ --work-tree=$HOME'
alias dotstatus='/usr/bin/git --git-dir=$HOME/<repwName>/ --work-tree=$HOME status'
alias dotlg='/usr/bin/git --git-dir=$HOME/<repwName>/ --work-tree=$HOME lg'
alias dotadd='/usr/bin/git --git-dir=$HOME/<repwName>/ --work-tree=$HOME add -u'
alias dotcommit='/usr/bin/git --git-dir=$HOME/<repwName>/ --work-tree=$HOME commit'
alias dotpush='/usr/bin/git --git-dir=$HOME/<repwName>/ --work-tree=$HOME push'
```
---
# Install in new system
## Ignore your repo directory
add <repoName> to a .gitignore file to prevent recursion problems.
`echo "<repoName>" >> .gitignore`
## Clone repo
`git clone --bare <git-repo-url> $HOME/<repoName>`
## Define the alias in the current shell scope
`alias config='/usr/bin/git --git-dir=$HOME/<repoName>/ --work-tree=$HOME'`
# Checkout the actual content from the bare repository to your $HOME
`dotfiles checkout`
you might get an error similar to this:
```
error: The following untracked working tree files would be overwritten by checkout:
    .bashrc
    .gitignore
Please move or remove them before you can switch branches.
Aborting
```
This is because your $HOME folder might already have some stock configuration
files  which would be overwritten by Git. The solution is simple: back up the
files if you care about them, remove them if you don't care. I provide you with
a possible rough shortcut to move all the offending files automatically to a
backup folder:
```
mkdir -p .config-backup && \
config checkout 2>&1 | egrep "\s+\." | awk {'print $1'} | \
xargs -I{} mv {} .config-backup/{}
```
rerun `dotfiles checkout` if you had the error.
# Set the showUntrackedFiles flat to no on local
`dotfiles config --local status.showUntrackedFiles no`
# This is a handy script that does all of the above:
```
git clone --bare https://bitbucket.org/durdn/cfg.git $HOME/.cfg
function config {
   /usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME $@
}
mkdir -p .config-backup
config checkout
if [ $? = 0 ]; then
  echo "Checked out config.";
  else
    echo "Backing up pre-existing dot files.";
    config checkout 2>&1 | egrep "\s+\." | awk {'print $1'} | xargs -I{} mv {} .config-backup/{}
fi;
config checkout
config config status.showUntrackedFiles no
```
