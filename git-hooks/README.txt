These Git hooks are to control other repositories, on intcvs1.

They are checked out at intcvs1:/repos/git/anacode/~githooks/tt/, and
then symlinked from the relevant bare repository.  See RT#275599
[intcvs1: Storage for Git checkouts of hooks?]



Q: If NFS breaks we can't even see our home directories.  Chances are
   our desktops will lock up.  Why worry about not being able to push?

A: /nfs/anacode (ons04-humgen) and homes (split between netapp1{a,b})
   can go down separately.  Laptops tend to stay up through this and
   may be pushing from the ssh gateway or internal wired network.


Q: what updates the checkout which is used for git-hooks?

A: currently manual, should probably stay manual


Q: will these hooks work for developer clones?

A: dunno, might be good to try


Q: where are git-hooks/server-config/ documented?

A: bin/server-config-op



Installation,
  ssh lenny-dev32  # currently has newer Git
  TTGH=/software/noarch/linux-i386/anacode/team_tools.git-hooks
# NB. since this happened, I have moved the directory
# and extended the hooks collection.
  iRGA=intcvs1:/repos/git/anacode
  umask 02
  git init --shared=all $TTGH
  cd $TTGH
  git remote add origin $iRGA/team_tools.git
  git fetch
  git reset --hard origin/master
  find $TTGH \! -perm -g+ws -type d -ls

  ssh intcvs1
  cd /repos/git/anacode/ensembl-otter.git
  ln -s /software/noarch/linux-i386/anacode/team_tools.git-hooks/git-hooks/ensembl-otter hooks+
  diff -ru hooks{+,}
  mv hooks{,~} && mv hooks{+,}
