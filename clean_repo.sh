#!/bin/bash 

#WARNING
#Do not run this outside of the VM disk image 
#Otherwise the repo will have to be freshly cloned
#This will remove all git files along with preparation scripts
TO_DELETE="dist-newstyle .git* README_Zenodo.md create_disk_tar.sh create_source_tar.sh disk_image clean_repo.sh comments.md"
rm -rf $TO_DELETE
