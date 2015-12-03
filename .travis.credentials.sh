#!/bin/bash
mkdir ~/.bintray/
BINTRAY_FILE=$HOME/.bintray/.credentials
ARTIFACTORY_FILE=$HOME/.bintray/.artifactory
cat <<EOF >$BINTRAY_FILE
realm = Bintray API Realm
host = api.bintray.com
user = maheshkelkar
password = 1d80239470569715ab634a0363c36e27ac12f9e6
EOF

cat <<EOF >$ARTIFACTORY_FILE
realm = Artifactory Realm
host = oss.jfrog.org
user = maheshkelkar
password = 1d80239470569715ab634a0363c36e27ac12f9e6
EOF
