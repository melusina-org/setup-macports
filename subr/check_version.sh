export selfupdate_macports="false"
export macports_version="$1"

if [ "$1" = "" ]; then
	macports_release_url=$(curl https://raw.githubusercontent.com/macports/macports-base/master/config/RELEASE_URL || curl https://trac.macports.org/export/HEAD/macports-base/config/RELEASE_URL || curl https://distfiles.macports.org/MacPorts/RELEASE_URL)
	export macports_version="$(echo $macports_release_url | awk -F'/v' '{ print $NF }')"
	export selfupdate_macports="true"
fi