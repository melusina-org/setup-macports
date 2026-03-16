# macos.sh — Functions for MacOS

# Melusina Actions (https://github.com/melusina-org/setup-macports)
# This file is part of Melusina Actions.
#
# Copyright © 2022–2023 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

ensure_macos()
{
    case $(uname) in
	Darwin)
	    return 0
	    ;;
    esac

    failwith '\047%s\047 is not a MacOS platform.\n' $(uname) 
    exit 1
}

known_macos_db()
{
    cat <<'EOF'
10.10-Yosemite
10.11-ElCapitan
10.12-Sierra
10.13-HighSierra
10.14-Mojave
10.15-Catalina
10.6-SnowLeopard
10.7-Lion
10.8-MountainLion
10.9-Mavericks
11-BigSur
12-Monterey
13-Ventura
14-Sonoma
15-Sequoia
26-Tahoe
EOF
}

probe_macos()
{
    {
	known_macos_db
	printf 'EOF\n'
	system_profiler SPSoftwareDataType
    } | awk -F '-' '
BEGIN {
  mode = "readdb"
}

$0 == "EOF" {
  mode = "readprofile"
}

mode == "readdb" {
  macos[$1] = $2 
}

/System Version/ {
  if(match($0, "10[.][0-9]+")) {
    version = substr($0, RSTART, RLENGTH)
  }
  else if(match($0, "[0-9]+[.][0-9]+")) {
    version = substr($0, RSTART, RLENGTH)
    gsub("[.][0-9]+*", "", version)
  }

}

END {
  if(version in macos) {
    print(macos[version])
  } else {
    print(":not-found")
  }
}

'
}

probe_architecture()
{
    case $(uname -m) in
	arm64)
	    echo "arm64"
	    ;;
	x86_64)
	    echo "x86_64"
	    ;;
	*)
	    echo "unknown"
	    ;;
    esac
}

# End of file `macos.sh'
