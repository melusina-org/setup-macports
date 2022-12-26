# testsuite.sh — The testsuite function

# Install MacPorts (https://github.com/melusina-org/gha-install-macports)
# This file is part of Install MacPorts.
#
# Copyright © 2022 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

testsuite_usage()
{
    local status
    status='0'

    if [ "$#" -gt 0 ]; then

	status='64'
	case "$1" in
	    [0-9][0-9][0-9]|[0-9][0-9]|[0-9])
		status="$1"
		shift
		;;
	    *)
		:
	esac

	wlog 'Failure' "$@"
    fi

    cat <<'EOF'
Usage: testsuite TEST-1 [ TEST-2 … ]
 This runs TEST-1 and following, print diagnostic and return status.
Description:
 Each argument TEST-1 … is a shell function or a program which is
 executed in a dedicated foilder and without arguments. Its exit code
 is used to determine if the test succeeded or failed.

 The exit code of testsuite is succesful iff all tests are
 succesful. When at least one test fails, detailed diagnostic is
 printed.
Options:
 -h Call this help screen.
EOF
    exit "${status}"
}

provision_testsuitedir()
{
    testsuitedir=$(mktemp -d -t "testsuite-XXXXXX")
    trap 'reclaim_testsuitedir' INT TERM EXIT
    
    reclaim_testsuitedir()
    {
	rm -r -f "${testsuitedir:?}"
    }
}

testsuite_display_diagnostic()
{
    printf '================================================================================\n'
    printf 'Exit Code: %s\n' "$2"
    printf 'Standard Output:\n'
    cat "${testsuitedir}/$1/stdout"
    printf -- '--------------------------------------------------------------------------------\n'
    printf 'Standard Error:\n'
    cat "${testsuitedir}/$1/stderr"
    printf '================================================================================\n'
}

testsuite_capture_outcome()
(
    local testcommand
    testcommand="$1"

    case "${testcommand}" in
	./*)
	    testcommand="$(pwd)/${testcommand}"
	    ;;
    esac
    
    printf 'testsuite: %s: ' "$1"
    install -d "${testsuitedir}/$1/workdir"
    (
	export TOPLEVELDIR
	cd "${testsuitedir}/$1/workdir"
	"${testcommand}"\
	    1>"${testsuitedir}/$1/stdout"\
	    2>"${testsuitedir}/$1/stderr"
    )
    exitcode="$?"

    if [ "${exitcode}" -eq 0 ]; then
	printf 'Success\n'
    else
	printf 'Failure\n'
    fi
    printf '%s' "${exitcode}" > "${testsuitedir}/$1/exitcode"
    printf '%s|%s\n' "$1" "${exitcode}" >> "${testsuitedir}/RESULT"
    return "${exitcode}"
)

testsuite_main()
(
    local currenttest success exitcode

    provision_testsuitedir

    local OPTIND OPTION OPTARG

    OPTIND=1

    while getopts 'h' OPTION; do
        case ${OPTION} in
            h)
                usage
	        ;;
            *)
                usage 'testsuite: %s: Unsupported option.' "${OPTION}"
	        ;;
        esac
    done

    shift $(expr ${OPTIND} - 1)

    if [ "$#" -eq 0 ]; then
	set -- "${TOPLEVELDIR}/testsuite/"*/*
    fi 
    
    success=0
    touch "${testsuitedir}/RESULT"
    for test in "$@"; do
	testsuite_capture_outcome "${test}"
	exitcode="$?"
	if [ "${exitcode}" -ne 0 ]; then
	    success=1
	    testsuite_display_diagnostic "${test}" "${exitcode}"
	fi
    done
    exit "${success}"
)

# End of file `testsuite.sh'
