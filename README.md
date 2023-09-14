# setup-macports

This GitHub Action configure and installs MacPorts. It supports the
selection and deselection of port variants, the use of supplementary
port definitions sources, the installation of additional ports and the
caching of an installation.

[![Continuous Integration](https://github.com/melusina-org/setup-macports/actions/workflows/continuous-integration.yaml/badge.svg?branch=main)](https://github.com/melusina-org/setup-macports/actions/workflows/continuous-integration.yaml)

## Usage

Create a workflow file in the`.github/workflows` directory of your
working copy.  This workflow file should use a MacOS runner such as
`macos-latest` and use the branch `v1` of this action.


## Outcomes

Once the action has been executed, the `port` command from MacPorts is
available and can be used to install ports, create packages, and any
other task it supports. The ports installation is owned by the runner,
thus no root access is required to install ports.

An [example workflow](#example-workflow) and an [example parameter file](#example-parameters)
are available below. See the GitHub Help Documentation for
[Creating a workflow file](https://help.github.com/en/articles/configuring-a-workflow#creating-a-workflow-file)
to get started with GitHub workflows.


## Inputs

* `parameters` — Pathname to a configuration file for the SETUP-MACPORTS
   action. When no pathname is provided, the configuration is
   expected to be found under
   `.github/parameters/setup-macports.yaml`.
   It is however no error when this file is not present.


## Parameter file

The configuration file is in YAML and has the following format:

* `version: '2.8.1'` — The MacPorts version to install.
* `prefix: '/opt/local'` — The installation prefix to install MacPorts to.
  Currently the only supported value is '/opt/local'.
* `variants.select: []` — The list of selected variants in the global
  variants configuration. See Also variants.conf(5).
* `variants.deselect: []` — The list of deselected variants in the
  global variants configuration. See Also: variants.conf(5).
* `sources: ['rsync://rsync.macports.org/macports/release/tarballs/ports.tar']` — The list
  of source locations for port definitions. The first entry of the list is marked with default.
  See Also: sources.conf(5)
* `ports: []` — The list of additional ports to install. Each term of the
  list is a dictionart with the mandatory 'name' holding the name
  of the port to install, optional key 'select' for the list of
  selected variants for this port, and the optional 'key'
  deselect for the list of deselected variants for this port.


## Outputs

* `prefix` — The installation prefix to install MacPorts to.
* `version` — The MacPorts version to install.


## Cache scopes

The cache is scoped to a key deduced from the MacOS version and the
parameter file.


## Example worflow

```yaml
name: 'Run Testsuite'
on:
  - workflow_dispatch
  - push

jobs:
  install-macports-on-macos-11:
    runs-on: macos-11
    name: 'Install MacPorts 2.8.1 on MacOS 11'
    steps:
      - uses: actions/checkout@v3
      - uses: melusina-org/setup-macports@v1
        id: 'macports'
        with:
          parameters: 'testsuite/run-testsuite-on-macos-11.yaml'
      - name: 'Validate installed MacPorts version'
        run: >-
          test "$(port version)" = 'Version: 2.8.1'
      - name: 'Validate transmitted MacPorts prefix'
        run: >-
          test "${{ steps.macports.outputs.prefix }}" = '/opt/local'
      - name: 'Validate transmitted MacPorts version'
        run: >-
          test "${{ steps.macports.outputs.version }}" = '2.8.1'

  install-macports-on-macos-12:
    runs-on: macos-12
    name: 'Install MacPorts 2.8.1 on MacOS 12'
    steps:
      - uses: actions/checkout@v3
      - name: 'Run testsuite'
        run: development/testsuite
      - uses: melusina-org/setup-macports@v1
        with:
          parameters: 'testsuite/run-testsuite-on-macos-12.yaml'
      - run: port version
```


## Example parameters

```yaml
version: '2.8.1'
prefix: '/opt/local'
variants:
  select:
    - aqua
    - metal
  deselect: x11
ports:
  - name: db48
    deselect: [ java ]
    select: [ tcl, universal ]
  - name: gmp
    select: native

```


## License
The scripts and documentation in this project are released under the [MIT License](LICENSE)
