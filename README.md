## Introduction

tox.el is an Emacs mode interface to tox the virtualenv-based
automation of test activities, see https://pypi.python.org/pypi/tox

## Usage

In python mode if you are on a test and do `M-x tox-current-test` the
current test will be launched with tox in a compilation buffer. Use
`M-x tox-current-class` to test the current class.

With a prefix to the test function `tox.el` will ask you for a tox
environement (which is by default what specified in `tox-default-env`)
parsing your tox.ini.

## Install

You can install `tox.el` from
[MELPA](https://github.com/milkypostman/melpa.git) with package.el
(`M-x package-install tox`).

And you can also install it with [el-get](https://github.com/dimitri/el-get).

## Contributing

1. Fork it!
2. Create your feature branch: `git checkout -b my-new-feature`
3. Commit your changes: `git commit -am 'Add some feature'`
4. Push to the branch: `git push origin my-new-feature`
5. Submit a pull request :D

## License

Apache
