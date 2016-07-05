![Build Status](https://gitlab.com/pages/hakyll/badges/master/build.svg)

---

Example [Hakyll] website using GitLab Pages.

Learn more about GitLab Pages at https://pages.gitlab.io and the official
documentation http://doc.gitlab.com/ee/pages/README.html.

---

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [GitLab CI](#gitlab-ci)
- [Building locally](#building-locally)
- [GitLab User or Group Pages](#gitlab-user-or-group-pages)
- [Did you fork this project?](#did-you-fork-this-project)
- [Getting help](#getting-help)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## GitLab CI

This project's static Pages are built by [GitLab CI][ci], following the steps
defined in [`.gitlab-ci.yml`](.gitlab-ci.yml):

```
image: haskell:7.10.3

pages:
  cache:
    paths:
      - .cabal-sandbox
      - cabal.sandbox.config
      - _cache
  before_script:
    - cabal sandbox init
    - cabal update
    - cabal install -j
  script:
    - cabal run site build
  artifacts:
    paths:
      - public
  only:
    - master
```

## Building locally

To work locally with this project, you'll have to follow the steps below:

1. Fork, clone or download this project
1. [Install][] Hakyll.
1. Generate the website: `stack exec site build`
1. Preview your project: `stack exec site watch`
1. Add content

Read more at Hakyll's [documentation][hakyll].

## GitLab User or Group Pages

To use this project as your user/group website, you will need one additional
step: just rename your project to `namespace.gitlab.io`, where `namespace` is
your `username` or `groupname`. This can be done by navigating to your
project's **Settings**.

Read more about [user/group Pages][userpages] and [project Pages][projpages].

## Did you fork this project?

If you forked this project for your own use, please go to your project's
**Settings** and remove the forking relationship, which won't be necessary
unless you want to contribute back to the upstream project.

## Getting help
* [Tutorials](https://jaspervdj.be/hakyll/tutorials.html)
* [Google discussion group](https://groups.google.com/forum/#!forum/hakyll)
* [Hakyll on StackOverflow](https://stackoverflow.com/questions/tagged/hakyll)

---

Forked from https://gitlab.com/jtojnar/hakyll

[ci]: https://about.gitlab.com/gitlab-ci/
[hakyll]: https://jaspervdj.be/hakyll/
[install]: https://jaspervdj.be/hakyll/tutorials/01-installation.html
[documentation]: http://link-to-main-documentation-page
[userpages]: http://doc.gitlab.com/ee/pages/README.html#user-or-group-pages
[projpages]: http://doc.gitlab.com/ee/pages/README.html#project-pages
