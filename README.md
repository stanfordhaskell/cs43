![Build Status](https://gitlab.com/pages/hakyll/badges/master/build.svg)

---

# Stanford CS43: Course website

Course website for Stanford's CS43: Functional Programming Paradigms.

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

## Webpage Structure

- Class (Home Page)
    1. Intro
    1. Info
    1. Thanks
- Notes
    1. General Description
    1. ToC
        - Notes 1
        - Notes 2
        - etc.
- Resources
    1. Haskell Language
        1. Language Reference
        1. Build and Project Tools
    1. General FP
        1. History
        1. Interesting papers and posts
- GitLab

## GitLab CI

This project's static Pages are built by GitLab CI, following the steps
defined in [`.gitlab-ci.yml`](.gitlab-ci.yml):

```
image: haskell:7.10.3

pages:
  cache:
    paths:
      - _cache
      - .stack
  before_script:
    - export STACK_ROOT=`pwd`/.stack
    - stack install --only-dependencies
    - stack build
  script:
    - stack exec site build
  artifacts:
    paths:
      - public
  only:
    - master
```

Initial build may take some time (around 30 minutes), following builds will be significantly faster.

## Building locally

To work locally with this project:

1. Fork, clone or download this project
1. Install Hakyll.
1. Build site generator: `stack build`
1. Generate the website: `stack exec site build`
1. Preview your project: `stack exec site watch`
1. Add content
