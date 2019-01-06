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
- [Slides](#slides)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## GitLab CI

This project's static Pages are built by GitLab CI, following the steps
defined in [`.gitlab-ci.yml`](.gitlab-ci.yml):

Initial build may take some time (around 30 minutes), following builds will be significantly faster.

## Building locally

To work locally with this project:

1. Fork, clone or download this project
1. Install Hakyll.
1. Build site generator: `stack build`
1. Generate the website: `stack exec site build`
1. Preview your project: `stack exec site watch`
1. Add content

## Slides

Slides are made using [reveal-md](https://github.com/webpro/reveal-md). First, install this globally with `npm`.

1. `cd` into `/slides`
2. To serve, run `reveal-md -w -t serif SLIDES.md`.
