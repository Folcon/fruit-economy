#! /usr/bin/env python3
import argparse, build_utils, functools, os

basedir = os.path.abspath(os.path.dirname(__file__) + '/..')

clojars = "https://repo.clojars.org"

@functools.lru_cache(maxsize=1)
def deps_clojure():
  return [
    build_utils.fetch_maven("org.clojure", "clojure", "1.11.0-alpha3"),
    build_utils.fetch_maven("org.clojure", "core.specs.alpha", "0.2.62"),
    build_utils.fetch_maven("org.clojure", "spec.alpha", "0.2.194")
  ]

@functools.lru_cache(maxsize=1)
def deps_compile():
  return deps_clojure() + [
    build_utils.fetch_maven('org.projectlombok', 'lombok', '1.18.22'),
    build_utils.fetch_maven('org.jetbrains', 'annotations', '20.1.0'),
  ]
