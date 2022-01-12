#! /usr/bin/env python3
import argparse, base64, functools, glob, itertools, json, os, pathlib, platform, re, shutil, subprocess, tempfile, time, urllib.request, zipfile
from typing import List, Tuple

def get_arg(name):
  parser = argparse.ArgumentParser()
  parser.add_argument(f'--{name}')
  (args, _) = parser.parse_known_args()
  return vars(args).get(name.replace("-", "_"))

execdir = os.getcwd()
arch   = get_arg("arch") or {'AMD64': 'x64', 'x86_64': 'x64', 'arm64': 'arm64'}[platform.machine()]
system = get_arg("system") or {'Darwin': 'macos', 'Linux': 'linux', 'Windows': 'windows'}[platform.system()]
classpath_separator = ';' if platform.system() == 'Windows' else ':'
mvn = "mvn.cmd" if platform.system() == "Windows" else "mvn"

def classpath_join(entries):
  return classpath_separator.join(entries)

def parse_ref() -> str:
  ref = get_arg("ref") or os.getenv('GITHUB_REF')
  if ref and ref.startswith('refs/tags/'):
    return ref[len('refs/tags/'):]

def parse_sha() -> str:
  sha = get_arg("sha") or os.getenv('GITHUB_SHA')
  if sha:
    return sha[:10]

def makedirs(path):
  os.makedirs(path, exist_ok=True)

def rmdir(path):
  shutil.rmtree(path, ignore_errors=True)

def cat(iterables):
  return list(itertools.chain(*iterables))

def files(*patterns):
  return cat([glob.glob(pattern, recursive=True) for pattern in patterns])

def slurp(path):
  if os.path.exists(path):
    with open(path, 'r') as f:
      return f.read()

def copy_replace(src, dst, replacements):
  original = slurp(src)
  updated = original
  for key, value in replacements.items():
    updated = updated.replace(key, value)
  makedirs(os.path.dirname(dst))
  if updated != slurp(dst):
    print("Writing", dst)
    with open(dst, 'w') as f:
      f.write(updated)

def copy_newer(src, dst):
  if not os.path.exists(dst) or os.path.getmtime(src) > os.path.getmtime(dst):
    if os.path.exists(dst):
      os.remove(dst)
    makedirs(os.path.dirname(dst))
    shutil.copy2(src, dst)
    return True

def has_newer(sources, targets):
  mtime = time.time()
  for target in targets:
    if os.path.exists(target):
      mtime = min(mtime, os.path.getmtime(target))
    else:
      mtime = 0
      break
  for path in sources:
    if os.path.getmtime(path) > mtime:
      return True
  return False

def fetch(url, file):
  if not os.path.exists(file):
    print('Downloading', url)
    if os.path.dirname(file):
      makedirs(os.path.dirname(file))
    with open(file, 'wb') as f:
      f.write(urllib.request.urlopen(url).read())

def fetch_maven(group, name, version, classifier=None, repo='https://repo1.maven.org/maven2'):
  path = '/'.join([group.replace('.', '/'), name, version, name + '-' + version + ('-' + classifier if classifier else '') + '.jar'])
  file = os.path.join(os.path.expanduser('~'), '.m2', 'repository', path)
  fetch(repo + '/' + path, file)
  return file

def javac(sources, target, classpath = [], modulepath = [], add_modules = [], release = '11', opts=[]):
  makedirs(target)
  classes = {path.stem: path.stat().st_mtime for path in pathlib.Path(target).rglob('*.class') if '$' not in path.stem}
  newer = lambda path: path.stem not in classes or path.stat().st_mtime > classes.get(path.stem)
  new_sources = [path for path in sources if newer(pathlib.Path(path))]
  if new_sources:
    print('Compiling', len(new_sources), 'java files to', target + ':', new_sources)
    subprocess.check_call([
      'javac',
      '-encoding', 'UTF8',
      *opts,
      '--release', release,
      *(['--class-path', classpath_join(classpath + [target])] if classpath else []),
      *(['--module-path', classpath_join(modulepath)] if modulepath else []),
      *(['--add-modules', ','.join(add_modules)] if add_modules else []),
      '-d', target,
      *new_sources])
