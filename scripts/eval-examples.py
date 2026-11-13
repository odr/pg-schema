#!/usr/bin/env python3
"""Check or update `-- >>>` examples in Haskell comments by running them in GHCi.

The examples are evaluated the way the Eval plugin of the Haskell Language
Server does it, so they can be written and refreshed in the editor and checked
here (e.g. in CI):

* A group is a run of consecutive `-- >>>` lines. Its expected output is the
  comment lines right after the group, up to an empty comment line (`--`),
  another group, a Haddock comment or code.
* All groups of a module are evaluated in one GHCi session in the context of the
  module (`:m *Module`), in the order they appear in the file. So a `$setup`
  group at the top of a module makes its definitions available to all groups
  below it.
* Warnings are switched off; errors are part of the output.
* With `--update` the actual output is written into the files, except output
  with a GHCi error or an exception (e.g. no database connection): such groups
  are reported and left as they are, unless `--accept-errors` is given.

Only line comments are supported (not `{- ... -}` blocks).

Usage:

    eval-examples.py [--update [--accept-errors]] [--cabal CMD] COMPONENT=SRCDIR...

COMPONENT is a cabal target for `cabal repl` (e.g. `lib:pg-schema`), SRCDIR is
its source directory (module names are file paths relative to it). All `.hs`
files under SRCDIR that contain `-- >>>` are checked.

Exit status: 0 if all examples match (or were updated), 1 if some example does
not match (and was not updated), 2 if GHCi could not be started.
"""
import argparse
import os
import re
import shlex
import subprocess
import sys

COMMENT = re.compile(r'^(\s*)-- ?(.*)$')
MARK = '<<<EVAL-EXAMPLES {}>>>'
MARK_RE = re.compile(r'<<<EVAL-EXAMPLES (\d+)>>>')
ERROR_RE = re.compile(r'^<interactive>:\d+:\d+: error|^\*\*\* Exception:')


class Group:
    def __init__(self, path, start, out_start, out_end, exprs, expected, indent):
        self.path, self.start = path, start
        self.out_start, self.out_end = out_start, out_end
        self.exprs, self.expected, self.indent = exprs, expected, indent


def is_example(line):
    m = COMMENT.match(line)
    return m is not None and m.group(2).startswith('>>>')


def ends_output(line):
    m = COMMENT.match(line)
    if m is None or m.group(2).strip() == '' or m.group(2).startswith('>>>'):
        return True
    return re.match(r'\s*-- [|^$*]', line) is not None


def parse(path):
    lines = open(path, encoding='utf-8').read().split('\n')
    groups, i = [], 0
    while i < len(lines):
        if not is_example(lines[i]):
            i += 1
            continue
        start, indent, exprs = i, COMMENT.match(lines[i]).group(1), []
        while i < len(lines) and is_example(lines[i]):
            exprs.append(COMMENT.match(lines[i]).group(2)[3:].strip())
            i += 1
        out_start, expected = i, []
        while i < len(lines) and not ends_output(lines[i]):
            expected.append(COMMENT.match(lines[i]).group(2))
            i += 1
        groups.append(Group(path, start, out_start, i, exprs, expected, indent))
    return groups


def source_files(srcdir):
    for root, _, files in os.walk(srcdir):
        for f in sorted(files):
            if f.endswith('.hs'):
                path = os.path.join(root, f)
                if '-- >>>' in open(path, encoding='utf-8').read():
                    yield path


def module_name(path, srcdir):
    return os.path.relpath(path, srcdir)[:-3].replace(os.sep, '.')


def run_component(cabal, component, srcdir):
    """Evaluate the examples of one component; return [(group, actual output)]."""
    groups, script = [], [
        ':set prompt ""', ':set prompt-cont ""', ':set -w',
        'System.IO.hSetBuffering System.IO.stdout System.IO.NoBuffering',
    ]
    for path in sorted(source_files(srcdir)):
        gs = parse(path)
        if not gs:
            continue
        script.append(':m *' + module_name(path, srcdir))
        for g in gs:
            script.append('System.IO.putStrLn "{}"'.format(MARK.format(len(groups))))
            script.extend(g.exprs)
            groups.append(g)
    if not groups:
        return []
    script += ['System.IO.putStrLn "{}"'.format(MARK.format(len(groups))), ':q']

    print('Evaluating {} example group(s) of {} ...'.format(len(groups), component),
          flush=True)
    p = subprocess.run(cabal + ['repl', '-v0', component], input='\n'.join(script) + '\n',
                       stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    outs, cur, before = {}, None, []
    for line in p.stdout.split('\n'):
        m = MARK_RE.fullmatch(line.strip())
        if m:
            cur = int(m.group(1))
            outs[cur] = []
        elif cur is None:
            before.append(line)
        else:
            outs[cur].append(line)
    if len(groups) not in outs:  # the session did not reach the end
        sys.stderr.write('\n'.join(before[-40:] + outs.get(max(outs, default=0), [])[-40:]) + '\n')
        sys.stderr.write('error: GHCi session for {} failed\n'.format(component))
        sys.exit(2)
    result = []
    for n, g in enumerate(groups):
        got = outs.get(n, [])
        while got and got[-1].strip() == '':
            got.pop()
        result.append((g, got))
    return result


def update_file(path, changes):
    lines = open(path, encoding='utf-8').read().split('\n')
    for g, got in sorted(changes, key=lambda c: -c[0].out_start):
        lines[g.out_start:g.out_end] = [
            g.indent + ('-- ' + l if l else '--') for l in got]
    open(path, 'w', encoding='utf-8').write('\n'.join(lines))


def main():
    ap = argparse.ArgumentParser(description=__doc__.split('\n')[0])
    ap.add_argument('--update', action='store_true',
                    help='write the actual output into the files')
    ap.add_argument('--accept-errors', action='store_true',
                    help='with --update: also write output with errors')
    ap.add_argument('--cabal', default='cabal',
                    help='cabal command, e.g. "cabal --builddir=dist-make"')
    ap.add_argument('targets', nargs='+', metavar='COMPONENT=SRCDIR')
    args = ap.parse_args()
    cabal = shlex.split(args.cabal)

    total, mismatches = 0, []
    for target in args.targets:
        component, sep, srcdir = target.partition('=')
        if not sep:
            ap.error('expected COMPONENT=SRCDIR, got ' + target)
        for g, got in run_component(cabal, component, srcdir):
            total += 1
            if got != g.expected:
                mismatches.append((g, got))

    for g, got in mismatches:
        print('\n{}:{}: example output differs'.format(g.path, g.start + 1))
        for e in g.exprs:
            print('  >>> ' + e)
        print('  expected:')
        print('\n'.join('    ' + l for l in g.expected) or '    (nothing)')
        print('  actual:')
        print('\n'.join('    ' + l for l in got) or '    (nothing)')

    print('\n{}/{} example groups match.'.format(total - len(mismatches), total))
    if not (mismatches and args.update):
        return 1 if mismatches else 0
    by_file, skipped = {}, []
    for g, got in mismatches:
        if not args.accept_errors and any(ERROR_RE.match(l) for l in got):
            skipped.append(g)
        else:
            by_file.setdefault(g.path, []).append((g, got))
    for path, changes in by_file.items():
        update_file(path, changes)
        print('updated ' + path)
    for g in skipped:
        print('{}:{}: not updated: the output has an error '
              '(use --accept-errors to write it)'.format(g.path, g.start + 1))
    return 1 if skipped else 0


if __name__ == '__main__':
    sys.exit(main())
