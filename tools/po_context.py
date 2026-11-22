#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
po_context.py - Adds context for translators to tero.pot and tero.*.po

For each entry it writes "#." comments (shown by Poedit as "Notes for
translators"), generated from the sources:

  - Where it appears: window, tab/group, menu path, context menus, toolbars.
  - What it is: menu command, button, checkbox, tooltip, message...
  - Default shortcut (from bin/Shortcuts/Tero.key).
  - For messages (procLocalize.pas): the unit and procedure that use them,
    and the placeholders to keep.
  - A description of what the feature does, taken from
    tools/po_descriptions.json (reviewed texts, written once).

Only the "#." lines are touched: translations, flags, translator comments
("# ...") and everything else is kept as it is. Running it again with the
same sources gives the same files. Lazarus keeps these comments when it
updates the .pot/.po, but new entries come without them: run the script
again after updating the language files in Lazarus.

Usage (from the root of the repository):

  python3 tools/po_context.py            update tero.pot and tero.*.po
  python3 tools/po_context.py --check    only report the files that would change
  python3 tools/po_context.py --missing  list the entries without description
  python3 tools/po_context.py --show ID  print the context of one entry

Only Python 3 is needed, no extra packages.
"""

import argparse
import json
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
SRC_DIR = os.path.join(ROOT, 'TeroSubtitler', 'src')
LANG_DIR = os.path.join(ROOT, 'TeroSubtitler', 'bin', 'Languages')
KEY_FILE = os.path.join(ROOT, 'TeroSubtitler', 'bin', 'Shortcuts', 'Tero.key')
DESC_FILE = os.path.join(HERE, 'po_descriptions.json')

MAX_PLACES = 4   # menus/toolbars listed for a command
MAX_USES = 3     # units/procedures listed for a message

# -----------------------------------------------------------------------------
# Friendly names
# -----------------------------------------------------------------------------

CLASS_NAMES = {
    'TAction': 'command (shown in menus and toolbars)',
    'TMenuItem': 'menu item',
    'TLabel': 'label',
    'TStaticText': 'label',
    'TCheckBox': 'checkbox',
    'TRadioButton': 'option button',
    'TButton': 'button',
    'TBitBtn': 'button',
    'TSpeedButton': 'button',
    'TToolButton': 'toolbar button',
    'TGroupBox': 'group box title',
    'TRadioGroup': 'option group title',
    'TCheckGroup': 'checkbox group title',
    'TTabSheet': 'tab title',
    'TPage': 'page title',
    'TPanel': 'panel text',
    'TEdit': 'text box',
    'TLabeledEdit': 'text box',
    'TComboBox': 'drop-down list',
    'TMemo': 'text area',
    'TListBox': 'list',
    'TCheckListBox': 'list',
    'TListView': 'list',
    'TTreeView': 'tree',
    'TSpinEdit': 'number box',
    'TFloatSpinEdit': 'number box',
    'TColorBox': 'color picker',
    'TTrackBar': 'slider',
    'TUWCheckBox': 'checkbox',
    'TUWRadioButton': 'option button',
    'TUWFlatButton': 'button',
    'TUWGroupBox': 'group box title',
    'TUWTimeEdit': 'time box',
    'TUWHotKey': 'shortcut box',
}

# Pages of the Settings window (TUWLayout), as the list on the left shows them
LAYOUT_NAMES = {
    'lyoMPV': 'Media player',
    'lyoFileTypeAssociations': 'File types',
}

PROP_NAMES = {
    'caption': '',
    'hint': 'tooltip',
    'text': 'default text',
    'texthint': 'placeholder text (greyed hint inside the box)',
}

# Context menus of the main window, by what the user right-clicks on
POPUP_NAMES = {
    'popVST': 'context menu of the subtitle list',
    'popVSTHeader': 'context menu of the subtitle list header',
    'popWAVE': 'context menu of the waveform/timeline',
    'popVideo': 'context menu of the video',
    'popMemo': 'context menu of the text and translation boxes',
    'popTimeEdit': 'context menu of the time boxes',
    'popCoolBarMain': 'context menu of the main toolbar (show/hide buttons)',
    'popCoolBarVideo': 'context menu of the video toolbar (show/hide buttons)',
    'popCoolBarWaveform': 'context menu of the waveform toolbar (show/hide buttons)',
    'popCoolBarEditor': 'context menu of the editor toolbar (show/hide buttons)',
    'popMRU': 'list of recent files',
    'popPlayRate': 'menu of the playback rate button',
    'popLoopCount': 'menu of the loop count button',
    'popDropInsert': 'menu shown when a file is dropped',
    'popSilentZones': 'menu of the silent zones button',
    'popTranscription': 'menu of the transcription button',
    'popDictionaries': 'menu of the dictionaries button',
    'popRes': 'menu of the resolution button',
}

FORM_NAMES = {
    'TfrmMain': 'Main window',
}

# -----------------------------------------------------------------------------
# .lfm reader
# -----------------------------------------------------------------------------


class Comp:
    __slots__ = ('name', 'cls', 'props', 'children', 'parent', 'form')

    def __init__(self, name, cls, parent):
        self.name = name
        self.cls = cls
        self.props = {}
        self.children = []
        self.parent = parent
        self.form = None


def pascal_string(expr):
    """Value of a Pascal string expression: 'a''b' #13#10 + 'c'."""
    out = []
    i, n = 0, len(expr)
    while i < n:
        c = expr[i]
        if c == "'":
            i += 1
            while i < n:
                if expr[i] == "'":
                    if i + 1 < n and expr[i + 1] == "'":
                        out.append("'")
                        i += 2
                        continue
                    i += 1
                    break
                out.append(expr[i])
                i += 1
        elif c == '#':
            m = re.match(r'#(\$[0-9A-Fa-f]+|\d+)', expr[i:])
            if not m:
                return None
            v = m.group(1)
            out.append(chr(int(v[1:], 16) if v.startswith('$') else int(v)))
            i += len(m.group(0))
        elif c in ' +\t\r\n':
            i += 1
        else:
            return None  # not a string
    return ''.join(out)


def read_lfm(path):
    with open(path, encoding='utf-8', errors='replace') as f:
        lines = f.read().splitlines()
    root = None
    stack = []
    i, n = 0, len(lines)
    obj_re = re.compile(r'^\s*(object|inherited|inline)\s+(\w+)\s*:\s*(\w+)')
    prop_re = re.compile(r'^\s*([\w.]+)\s*=\s*(.*)$')
    while i < n:
        line = lines[i]
        s = line.strip()
        i += 1
        m = obj_re.match(line)
        if m:
            comp = Comp(m.group(2), m.group(3), stack[-1] if stack else None)
            if stack:
                stack[-1].children.append(comp)
                comp.form = root
            else:
                root = comp
                comp.form = comp
            stack.append(comp)
            continue
        if s == 'end':
            if stack:
                stack.pop()
            continue
        m = prop_re.match(line)
        if not m or not stack:
            continue
        key, val = m.group(1), m.group(2).strip()
        if val.startswith('('):          # list of values
            while not val.rstrip().endswith(')') and i < n:
                val = lines[i].strip()
                i += 1
            continue
        if val.startswith('{'):          # binary data
            while not val.rstrip().endswith('}') and i < n:
                val = lines[i].strip()
                i += 1
            continue
        if val.startswith('<'):          # collection: item ... end>
            if val != '<>':
                depth = 0
                while i < n:
                    t = lines[i].strip()
                    i += 1
                    if t == 'item':
                        depth += 1
                    elif t in ('end', 'end>'):
                        depth -= 1
                        if t == 'end>' and depth <= 0:
                            break
                    elif t == '>':
                        break
            continue
        while val.endswith('+') and i < n:   # string on several lines
            val += lines[i].strip()
            i += 1
        if val[:1] in ("'", '#'):
            sval = pascal_string(val)
            if sval is not None:
                stack[-1].props[key.lower()] = sval
                continue
        stack[-1].props[key.lower()] = val
    return root


def walk(comp):
    yield comp
    for c in comp.children:
        yield from walk(c)


def clean(text):
    """Caption as the user sees it: no accelerator '&', one line."""
    text = re.sub(r'&(?!&)', '', text or '').replace('&&', '&')
    return ' '.join(text.split())


def humanize(name):
    name = re.sub(r'^(TfrmT?|frm|ToolBar|tlb|mnu|pop|act|btn|lbl|chk|cbo|grp|tab)', '', name)
    words = re.sub(r'([a-z0-9])([A-Z])', r'\1 \2', name).strip()
    return words[:1].upper() + words[1:]


# -----------------------------------------------------------------------------
# Context of the form entries
# -----------------------------------------------------------------------------


class FormInfo:
    def __init__(self, path, root):
        self.path = path
        self.file = os.path.basename(path)
        self.root = root
        self.by_name = {}
        for c in walk(root):
            self.by_name.setdefault(c.name.lower(), c)
        cap = clean(root.props.get('caption', ''))
        if root.cls in FORM_NAMES:
            self.title = FORM_NAMES[root.cls]
        elif cap and cap.lower() not in (root.name.lower(), 'form1'):
            self.title = 'window "%s"' % cap
        else:
            self.title = 'window "%s"' % humanize(root.cls)
        # who uses each action / popup
        self.users = {}
        for c in walk(root):
            for key in ('action', 'popupmenu'):
                v = c.props.get(key)
                if v and v[:1] not in ("'", '#'):
                    self.users.setdefault(v.lower(), []).append(c)

    def caption_of(self, comp):
        cap = comp.props.get('caption')
        if cap is None and 'action' in comp.props:
            act = self.by_name.get(comp.props['action'].lower())
            if act is not None:
                cap = act.props.get('caption')
        return clean(cap or '')

    def menu_path(self, item):
        """('Edit > Timings > X', 'menu'|'popup name')."""
        names = []
        c = item
        while c is not None and c.cls == 'TMenuItem':
            names.append(self.caption_of(c) or humanize(c.name))
            c = c.parent
        names.reverse()
        where = 'menu'
        if c is not None and c.cls != 'TMainMenu':
            where = POPUP_NAMES.get(c.name, 'menu "%s"' % humanize(c.name))
        return ' > '.join(names), where

    def location(self, comp):
        """Tabs and group boxes around a control."""
        parts = []
        c = comp.parent
        while c is not None and c is not self.root:
            if c.cls == 'TUWLayout' and c.name.startswith('lyo'):   # pages of Settings
                parts.append('page "%s"' % LAYOUT_NAMES.get(c.name, humanize(c.name[3:])))
            elif c.cls in ('TTabSheet', 'TGroupBox', 'TUWGroupBox', 'TRadioGroup', 'TCheckGroup', 'TPage'):
                cap = clean(c.props.get('caption', ''))
                if cap and cap.lower() != c.name.lower():
                    parts.append(cap)
            c = c.parent
        parts.reverse()
        return ' > '.join(parts)

    def places_of_action(self, act):
        places = []
        for u in self.users.get(act.name.lower(), []):
            if u.cls == 'TMenuItem':
                path, where = self.menu_path(u)
                if where == 'menu':
                    places.append('menu %s' % path)
                else:
                    places.append('%s > %s' % (where, path) if ' > ' in path
                                  else where)
            else:
                bar = u.parent
                while bar is not None and bar.cls not in ('TToolBar', 'TForm') \
                        and bar is not self.root:
                    bar = bar.parent
                if bar is not None and bar.cls == 'TToolBar':
                    places.append('toolbar "%s"' % humanize(bar.name))
                else:
                    loc = self.location(u)
                    places.append('button' + (' in ' + loc if loc else ''))
        seen, out = set(), []
        for p in places:
            if p not in seen:
                seen.add(p)
                out.append(p)
        return out


def read_shortcuts(path):
    keys = {}
    try:
        with open(path, encoding='utf-8', errors='replace') as f:
            data = f.read()
        for m in re.finditer(r'"Action"\s*:\s*"(\w+)"\s*,\s*"Key"\s*:\s*"([^"]*)"', data):
            keys.setdefault(m.group(1).lower(), []).append(m.group(2))
    except OSError:
        pass
    return keys


def form_context(info, comp, prop, shortcuts):
    lines = []
    kind = CLASS_NAMES.get(comp.cls, humanize(comp.cls[1:] if comp.cls.startswith('T') else comp.cls).lower())
    value = comp.props.get(prop, '')

    if comp is info.root:
        lines.append('Where: title of the %s (%s)' % (info.title, info.file))
        return lines

    if comp.cls == 'TAction':
        places = info.places_of_action(comp)
        if places:
            more = len(places) - MAX_PLACES
            txt = '; '.join(places[:MAX_PLACES])
            if more > 0:
                txt += '; +%d more' % more
            lines.append('Where: %s, %s' % (info.title, txt))
        else:
            lines.append('Where: %s (%s)' % (info.title, info.file))
        what = 'command'
        if prop == 'hint':
            what = 'tooltip of the command "%s"' % clean(comp.props.get('caption', ''))
        cat = comp.props.get('category')
        extra = []
        if cat:
            extra.append('category %s' % clean(cat))
        keys = shortcuts.get(comp.name.lower())
        if keys and info.root.cls == 'TfrmMain':
            extra.append('default shortcut %s' % ', '.join(keys))
        lines.append('What: %s%s' % (what, (' (' + ', '.join(extra) + ')') if extra else ''))
    elif comp.cls == 'TMenuItem':
        path, where = info.menu_path(comp)
        sub = ' (opens a submenu)' if any(c.cls == 'TMenuItem' for c in comp.children) else ''
        if where == 'menu':
            lines.append('Where: %s, menu %s%s' % (info.title, path, sub))
        else:
            lines.append('Where: %s, %s > %s%s' % (info.title, where, path, sub))
        if prop == 'hint':
            lines.append('What: tooltip of a menu item')
    else:
        loc = info.location(comp)
        lines.append('Where: %s%s' % (info.title, (', ' + loc) if loc else ''))
        what = kind
        if prop != 'caption' and PROP_NAMES.get(prop):
            what = '%s of a %s' % (PROP_NAMES[prop], kind)
            if prop == 'hint':
                cap = clean(comp.props.get('caption', ''))
                if cap and cap.lower() != comp.name.lower():
                    what = 'tooltip of the %s "%s"' % (kind, cap)
        lines.append('What: %s' % what)

    owner = comp.parent
    if value and (value == comp.name or (owner is not None and value == owner.name)):
        lines.append('Note: placeholder set by the designer, replaced at runtime '
                     '(usually not shown; any short translation is fine)')
    return lines


# -----------------------------------------------------------------------------
# Context of the resource strings (procLocalize.pas)
# -----------------------------------------------------------------------------


ROUTINE_RE = re.compile(r'^\s*(?:class\s+)?(procedure|function|constructor|destructor)\s+([\w.]+)', re.I)


def read_resourcestrings(path):
    """name(lower) -> group comment above it."""
    groups = {}
    group = ''
    inside = False
    with open(path, encoding='utf-8', errors='replace') as f:
        for line in f:
            s = line.strip()
            if s.lower() == 'resourcestring':
                inside = True
                continue
            if not inside:
                continue
            if s.lower() == 'implementation':
                break
            if s.startswith('//'):
                group = s[2:].strip()
                continue
            m = re.match(r'^(\w+)\s*=', s)
            if m:
                groups[m.group(1).lower()] = group
    return groups


def scan_uses(src_dir, names, forms_by_unit):
    """name(lower) -> [(file, routine, window)] in source order."""
    uses = {k: [] for k in names}
    word = re.compile(r'\b(lng\w+)\b', re.I)
    for fn in sorted(os.listdir(src_dir)):
        if not fn.lower().endswith(('.pas', '.inc')):
            continue
        # procLocalize.pas: only its implementation, not the declarations
        in_code = fn.lower() != 'proclocalize.pas'
        unit = fn.split('_')[0].split('.')[0].lower()
        window = forms_by_unit.get(unit, '')
        routine = ''
        with open(os.path.join(src_dir, fn), encoding='utf-8', errors='replace') as f:
            for line in f:
                code = line.split('//')[0]
                if not in_code:
                    in_code = code.strip().lower() == 'implementation'
                    continue
                m = ROUTINE_RE.match(code)
                if m and not code[:1].isspace():
                    routine = m.group(2)
                for w in word.findall(code):
                    lw = w.lower()
                    if lw in uses:
                        item = (fn, routine, window)
                        if item not in uses[lw]:
                            uses[lw].append(item)
    return uses


FORMAT_RE = re.compile(r'%(?:\d+:)?-?\d*(?:\.\d+)?[sdfxeguMmnp]', re.I)


def lng_context(name, group, uses, msgid):
    lines = []
    places = []
    for fn, routine, window in uses:
        p = fn + (' (%s)' % routine if routine else '')
        if window and window not in p:
            p += ' - ' + window
        places.append(p)
    if places:
        more = len(places) - MAX_USES
        txt = '; '.join(places[:MAX_USES])
        if more > 0:
            txt += '; +%d more' % more
        lines.append('Where: %s' % txt)
    else:
        lines.append('Where: not found in the code (maybe unused)')
    what = 'message or text set by the code'
    if group:
        what += ', group "%s"' % group
    lines.append('What: %s' % what)
    args = []
    for a in FORMAT_RE.findall(msgid):
        if a not in args:
            args.append(a)
    if args:
        lines.append('Placeholders: %s are replaced at runtime, keep them '
                     '(their order can change if they are numbered)' % ' '.join(args))
    if '\n' in msgid:
        lines.append('Note: the text has line breaks, keep them where they make sense')
    return lines


# -----------------------------------------------------------------------------
# .po files
# -----------------------------------------------------------------------------


def po_unquote(s):
    s = s.strip()
    if s.startswith('"') and s.endswith('"'):
        s = s[1:-1]
    return (s.replace('\\n', '\n').replace('\\t', '\t').replace('\\"', '"')
            .replace('\\\\', '\\'))


def split_entries(text):
    """Blocks separated by empty lines, keeping the exact text."""
    return re.split(r'(\r?\n[ \t]*\r?\n)', text)


def entry_fields(block):
    lines = block.splitlines()
    ident = None
    msgid = []
    cur = None
    for ln in lines:
        if ln.startswith('#: ') and ident is None:
            ident = ln[3:].strip().split()[0] if ln[3:].strip() else None
        elif ln.startswith('msgid '):
            cur = msgid
            msgid.append(po_unquote(ln[6:]))
        elif ln.startswith('"') and cur is not None:
            cur.append(po_unquote(ln))
        elif ln.startswith(('msgstr', 'msgctxt', 'msgid_plural')):
            cur = None
    return ident, ''.join(msgid)


def set_comments(block, comments, eol):
    lines = block.split(eol) if eol in block else [block]
    kept = [ln for ln in lines if not ln.startswith('#.')]
    pos = None
    for k, ln in enumerate(kept):
        if ln.startswith('#: ') or ln.startswith('#,') or ln.startswith('msg'):
            pos = k
            break
    if pos is None:
        return block
    # Lazarus reads "\n" in a comment as a line break: no backslashes
    new = ['#. ' + c.replace('\\', '/') for c in comments]
    return eol.join(kept[:pos] + new + kept[pos:])


def process_po(path, contexts, check):
    with open(path, encoding='utf-8', newline='') as f:
        text = f.read()
    eol = '\r\n' if '\r\n' in text else '\n'
    parts = split_entries(text)
    for k in range(0, len(parts), 2):
        block = parts[k]
        if not block.strip() or block.lstrip().startswith('#~'):
            continue
        ident, msgid = entry_fields(block)
        if not ident:
            continue
        comments = contexts.get(ident.lower())
        if comments is None:
            comments = []   # unknown entry: remove old notes only
        parts[k] = set_comments(block, comments, eol)
    new = ''.join(parts)
    if new == text:
        return False
    if not check:
        with open(path, 'w', encoding='utf-8', newline='') as f:
            f.write(new)
    return True


# -----------------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------------


def load_descriptions(path):
    if not os.path.exists(path):
        return {}
    with open(path, encoding='utf-8') as f:
        data = json.load(f)
    return {k.lower(): v for k, v in data.items() if not k.startswith('_') and v}


def build_contexts(pot_path):
    # forms
    forms = {}           # class(lower) -> FormInfo
    forms_by_unit = {}   # unit(lower) -> window title
    for fn in sorted(os.listdir(SRC_DIR)):
        if fn.lower().endswith('.lfm'):
            root = read_lfm(os.path.join(SRC_DIR, fn))
            if root is None:
                continue
            info = FormInfo(os.path.join(SRC_DIR, fn), root)
            forms[root.cls.lower()] = info
            forms_by_unit[fn[:-4].lower()] = info.title

    shortcuts = read_shortcuts(KEY_FILE)
    groups = read_resourcestrings(os.path.join(SRC_DIR, 'procLocalize.pas'))
    descs = load_descriptions(DESC_FILE)

    with open(pot_path, encoding='utf-8', newline='') as f:
        pot = f.read()
    entries = []
    for block in split_entries(pot)[::2]:
        ident, msgid = entry_fields(block)
        if ident:
            entries.append((ident.lower(), msgid))

    lng_names = [i.split('.', 1)[1] for i, _ in entries if i.startswith('proclocalize.')]
    uses = scan_uses(SRC_DIR, lng_names, forms_by_unit)

    contexts = {}
    for ident, msgid in entries:
        unit, _, rest = ident.partition('.')
        lines = []
        if unit == 'proclocalize':
            lines = lng_context(rest, groups.get(rest, ''), uses.get(rest, []), msgid)
        elif unit in forms:
            info = forms[unit]
            cname, _, prop = rest.rpartition('.')
            comp = info.root if cname in ('', info.root.name.lower()) else info.by_name.get(cname)
            if comp is None and '.' in cname:     # sub-property: LabelMemo.Caption
                comp = info.by_name.get(cname.split('.')[0])
            if comp is not None:
                lines = form_context(info, comp, prop, shortcuts)
        d = descs.get(ident)
        if d:
            lines.append('Description: ' + ' '.join(d.split()))
        contexts[ident] = lines
    return contexts, entries, descs


def main():
    ap = argparse.ArgumentParser(description='Adds context for translators to the .pot/.po files.')
    ap.add_argument('--check', action='store_true', help='do not write, only list the files that would change (exit code 1 if any)')
    ap.add_argument('--missing', action='store_true', help='list the entries without description')
    ap.add_argument('--show', metavar='ID', help='print the context of one entry (e.g. tfrmmain.actundo.caption)')
    args = ap.parse_args()

    pot_path = os.path.join(LANG_DIR, 'tero.pot')
    if not os.path.exists(pot_path):
        sys.exit('tero.pot not found in %s' % LANG_DIR)

    contexts, entries, descs = build_contexts(pot_path)

    if args.show:
        key = args.show.lower()
        if key not in contexts:
            sys.exit('Entry not found: %s' % args.show)
        for ln in contexts[key]:
            print('#. ' + ln)
        return

    if args.missing:
        for ident, msgid in entries:
            if ident not in descs:
                print('%s\t%s' % (ident, msgid.replace('\n', '\\n')))
        print('%d of %d entries without description' %
              (sum(1 for i, _ in entries if i not in descs), len(entries)), file=sys.stderr)
        return

    unknown = sorted(k for k in descs if k not in contexts)
    for k in unknown:
        print('warning: description for an entry not in tero.pot (ignored): %s' % k, file=sys.stderr)

    files = [pot_path] + sorted(
        os.path.join(LANG_DIR, fn) for fn in os.listdir(LANG_DIR)
        if fn.startswith('tero.') and fn.endswith('.po'))
    changed = [p for p in files if process_po(p, contexts, args.check)]
    for p in changed:
        print(('would update: ' if args.check else 'updated: ') + os.path.relpath(p, ROOT))
    if not changed:
        print('nothing to change')
    if args.check and changed:
        sys.exit(1)


if __name__ == '__main__':
    main()
