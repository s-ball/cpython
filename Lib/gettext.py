"""Internationalization and localization support.

This module provides internationalization (I18N) and localization (L10N)
support for your Python programs by providing an interface to the GNU gettext
message catalog library.

I18N refers to the operation by which a program is made aware of multiple
languages.  L10N refers to the adaptation of your program, once
internationalized, to the local language and cultural habits.

"""

# This module represents the integration of work, contributions, feedback, and
# suggestions from the following people:
#
# Martin von Loewis, who wrote the initial implementation of the underlying
# C-based libintlmodule (later renamed _gettext), along with a skeletal
# gettext.py implementation.
#
# Peter Funk, who wrote fintl.py, a fairly complete wrapper around intlmodule,
# which also included a pure-Python implementation to read .mo files if
# intlmodule wasn't available.
#
# James Henstridge, who also wrote a gettext.py module, which has some
# interesting, but currently unsupported experimental features: the notion of
# a Catalog class and instances, and the ability to add to a catalog file via
# a Python API.
#
# Barry Warsaw integrated these modules, wrote the .install() API and code,
# and conformed all C and Python code to Python's coding standards.
#
# Francois Pinard and Marc-Andre Lemburg also contributed valuably to this
# module.
#
# J. David Ibanez implemented plural forms. Bruno Haible fixed some bugs.
#
# TODO:
# - Lazy loading of .mo files.  Currently the entire catalog is loaded into
#   memory, but that's probably bad for large translated programs.  Instead,
#   the lexical sort of original strings in GNU .mo files should be exploited
#   to do binary searches and lazy initializations.  Or you might want to use
#   the undocumented double-hash algorithm for .mo files with hash tables, but
#   you'll need to study the GNU gettext code to do this.
#
# - Support Solaris .mo file formats.  Unfortunately, we've been unable to
#   find this format documented anywhere.

# Additions from s-ball to allow lazy loading of .mo file

import locale
import os
import re
import sys


__all__ = ['NullTranslations', 'GNUTranslations', 'Catalog',
           'find', 'translation', 'install', 'textdomain', 'bindtextdomain',
           'bind_textdomain_codeset',
           'dgettext', 'dngettext', 'gettext', 'lgettext', 'ldgettext',
           'ldngettext', 'lngettext', 'ngettext',
           'pgettext', 'dpgettext', 'npgettext', 'dnpgettext',
           'mo_compile', 'MoCompiler', 'PoError',
           ]

_default_localedir = os.path.join(sys.base_prefix, 'share', 'locale')

# Expression parsing for plural form selection.
#
# The gettext library supports a small subset of C syntax.  The only
# incompatible difference is that integer literals starting with zero are
# decimal.
#
# https://www.gnu.org/software/gettext/manual/gettext.html#Plural-forms
# http://git.savannah.gnu.org/cgit/gettext.git/tree/gettext-runtime/intl/plural.y

_token_pattern = re.compile(r"""
        (?P<WHITESPACES>[ \t]+)                    | # spaces and horizontal tabs
        (?P<NUMBER>[0-9]+\b)                       | # decimal integer
        (?P<NAME>n\b)                              | # only n is allowed
        (?P<PARENTHESIS>[()])                      |
        (?P<OPERATOR>[-*/%+?:]|[><!]=?|==|&&|\|\|) | # !, *, /, %, +, -, <, >,
                                                     # <=, >=, ==, !=, &&, ||,
                                                     # ? :
                                                     # unary and bitwise ops
                                                     # not allowed
        (?P<INVALID>\w+|.)                           # invalid token
    """, re.VERBOSE|re.DOTALL)

def _tokenize(plural):
    for mo in re.finditer(_token_pattern, plural):
        kind = mo.lastgroup
        if kind == 'WHITESPACES':
            continue
        value = mo.group(kind)
        if kind == 'INVALID':
            raise ValueError('invalid token in plural form: %s' % value)
        yield value
    yield ''

def _error(value):
    if value:
        return ValueError('unexpected token in plural form: %s' % value)
    else:
        return ValueError('unexpected end of plural form')

_binary_ops = (
    ('||',),
    ('&&',),
    ('==', '!='),
    ('<', '>', '<=', '>='),
    ('+', '-'),
    ('*', '/', '%'),
)
_binary_ops = {op: i for i, ops in enumerate(_binary_ops, 1) for op in ops}
_c2py_ops = {'||': 'or', '&&': 'and', '/': '//'}

def _parse(tokens, priority=-1):
    result = ''
    nexttok = next(tokens)
    while nexttok == '!':
        result += 'not '
        nexttok = next(tokens)

    if nexttok == '(':
        sub, nexttok = _parse(tokens)
        result = '%s(%s)' % (result, sub)
        if nexttok != ')':
            raise ValueError('unbalanced parenthesis in plural form')
    elif nexttok == 'n':
        result = '%s%s' % (result, nexttok)
    else:
        try:
            value = int(nexttok, 10)
        except ValueError:
            raise _error(nexttok) from None
        result = '%s%d' % (result, value)
    nexttok = next(tokens)

    j = 100
    while nexttok in _binary_ops:
        i = _binary_ops[nexttok]
        if i < priority:
            break
        # Break chained comparisons
        if i in (3, 4) and j in (3, 4):  # '==', '!=', '<', '>', '<=', '>='
            result = '(%s)' % result
        # Replace some C operators by their Python equivalents
        op = _c2py_ops.get(nexttok, nexttok)
        right, nexttok = _parse(tokens, i + 1)
        result = '%s %s %s' % (result, op, right)
        j = i
    if j == priority == 4:  # '<', '>', '<=', '>='
        result = '(%s)' % result

    if nexttok == '?' and priority <= 0:
        if_true, nexttok = _parse(tokens, 0)
        if nexttok != ':':
            raise _error(nexttok)
        if_false, nexttok = _parse(tokens)
        result = '%s if %s else %s' % (if_true, result, if_false)
        if priority == 0:
            result = '(%s)' % result

    return result, nexttok

def _as_int(n):
    try:
        i = round(n)
    except TypeError:
        raise TypeError('Plural value must be an integer, got %s' %
                        (n.__class__.__name__,)) from None
    import warnings
    warnings.warn('Plural value must be an integer, got %s' %
                  (n.__class__.__name__,),
                  DeprecationWarning, 4)
    return n

def c2py(plural):
    """Gets a C expression as used in PO files for plural forms and returns a
    Python function that implements an equivalent expression.
    """

    if len(plural) > 1000:
        raise ValueError('plural form expression is too long')
    try:
        result, nexttok = _parse(_tokenize(plural))
        if nexttok:
            raise _error(nexttok)

        depth = 0
        for c in result:
            if c == '(':
                depth += 1
                if depth > 20:
                    # Python compiler limit is about 90.
                    # The most complex example has 2.
                    raise ValueError('plural form expression is too complex')
            elif c == ')':
                depth -= 1

        ns = {'_as_int': _as_int}
        exec('''if True:
            def func(n):
                if not isinstance(n, int):
                    n = _as_int(n)
                return int(%s)
            ''' % result, ns)
        return ns['func']
    except RecursionError:
        # Recursion error can be raised in _parse() or exec().
        raise ValueError('plural form expression is too complex')


def _expand_lang(loc):
    loc = locale.normalize(loc)
    COMPONENT_CODESET   = 1 << 0
    COMPONENT_TERRITORY = 1 << 1
    COMPONENT_MODIFIER  = 1 << 2
    # split up the locale into its base components
    mask = 0
    pos = loc.find('@')
    if pos >= 0:
        modifier = loc[pos:]
        loc = loc[:pos]
        mask |= COMPONENT_MODIFIER
    else:
        modifier = ''
    pos = loc.find('.')
    if pos >= 0:
        codeset = loc[pos:]
        loc = loc[:pos]
        mask |= COMPONENT_CODESET
    else:
        codeset = ''
    pos = loc.find('_')
    if pos >= 0:
        territory = loc[pos:]
        loc = loc[:pos]
        mask |= COMPONENT_TERRITORY
    else:
        territory = ''
    language = loc
    ret = []
    for i in range(mask+1):
        if not (i & ~mask):  # if all components for this combo exist ...
            val = language
            if i & COMPONENT_TERRITORY: val += territory
            if i & COMPONENT_CODESET:   val += codeset
            if i & COMPONENT_MODIFIER:  val += modifier
            ret.append(val)
    ret.reverse()
    return ret



class NullTranslations:
    def __init__(self, fp=None):
        self._info = {}
        self._charset = None
        self._output_charset = None
        self._fallback = None
        if fp is not None:
            self._parse(fp)

    def _parse(self, fp):
        pass

    def add_fallback(self, fallback):
        if self._fallback:
            self._fallback.add_fallback(fallback)
        else:
            self._fallback = fallback

    def gettext(self, message):
        if self._fallback:
            return self._fallback.gettext(message)
        return message

    def lgettext(self, message):
        import warnings
        warnings.warn('lgettext() is deprecated, use gettext() instead',
                      DeprecationWarning, 2)
        if self._fallback:
            with warnings.catch_warnings():
                warnings.filterwarnings('ignore', r'.*\blgettext\b.*',
                                        DeprecationWarning)
                return self._fallback.lgettext(message)
        if self._output_charset:
            return message.encode(self._output_charset)
        return message.encode(locale.getpreferredencoding())

    def ngettext(self, msgid1, msgid2, n):
        if self._fallback:
            return self._fallback.ngettext(msgid1, msgid2, n)
        if n == 1:
            return msgid1
        else:
            return msgid2

    def lngettext(self, msgid1, msgid2, n):
        import warnings
        warnings.warn('lngettext() is deprecated, use ngettext() instead',
                      DeprecationWarning, 2)
        if self._fallback:
            with warnings.catch_warnings():
                warnings.filterwarnings('ignore', r'.*\blngettext\b.*',
                                        DeprecationWarning)
                return self._fallback.lngettext(msgid1, msgid2, n)
        if n == 1:
            tmsg = msgid1
        else:
            tmsg = msgid2
        if self._output_charset:
            return tmsg.encode(self._output_charset)
        return tmsg.encode(locale.getpreferredencoding())

    def pgettext(self, context, message):
        if self._fallback:
            return self._fallback.pgettext(context, message)
        return message

    def npgettext(self, context, msgid1, msgid2, n):
        if self._fallback:
            return self._fallback.npgettext(context, msgid1, msgid2, n)
        if n == 1:
            return msgid1
        else:
            return msgid2

    def info(self):
        return self._info

    def charset(self):
        return self._charset

    def output_charset(self):
        import warnings
        warnings.warn('output_charset() is deprecated',
                      DeprecationWarning, 2)
        return self._output_charset

    def set_output_charset(self, charset):
        import warnings
        warnings.warn('set_output_charset() is deprecated',
                      DeprecationWarning, 2)
        self._output_charset = charset

    def install(self, names=None):
        import builtins
        builtins.__dict__['_'] = self.gettext
        if names is not None:
            allowed = {'gettext', 'lgettext', 'lngettext',
                       'ngettext', 'npgettext', 'pgettext'}
            for name in allowed & set(names):
                builtins.__dict__[name] = getattr(self, name)


class GNUTranslations(NullTranslations):
    # Magic number of .mo files
    LE_MAGIC = 0x950412de
    BE_MAGIC = 0xde120495

    # The encoding of a msgctxt and a msgid in a .mo file is
    # msgctxt + "\x04" + msgid (gettext version >= 0.15)
    CONTEXT = "%s\x04%s"

    # Acceptable .mo versions
    VERSIONS = (0, 1)

    def _get_versions(self, version):
        """Returns a tuple of major version, minor version"""
        return (version >> 16, version & 0xffff)

    def _parse(self, fp):
        """Override this method to support alternative .mo formats."""
        # Delay struct import for speeding up gettext import when .mo files
        # are not used.
        from struct import unpack
        filename = getattr(fp, 'name', '')
        # Parse the .mo file header, which consists of 5 little endian 32
        # bit words.
        self._catalog = catalog = {}
        self.plural = lambda n: int(n != 1) # germanic plural by default
        buf = fp.read()
        buflen = len(buf)
        # Are we big endian or little endian?
        magic = unpack('<I', buf[:4])[0]
        if magic == self.LE_MAGIC:
            version, msgcount, masteridx, transidx = unpack('<4I', buf[4:20])
            ii = '<II'
        elif magic == self.BE_MAGIC:
            version, msgcount, masteridx, transidx = unpack('>4I', buf[4:20])
            ii = '>II'
        else:
            raise OSError(0, 'Bad magic number', filename)

        major_version, minor_version = self._get_versions(version)

        if major_version not in self.VERSIONS:
            raise OSError(0, 'Bad version number ' + str(major_version), filename)

        # Now put all messages from the .mo file buffer into the catalog
        # dictionary.
        for i in range(0, msgcount):
            mlen, moff = unpack(ii, buf[masteridx:masteridx+8])
            mend = moff + mlen
            tlen, toff = unpack(ii, buf[transidx:transidx+8])
            tend = toff + tlen
            if mend < buflen and tend < buflen:
                msg = buf[moff:mend]
                tmsg = buf[toff:tend]
            else:
                raise OSError(0, 'File is corrupt', filename)
            # See if we're looking at GNU .mo conventions for metadata
            if mlen == 0:
                # Catalog description
                lastk = None
                for b_item in tmsg.split(b'\n'):
                    item = b_item.decode().strip()
                    if not item:
                        continue
                    k = v = None
                    if ':' in item:
                        k, v = item.split(':', 1)
                        k = k.strip().lower()
                        v = v.strip()
                        self._info[k] = v
                        lastk = k
                    elif lastk:
                        self._info[lastk] += '\n' + item
                    if k == 'content-type':
                        self._charset = v.split('charset=')[1]
                    elif k == 'plural-forms':
                        v = v.split(';')
                        plural = v[1].split('plural=')[1]
                        self.plural = c2py(plural)
            # Note: we unconditionally convert both msgids and msgstrs to
            # Unicode using the character encoding specified in the charset
            # parameter of the Content-Type header.  The gettext documentation
            # strongly encourages msgids to be us-ascii, but some applications
            # require alternative encodings (e.g. Zope's ZCML and ZPT).  For
            # traditional gettext applications, the msgid conversion will
            # cause no problems since us-ascii should always be a subset of
            # the charset encoding.  We may want to fall back to 8-bit msgids
            # if the Unicode conversion fails.
            charset = self._charset or 'ascii'
            if b'\x00' in msg:
                # Plural forms
                msgid1, msgid2 = msg.split(b'\x00')
                tmsg = tmsg.split(b'\x00')
                msgid1 = str(msgid1, charset)
                for i, x in enumerate(tmsg):
                    catalog[(msgid1, i)] = str(x, charset)
            else:
                catalog[str(msg, charset)] = str(tmsg, charset)
            # advance to next entry in the seek tables
            masteridx += 8
            transidx += 8

    def lgettext(self, message):
        import warnings
        warnings.warn('lgettext() is deprecated, use gettext() instead',
                      DeprecationWarning, 2)
        missing = object()
        tmsg = self._catalog.get(message, missing)
        if tmsg is missing:
            if self._fallback:
                return self._fallback.lgettext(message)
            tmsg = message
        if self._output_charset:
            return tmsg.encode(self._output_charset)
        return tmsg.encode(locale.getpreferredencoding())

    def lngettext(self, msgid1, msgid2, n):
        import warnings
        warnings.warn('lngettext() is deprecated, use ngettext() instead',
                      DeprecationWarning, 2)
        try:
            tmsg = self._catalog[(msgid1, self.plural(n))]
        except KeyError:
            if self._fallback:
                return self._fallback.lngettext(msgid1, msgid2, n)
            if n == 1:
                tmsg = msgid1
            else:
                tmsg = msgid2
        if self._output_charset:
            return tmsg.encode(self._output_charset)
        return tmsg.encode(locale.getpreferredencoding())

    def gettext(self, message):
        missing = object()
        tmsg = self._catalog.get(message, missing)
        if tmsg is missing:
            if self._fallback:
                return self._fallback.gettext(message)
            return message
        return tmsg

    def ngettext(self, msgid1, msgid2, n):
        try:
            tmsg = self._catalog[(msgid1, self.plural(n))]
        except KeyError:
            if self._fallback:
                return self._fallback.ngettext(msgid1, msgid2, n)
            if n == 1:
                tmsg = msgid1
            else:
                tmsg = msgid2
        return tmsg

    def pgettext(self, context, message):
        ctxt_msg_id = self.CONTEXT % (context, message)
        missing = object()
        tmsg = self._catalog.get(ctxt_msg_id, missing)
        if tmsg is missing:
            if self._fallback:
                return self._fallback.pgettext(context, message)
            return message
        return tmsg

    def npgettext(self, context, msgid1, msgid2, n):
        ctxt_msg_id = self.CONTEXT % (context, msgid1)
        try:
            tmsg = self._catalog[ctxt_msg_id, self.plural(n)]
        except KeyError:
            if self._fallback:
                return self._fallback.npgettext(context, msgid1, msgid2, n)
            if n == 1:
                tmsg = msgid1
            else:
                tmsg = msgid2
        return tmsg


# Locate a .mo file using the gettext strategy
def find(domain, localedir=None, languages=None, all=False):
    # Get some reasonable defaults for arguments that were not supplied
    if localedir is None:
        localedir = _default_localedir
    if languages is None:
        languages = []
        for envar in ('LANGUAGE', 'LC_ALL', 'LC_MESSAGES', 'LANG'):
            val = os.environ.get(envar)
            if val:
                languages = val.split(':')
                break
        if 'C' not in languages:
            languages.append('C')
    # now normalize and expand the languages
    nelangs = []
    for lang in languages:
        for nelang in _expand_lang(lang):
            if nelang not in nelangs:
                nelangs.append(nelang)
    # select a language
    if all:
        result = []
    else:
        result = None
    for lang in nelangs:
        if lang == 'C':
            break
        mofile = os.path.join(localedir, lang, 'LC_MESSAGES', '%s.mo' % domain)
        if os.path.exists(mofile):
            if all:
                result.append(mofile)
            else:
                return mofile
    return result



# a mapping between absolute .mo file path and Translation object
_translations = {}
_unspecified = ['unspecified']

def translation(domain, localedir=None, languages=None,
                class_=None, fallback=False, codeset=_unspecified):
    if class_ is None:
        class_ = GNUTranslations
    mofiles = find(domain, localedir, languages, all=True)
    if not mofiles:
        if fallback:
            return NullTranslations()
        from errno import ENOENT
        raise FileNotFoundError(ENOENT,
                                'No translation file found for domain', domain)
    # Avoid opening, reading, and parsing the .mo file after it's been done
    # once.
    result = None
    for mofile in mofiles:
        key = (class_, os.path.abspath(mofile))
        t = _translations.get(key)
        if t is None:
            with open(mofile, 'rb') as fp:
                t = _translations.setdefault(key, class_(fp))
        # Copy the translation object to allow setting fallbacks and
        # output charset. All other instance data is shared with the
        # cached object.
        # Delay copy import for speeding up gettext import when .mo files
        # are not used.
        import copy
        t = copy.copy(t)
        if codeset is not _unspecified:
            import warnings
            warnings.warn('parameter codeset is deprecated',
                          DeprecationWarning, 2)
            if codeset:
                with warnings.catch_warnings():
                    warnings.filterwarnings('ignore', r'.*\bset_output_charset\b.*',
                                            DeprecationWarning)
                    t.set_output_charset(codeset)
        if result is None:
            result = t
        else:
            result.add_fallback(t)
    return result


def install(domain, localedir=None, codeset=_unspecified, names=None):
    t = translation(domain, localedir, fallback=True, codeset=codeset)
    t.install(names)



# a mapping b/w domains and locale directories
_localedirs = {}
# a mapping b/w domains and codesets
_localecodesets = {}
# current global domain, `messages' used for compatibility w/ GNU gettext
_current_domain = 'messages'


def textdomain(domain=None):
    global _current_domain
    if domain is not None:
        _current_domain = domain
    return _current_domain


def bindtextdomain(domain, localedir=None):
    global _localedirs
    if localedir is not None:
        _localedirs[domain] = localedir
    return _localedirs.get(domain, _default_localedir)


def bind_textdomain_codeset(domain, codeset=None):
    import warnings
    warnings.warn('bind_textdomain_codeset() is deprecated',
                  DeprecationWarning, 2)
    global _localecodesets
    if codeset is not None:
        _localecodesets[domain] = codeset
    return _localecodesets.get(domain)


def dgettext(domain, message):
    try:
        t = translation(domain, _localedirs.get(domain, None))
    except OSError:
        return message
    return t.gettext(message)

def ldgettext(domain, message):
    import warnings
    warnings.warn('ldgettext() is deprecated, use dgettext() instead',
                  DeprecationWarning, 2)
    codeset = _localecodesets.get(domain)
    try:
        with warnings.catch_warnings():
            warnings.filterwarnings('ignore', r'.*\bparameter codeset\b.*',
                                    DeprecationWarning)
            t = translation(domain, _localedirs.get(domain, None), codeset=codeset)
    except OSError:
        return message.encode(codeset or locale.getpreferredencoding())
    with warnings.catch_warnings():
        warnings.filterwarnings('ignore', r'.*\blgettext\b.*',
                                DeprecationWarning)
        return t.lgettext(message)

def dngettext(domain, msgid1, msgid2, n):
    try:
        t = translation(domain, _localedirs.get(domain, None))
    except OSError:
        if n == 1:
            return msgid1
        else:
            return msgid2
    return t.ngettext(msgid1, msgid2, n)

def ldngettext(domain, msgid1, msgid2, n):
    import warnings
    warnings.warn('ldngettext() is deprecated, use dngettext() instead',
                  DeprecationWarning, 2)
    codeset = _localecodesets.get(domain)
    try:
        with warnings.catch_warnings():
            warnings.filterwarnings('ignore', r'.*\bparameter codeset\b.*',
                                    DeprecationWarning)
            t = translation(domain, _localedirs.get(domain, None), codeset=codeset)
    except OSError:
        if n == 1:
            tmsg = msgid1
        else:
            tmsg = msgid2
        return tmsg.encode(codeset or locale.getpreferredencoding())
    with warnings.catch_warnings():
        warnings.filterwarnings('ignore', r'.*\blngettext\b.*',
                                DeprecationWarning)
        return t.lngettext(msgid1, msgid2, n)


def dpgettext(domain, context, message):
    try:
        t = translation(domain, _localedirs.get(domain, None))
    except OSError:
        return message
    return t.pgettext(context, message)


def dnpgettext(domain, context, msgid1, msgid2, n):
    try:
        t = translation(domain, _localedirs.get(domain, None))
    except OSError:
        if n == 1:
            return msgid1
        else:
            return msgid2
    return t.npgettext(context, msgid1, msgid2, n)


def gettext(message):
    return dgettext(_current_domain, message)

def lgettext(message):
    import warnings
    warnings.warn('lgettext() is deprecated, use gettext() instead',
                  DeprecationWarning, 2)
    with warnings.catch_warnings():
        warnings.filterwarnings('ignore', r'.*\bldgettext\b.*',
                                DeprecationWarning)
        return ldgettext(_current_domain, message)

def ngettext(msgid1, msgid2, n):
    return dngettext(_current_domain, msgid1, msgid2, n)

def lngettext(msgid1, msgid2, n):
    import warnings
    warnings.warn('lngettext() is deprecated, use ngettext() instead',
                  DeprecationWarning, 2)
    with warnings.catch_warnings():
        warnings.filterwarnings('ignore', r'.*\bldngettext\b.*',
                                DeprecationWarning)
        return ldngettext(_current_domain, msgid1, msgid2, n)


def pgettext(context, message):
    return dpgettext(_current_domain, context, message)


def npgettext(context, msgid1, msgid2, n):
    return dnpgettext(_current_domain, context, msgid1, msgid2, n)


# dcgettext() has been deemed unnecessary and is not implemented.

# James Henstridge's Catalog constructor from GNOME gettext.  Documented usage
# was:
#
#    import gettext
#    cat = gettext.Catalog(PACKAGE, localedir=LOCALEDIR)
#    _ = cat.gettext
#    print _('Hello World')

# The resulting catalog object currently don't support access through a
# dictionary API, which was supported (but apparently unused) in GNOME
# gettext.

Catalog = translation

######################################################################
# Copyright 2018 s-ball: compile po files into a mo file
# Original version: msgfmt.py
# Written by Martin v. Löwis <loewis@informatik.hu-berlin.de>
# Compile a list of po files into a mo file
######################################################################


from email.parser import HeaderParser
import ast
import struct
import array
import math
import warnings


class PoError(Exception):
    """Raised when a syntax error is encountered in a po file

Parameters:
    txt:    detail on the error
    infile: name of the file where the error was found
    lno:    line number where the error was found
"""
    
    def __init__(self, txt, infile, lno):
        self.message, self.infile, self.lno = tst, infile, lno

    def __str__(self):
        return "{} on {}:{}".format(self.message, self.infile, self.lno)


class MoCompiler:
    """Helper class to compile po files into a mo file

Parameters:
    mofile:   name of the mo file to be generated
    use_hash: if True, the mo file will contain a hash table
"""
    
    def __init__(self, mofile, use_hash):
        self.mofile = mofile
        self.messages = {}
        self.use_hash = use_hash

    def add(self, ctxt, id, str, fuzzy, infile):
        """Add a non-fuzzy translation to the dictionary.

Parameters:
    ctxt:   context for the id or None
    id:     id (untranslated string)
    str:    translated string
    fuzzy:  boolean asking to ignore the string
    infile: name of the file where the string is declared

Adds the string to the current mo file if fuzzy is False.
The string is ignored and a warning is raised if the string has already
be declared in another po source file.
"""
        if not fuzzy and str:
            if 0 in id:
                key = id.split(b'\x00')[0]
            else:
                key = id
            if ctxt is None:
                if key in self.messages:
                    warnings.warn("String {key} ignored in {file}:"
                                  " already defined in {old}",
                                  {"key": key, "file": infile,
                                   "old": self.messages[key][2]})
                else:
                    self.messages[key] = id, str, infile
            else:
                ckey = b"%b\x04%b" % (ctxt, key)
                if ckey in self.messages:
                    warnings.warn("String {key} with context {ctxt} ignored"
                                  " in {file}: already defined in {old}",
                                  {"key": key, "ctxt": ctxt, "file": infile,
                                   "old": self.messages[key][2]})
                else:
                    self.messages[ckey] = (b"%b\x04%b" % (ctxt, id),
                                           str, infile)

    def parse(self, infile):
        """Process a source po file"""
        ID = 1
        STR = 2
        CTXT = 3

        section = msgctxt = None
        fuzzy = 0

        # Start off assuming Latin-1, so everything decodes without failure,
        # until we know the exact encoding
        encoding = 'latin-1'

        # Parse the catalog
        lno = 0
        with open(infile, 'rb') as f:
            for l in f:
                l = l.decode(encoding)
                lno += 1
                # If we get a comment line after a msgstr, this is a new entry
                if l[0] == '#' and section == STR:
                    self.add(msgctxt, msgid, msgstr, fuzzy)
                    section = msgctxt = None
                    fuzzy = 0
                # Record a fuzzy mark
                if l[:2] == '#,' and 'fuzzy' in l:
                    fuzzy = 1
                # Skip comments
                if l[0] == '#':
                    continue
                # Now we are in a msgid or msgctxt section, output previous section
                if l.startswith('msgctxt'):
                    if section == STR:
                        self.add(msgctxt, msgid, msgstr, fuzzy)
                    section = CTXT
                    l = l[7:]
                    msgctxt = b''
                elif l.startswith('msgid') and not l.startswith('msgid_plural'):
                    if section == STR:
                        self.add(msgctxt, msgid, msgstr, fuzzy)
                        if not msgid:
                            # See whether there is an encoding declaration
                            p = HeaderParser()
                            charset = p.parsestr(msgstr.decode(encoding)).get_content_charset()
                            if charset:
                                encoding = charset
                        msgctxt = None
                    section = ID
                    l = l[5:]
                    msgid = msgstr = b''
                    is_plural = False
                # This is a message with plural forms
                elif l.startswith('msgid_plural'):
                    if section != ID:
                        raise PoError('msgid_plural not preceded by msgid',
                                      infile, lno)
                    l = l[12:]
                    msgid += b'\0' # separator of singular and plural
                    is_plural = True
                # Now we are in a msgstr section
                elif l.startswith('msgstr'):
                    section = STR
                    if l.startswith('msgstr['):
                        if not is_plural:
                            raise PoError('plural without msgid_plural',
                                          infile, lno)
                        l = l.split(']', 1)[1]
                        if msgstr:
                            msgstr += b'\0' # Separator of the various plural forms
                    else:
                        if is_plural:
                            raise PoError('indexed msgstr required for plural',
                                          infile, lno)
                        l = l[6:]
                # Skip empty lines
                l = l.strip()
                if not l:
                    continue
                l = ast.literal_eval(l)
                if section == CTXT:
                    msgctxt += l.encode(encoding)
                elif section == ID:
                    msgid += l.encode(encoding)
                elif section == STR:
                    msgstr += l.encode(encoding)
                else:
                    raise PoError('Syntax error before:' + l,
                                  infile, lno)
        # Add last entry
        if section == STR:
            self.add(msgctxt, msgid, msgstr, fuzzy)

    def generate(self):
        "Generates the output file."
        # the keys are sorted in the .mo file
        keys = sorted(self.messages.keys())
        offsets = []
        ids = strs = b''
        for key in keys:
            # For each string, we need size and file offset.  Each string is
            # NUL terminated; the NUL does not count into the size.
            id, str = self.messages[key]
            offsets.append((len(ids), len(id), len(strs), len(str)))
            ids += id + b'\0'
            strs += str + b'\0'
        output = ''
        # The header is 7 32-bit unsigned integers.
        htab_size = self.hash_tab_size() if self.use_hash else 0
        htabstart = 7*4 + 16*len(keys)
        # translated string.
        keystart = htabstart + 4*htab_size
        # and the values start after the keys
        valuestart = keystart + len(ids)
        koffsets = []
        voffsets = []
        # The string table first has the list of keys, then the list of values.
        # Each entry has first the size of the string, then the file offset.
        for o1, l1, o2, l2 in offsets:
            koffsets += [l1, o1+keystart]
            voffsets += [l2, o2+valuestart]
        offsets = koffsets + voffsets
        output = struct.pack("Iiiiiii",
                             0x950412de,       # Magic
                             0,                 # Version
                             len(keys),         # # of entries
                             7*4,               # start of key index
                             7*4+len(keys)*8,   # start of value index
                             htab_size, htabstart) # size offset of hash table
        # compute hash table
        if self.use_hash:
            ht = array.array("i", [0] * htab_size)
            for i, key in enumerate(keys, 1):
                hval = hashval(key)
                index = hval % htab_size
                if ht[index] != 0:
                    delta = 1 + hval%(htab_size - 2)
                    while True:
                        index += delta
                        if index >= htab_size:
                            index -= htab_size
                        if ht[index] == 0:
                            break
                ht[index] = i
        # write file                
        with open(self.mofile, "wb") as fd:
            _ = fd.write(output)
            array.array("i", offsets).tofile(fd)
            if self.use_hash:
                ht.tofile(fd)
            fd.write(ids)
            fd.write(strs)

    def hash_tab_size(self):
        """The size of the hash table is the first odd prime above
4 * 3 * nitems where nitems is the number of strings"""
        size = len(self.messages) * 4 // 3
        size |= 1          # ensure odd value
        while True:
            sq = int(math.sqrt(size) + 1.5)
            for div in range(3, sq, 2):
                if size % div == 0:
                    size += 2
                    break
            else:
                return size
        

def mo_compile(output, inputs, use_hash=False):
    """Compile one or more po source files into a mo file.

Parameters:
    output:   name of the output mo file
    inputs:   either a string for a single po file or an iterable of strings
                if more than one source file
    use_hash: boolean asking for the generation of a hash table (default False)
"""
    if isinstance(inputs, str):
        inputs = (inputs,)
    moc = MoCompiler(output, use_hash)
    for f in inputs:
        moc.parse(f)
    moc.generate()

        
def hashval(bstr):
    """Python port of  hashpjw by P.J.Weinberger published in
[Aho, Sethi, Ullman].

It is specially tweaked to be compatible with GNU gettext hash_string
function.
"""
    val = 0
    for b in bstr:
        val = (val << 4) + b
        g = val & 0xF0000000
        if g:
            val ^= g >> 24
            val ^= g
    return val

