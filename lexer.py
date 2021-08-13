import ply.lex as lex
import ply.yacc as yacc
import re

alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789&|+?.,{}()<>"
tokens = ("SCREEN", "SYMBOL", "OR", "DOT", "PLUS", "OPTPART", "LEFTBRACKET", "RIGHTBRACKET", "ID", "KLEENE", "GR")
ErrorsList = []
captured_groups = set()

def Lexer():
    def t_SCREEN(t):  # Проверка на экранирование
        r"(\&\+)|(\&\|)|(\&\()|(\&\))|(\&\{)|(\&\})|(\&\*)|(\&\&)|(\&\<)|(\&\>)|(\&\.)|(\&\?)"
        sym = str(t.value)
        if len(sym) > 1:
            sym = sym[1]
        t.value = sym
        return t

    def t_SYMBOL(t):
        r"[a-zA-Z0-9]"
        t.value = str(t.value)
        return t

    def t_OR(t):
        r"\|"
        t.value = str(t.value)
        return t

    def t_DOT(t):
        r"\."
        t.value = str(t.value)
        return t

    def t_PLUS(t):
        r"\+"
        t.value = str(t.value)
        return t

    def t_KLEENE(t):
        r"\*"
        t.value = str(t.value)
        return t

    def t_OPTPART(t):
        r"\?"
        t.value = str(t.value)
        return t

    def t_LEFTBRACKET(t):
        r"\("
        t.value = str(t.value)
        return t

    def t_RIGHTBRACKET(t):
        r"\)"
        t.value = str(t.value)
        return t

    def t_GR(t):
        r"\<[A-Za-z][A-Za-z0-9_]*\>"
        t.value = str(t.value)[1:-1]
        return t

    def t_error(t):
        global ErrorsList
        ErrorsList.append("Illegal character '%s'" % t.value[0])
        t.lexer.skip(1)  # пропустить n=1 символов в строке

    return lex.lex()

class Node:
    nval = None
    ntype = ''
    nchild = []

    def __init__(self, nval, ntype, nchild):
        self.nval = nval
        self.ntype = ntype
        self.nchild = nchild

    # def __str__(self):
    #     if self.nchild == []:
    #         return str(self.ntype) + ', val ' + str(self.nval)
    #     else:
    #         childrn = ''
    #         for nod in self.nchild:
    #             childrn += str(nod) + '; '
    #         return str(self.ntype) + ', val ' + str(self.nval) + ', children: [' + childrn + ']'

def dfs(node, _iter):
    if node.nchild == []:
        print(('\t' * _iter) + node.ntype + ", value: " + str(node.nval))
    else:
        print(('\t' * _iter) + node.ntype + ", value: " + str(node.nval) + ", children:")
        for ch in node.nchild:
            dfs(ch, _iter + 1)

def dfs_init(node):
    dfs(node, 0)

def PreOpt(regex):
    range_temp = r"\{[0-9]*,[0-9]*\}"
    mtch = re.search(range_temp, regex)
    if mtch == None:
        return regex
    else:
        ind = mtch.span()[0]
        ind_end = mtch.span()[1]
        if regex[ind - 1] in '+|*{}&[],(':
            if ind - 2 < 0:
                raise Exception("SYNTAX ERROR: range rule must be preceded by a symbol or a group.")
            elif regex[ind - 2] != '&':
                raise Exception("SYNTAX ERROR: range rule must be preceded by a symbol or a group.")
        range = regex[mtch.span()[0]:mtch.span()[1]]
        bounds = re.findall(r'\d+', range)
        lower_bound = (re.findall(r'\{,', range) == [])
        upper_bound = (re.findall(r',\}', range) == [])
        if regex[ind - 1] in 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.':
            regex_start = regex[:ind - 1]
            regex_end = regex[ind_end:]
            rep_range = regex[ind - 1]
        elif regex[ind - 1] in '+|*{}&[],(':
            regex_start = regex[:ind - 2]
            regex_end = regex[ind_end:]
            rep_range = regex[ind - 2:ind]
        elif regex[ind - 1] == '>':
            ind_l_ang = ind - 1
            while True:
                ind_l_ang -= 1
                if ind_l_ang < 0:
                    raise Exception("Syntax error: not balanced brackets")
                if regex[ind_l_ang] == '<':
                    break
            rep_range = regex[ind_l_ang:ind]
            regex_start = regex[:ind_l_ang]
            regex_end = regex[ind_end:]
            print(regex_end)
            if lower_bound == False and upper_bound == False:
                return regex_start + rep_range + '*' + regex_end
            elif lower_bound == False and upper_bound == True:
                return regex_start + (rep_range + '?') * int(bounds[0]) + regex_end
            elif lower_bound == True and upper_bound == False:
                return regex_start + rep_range * int(bounds[0]) + rep_range + '*' + regex_end
            elif lower_bound == True and upper_bound == True:
                if bounds[0] > bounds[1]:
                    raise Exception("SYNTAX ERROR: upper bound must be more then lower bound.")
                return regex_start + rep_range * int(bounds[0]) + (rep_range + '?') * (
                            int(bounds[1]) - int(bounds[0])) + regex_end
        elif regex[ind - 1] == ')':
            ind_lp = -1
            string_ptr = ind - 1
            parentheses_ctr = -1
            while string_ptr > 0:
                string_ptr -= 1
                if regex[string_ptr] == '(':
                    parentheses_ctr += 1
                elif regex[string_ptr] == ')':
                    parentheses_ctr -= 1
                if parentheses_ctr == 0:
                    ind_lp = string_ptr
                    break
                if string_ptr == 0 and parentheses_ctr != 0:
                    raise Exception("not balanced brackets")
            regex_start = regex[:ind_lp]
            regex_end = regex[ind_end:]
            rep_range = regex[ind_lp:ind]
        if lower_bound == False and upper_bound == False:
            return regex_start + '(' + rep_range + '*)' + regex_end
        elif lower_bound == False and upper_bound == True:
            return regex_start + '(' + (rep_range + '?') * int(bounds[0]) + ')' + regex_end
        elif lower_bound == True and upper_bound == False:
            return regex_start + '(' + rep_range * int(bounds[0]) + rep_range + '*' + ')' + regex_end
        elif lower_bound == True and upper_bound == True:
            if bounds[0] > bounds[1]:
                raise Exception("SYNTAX ERROR: upper bound must be more then lower bound.")
            return regex_start + '(' + rep_range * int(bounds[0]) + (rep_range + '?') * (
                        int(bounds[1]) - int(bounds[0])) + ')' + regex_end

def GroupNodes(tstream):
    global captured_groups
    current_alias = ''
    for i in range(len(tstream)):
        if not isinstance(tstream[i], Node):
            if tstream[i].type == 'SYMBOL' or tstream[i].type == 'SCREEN':
                tstream[i] = Node(tstream[i].value, 'a-node', [])
            elif tstream[i].type == 'DOT':
                tstream[i] = Node(None, '.-node', [])
            elif tstream[i].type == 'GR':
                if i == 0:
                    if len(tstream) == 1:
                        print(tstream)
                        raise Exception("Syntax error: empty group can't ba captured.")
                    if tstream[i].value in captured_groups:
                        raise Exception("Syntax error: can't capture a group more than once.")
                    tstream[i].type = 'GR_CAP'
                    captured_groups.add(tstream[i].value)
                    current_alias = tstream[i].value
                else:
                    if tstream[i].value == current_alias:
                        raise Exception("Syntax error: tried to refer the group inside itself.")
                    if tstream[i].value not in captured_groups:
                        raise Exception("Syntax error: tried to refer a group before capture.")
                    tstream[i] = Node(tstream[i].value, 'grref_node', [])
    while True:
        len_last = len(tstream)
        for i in range(len(tstream)):
            if not isinstance(tstream[i], Node):
                if tstream[i].type == 'KLEENE':
                    if i < 1:
                        raise Exception("Syntax error: asterisk without a symbol.")
                    if not isinstance(tstream[i - 1], Node):
                        raise Exception("Syntax error: an asterisk is not preceded by a valid group.")
                    tstream[i] = Node(None, '*-node', [tstream[i - 1]])
                    tstream.pop(i - 1)
                    break
                elif tstream[i].type == 'PLUS':
                    if i < 1:
                        raise Exception("Syntax error: plus without a symbol.")
                    if not isinstance(tstream[i - 1], Node):
                        raise Exception("Syntax error: plus is not preceded by a valid group.")
                    asterisk = Node(None, '*-node', [tstream[i - 1]])
                    tstream[i] = Node(None, 'concat_node', [tstream[i - 1], asterisk])
                    tstream.pop(i - 1)
                    break
                elif tstream[i].type == 'OPTPART':
                    if i < 1:
                        raise Exception("Syntax error: opt without a symbol.")
                    if not isinstance(tstream[i - 1], Node):
                        raise Exception("Syntax error: opt not preceded by a valid group.")
                    tstream[i] = Node(None, '?-node', [tstream[i - 1]])
                    tstream.pop(i - 1)
                    break
        if len_last == len(tstream):
            break
    while True:
        len_last = len(tstream)
        for i in range(len(tstream)):
            if isinstance(tstream[i], Node):
                if i + 1 < len(tstream):
                    if isinstance(tstream[i + 1], Node):
                        tstream[i] = Node(None, 'concat_node', [tstream[i], tstream[i + 1]])
                        tstream.pop(i + 1)
                        break
        if len_last == len(tstream):
            break
    while True:
        len_last = len(tstream)
        if isinstance(tstream[0], Node):
            start = 0
        else:
            start = 1
        for i in range(start, len(tstream)):
            if not isinstance(tstream[i], Node):
                if tstream[i].type != 'OR':
                    raise Exception("Syntax error: handled token encountered.")
                else:
                    if i - 1 >= start:
                        if not isinstance(tstream[i - 1], Node):
                            raise Exception("Syntax error: invalid symbol left of OR token.")
                        else:
                            if i + 1 < len(tstream):
                                if not isinstance(tstream[i + 1], Node):
                                    raise Exception("Syntax error: invalid symbol right of OR token.")
                                else:
                                    tstream[i] = Node(None, '|-node', [tstream[i - 1], tstream[i + 1]])
                                    tstream.pop(i + 1)
                                    tstream.pop(i - 1)
                                    break
        if len_last == len(tstream):
            break
    if not isinstance(tstream[0], Node):
        if tstream[0].type == 'GR_CAP' and len(tstream) == 2:
            tstream[0] = Node(tstream[0].value, 'grcap_node', [tstream[1]])
            tstream.pop(1)
        else:
            raise Exception("Syntax error: the capture group is placed incorrectly.")
    if len(tstream) != 1:
        raise Exception("Syntax error: an unhandled expression was encountered.")
    return tstream[0]


def BracketGroup(tstream):
    lcounter = rcounter = 0
    for token in tstream:
        if not isinstance(token, Node):
            if token.type == 'LEFTBRACKET':
                lcounter += 1
            if token.type == 'RIGHTBRACKET':
                rcounter += 1
    if rcounter != lcounter:
        raise Exception("Syntax error: unmatched brackets.")
    if lcounter == 0:
        return GroupNodes(tstream)
    lp_move = lp_ind = rp_move = 0
    for i in range(len(tstream)):
        if tstream[i].type == 'LEFTBRACKET':
            lp_move += 1
            if lp_move == lcounter:
                break
        lp_ind += 1
    rp_ind = lp_ind
    for j in range(lp_ind, len(tstream)):
        if not isinstance(tstream[j], Node):
            if tstream[j].type == 'RIGHTBRACKET':
                break
        rp_ind += 1
    new_tstream = []
    if lp_ind != 0:
        new_tstream += tstream[:lp_ind]
    new_tstream.append(BracketGroup(tstream[lp_ind + 1:rp_ind]))
    if rp_ind != len(tstream) - 1:
        new_tstream += tstream[rp_ind + 1:]
    return BracketGroup(new_tstream)

def Wrap(group):
    global ErrorsList
    ErrorsList = []
    rl = Lexer()
    tstream = []
    rl.input(group)
    for token in rl:
        tstream.append(token)
    if ErrorsList != []:
        raise Exception("Lex error: " + str(ErrorsList))
    return BracketGroup(tstream)
