import lexer as lx
import itertools

st_count = 0

class Transition:
    source = ''
    dest = ''
    in_char = ''

    def __init__(self, source, dest, in_char):
        if (not isinstance(source, str)) or (not isinstance(dest, str)) or (not isinstance(in_char, str)):
            raise Exception("Wrong type in transition constructor.")
        self.source = source
        self.dest = dest
        self.in_char = in_char

    # def __str__(self):
    #     return "\'" + self.source + "\' -> \'" + self.dest + "\' when \'" + self.in_char + "\'"

class DFA:

    def __init__(self):
        self.transitions = []
        self.states = set()
        self.acc_states = set()
        self.curr_state = None

    def AddState(self, state):
        if not isinstance(state, str):
            raise Exception("Wrong type in DFA state add.")
        self.states.add(state)

    def AddAcceptState(self, state):
        if not isinstance(state, str):
            raise Exception("Wrong type in DFA accept state add.")
        self.acc_states.add(state)

    def AddTransition(self, source, dest, in_char):
        if in_char == '':
            raise Exception("Attempted to add an epsilon-transition in a DFA.")
        if source not in self.states or dest not in self.states:
            raise Exception("Attempted to add a transition with undefined states.")
        for t in self.transitions:
            if(t.source == source) and (t.in_char == in_char) and in_char != '':
                raise Exception("Attempted to create an edge with an existing in char twice.")
        self.transitions.append(Transition(source, dest, in_char))

    def IsAccepting(self):
        if self.curr_state in self.acc_states:
            return True
        return False

    # def __str__(self):
    #     s = "States: " + str(self.states)
    #     s += "\nCurrent state: " + str(self.curr_state)
    #     s += "\nAccept states: " + str(self.acc_states)
    #     s += "\nTransitions:"
    #     for t in self.transitions:
    #         s += "\n" + str(t)
    #     return s

    def Step(self, in_char):
        if in_char == '':
            raise Exception("Attempted to make an epsilon-transition in a DFA.")
        step_made = False
        for tr in self.transitions:
            if self.curr_state == tr.source and tr.in_char == in_char:
                self.curr_state = tr.dest
                step_made = True
                break
        if not step_made:
            print("Couldn't make a transition from state " + self.curr_state)

    def Init(self, state):
        if state not in self.states:
            raise Exception("Attempted to Init DFA with undefined state.")
        if not isinstance(state, str):
            raise Exception("Wrong type of init DFA state.")
        self.curr_state = state

    def Step_By_Step(self, s):
        if not isinstance(s, str):
            raise Exception("Error in DFA.Step_By_Step(): s must be a string.")
        for c in s:
            self.Step(c)

class NDA:

    def __init__(self):
        self.states         = set()
        self.prev_states    = set()
        self.transitions    = []
        self.curr_states    = set()
        self.acc_states     = set()
        self.curr_state     = set()
        self.grcap_id       = None
        self.grref_id       = None
        self.grcap_list     = []
        self.grref_list     = []
        self.processing_cap = None
        self.processing_ref = None
        self.cap_buffer     = {}
        self.ref_curr_pos   = None
        self.ref_flag       = None
        self.truncate_dict  = set()
        self.captured_gr    = set()

    def Init(self, state):
        if isinstance(state, str):
            if state not in self.states:
                raise Exception("Attempted to Init NDA with undefined state \'" + state + "\'")
            self.curr_states = set()
            self.curr_states.add(state)
        elif isinstance(state, set):
            for s in state:
                if s not in self.states:
                    raise Exception("Attempted to Init NDA with undefined state \'" + state + "\'")
                if not isinstance(s, str):
                    raise Exception("Wrong type in init state set.")
            self.curr_states = state
        else:
            raise Exception("Wrong type of init state.")

    def AddState(self, state):
        if not isinstance(state, str):
            raise Exception("Wrong type in automaton state add.")
        self.states.add(state)

    def AddAcceptState(self, state):
        if not isinstance(state, str):
            raise Exception("Wrong type in automaton state add.")
        self.acc_states.add(state)

    def AddTransition(self, source, dest, in_char):
        if source not in self.states or dest not in self.states:
            raise Exception("Attempted to add a transition with undefined states.")
        for t in self.transitions:
            if t.source == source and t.in_char == in_char and in_char != '':
                raise Exception("Attempted to create an edge with an existing in char twice.")
        self.transitions.append(Transition(source, dest, in_char))

    def EClosure(self):
        while True:
            states_q = len(self.curr_states)
            for st in self.curr_states.copy():
                for tr in self.transitions:
                    if st == tr.source and tr.in_char == '':
                        self.curr_states.add(tr.dest)
            if states_q == len(self.curr_states):
                break

    def SymbCl(self, symbol, states):
        new_states = set()
        for st in states:
            for tr in self.transitions:
                if st == tr.source and symbol == tr.in_char:
                    new_states.add(tr.dest)
        return new_states

    def Step(self, in_char):
        for gr in self.grcap_list:
            if [x for x in gr[2] if x in self.curr_states] != []:
                self.captured_gr.add(self.processing_cap)
                self.processing_cap = None
            for st in gr[1]:
                if st in self.curr_states:
                    self.processing_cap = gr[0]
        for gr in self.grref_list:
            if gr[1] in self.curr_states:
                self.processing_ref = gr[0]
        if self.processing_ref != None:
            gr_outstates = [x[2] for x in self.grcap_list if x[0] == self.processing_ref]
            if gr_outstates == 0:
                raise("Syntax error: attempted to reference an unspecified group.")
            else:
                gr_outstates = gr_outstates[0][0]
            self.Init('unacceptable')
            if self.ref_curr_pos == None:
                self.ref_curr_pos = 0
            if self.cap_buffer[self.processing_ref][self.ref_curr_pos] != in_char:
                self.ref_flag = False
            self.ref_curr_pos += 1
            if self.ref_curr_pos == len(self.cap_buffer[self.processing_ref]):
                if self.ref_flag != False:
                    for g in self.grref_list:
                        if g[0] == self.processing_ref:
                            self.Init(g[2])
                            self.EClosure()
                            self.grref_list.remove(g)
                            break
                ref_curr_pos = None
                self.processing_ref = None
                self.ref_curr_pos = None
                self.processing_cap = None
            return
        self.prev_states = self.states
        self.EClosure()
        if in_char == '':
            raise Exception("Attempted to explicitly make an epsilon-transition.")
        transitions_made = set()
        for st in self.curr_states.copy():
            for tr in self.transitions:
                if st == tr.source and tr.in_char == in_char:
                    if self.processing_cap != None:
                        if self.processing_cap not in self.captured_gr:
                            if self.cap_buffer.get(self.processing_cap) == None:
                                self.cap_buffer[self.processing_cap] = in_char
                            else:
                                self.cap_buffer[self.processing_cap] = self.cap_buffer[self.processing_cap] + in_char
                    self.curr_states.add(tr.dest)
                    transitions_made.add(tr.dest)
            if st not in transitions_made:
                self.curr_states.remove(st)
        self.EClosure()

    def IsAccepting(self):
        for st in self.curr_states:
            if st in self.acc_states:
                return True
        return False

    def Step_By_Step(self, s):
        if not isinstance(s, str):
            raise Exception("Error in NDA.Step_By_Step(): s must be a string.")
        for c in s:
            self.Step(c)

    def GetDFAFromNDA(self):
        global NDA_alphabet
        global st_count
        if len(self.curr_states) != 1:
            raise Exception("There must be only one initial NDA state by construction.")
        if self.grcap_list != []:
            self.curr_state = list(self.curr_states)[0]
            return self
        d_aut = DFA()
        states_table = []
        unmarked_st = set()
        self.EClosure()
        d_aut.AddState('st' + str(st_count))
        d_aut.Init('st' + str(st_count))
        states_table.append(['st' + str(st_count), self.curr_states])
        unmarked_st.add('st' + str(st_count))
        st_count += 1
        while len(unmarked_st) != 0:
            for item in unmarked_st:
                break
            c_states = set()
            ex = False
            for c in states_table:
                if c[0] == item:
                    c_states = c[1]
                    ex = True
            if not ex:
                raise Exception("The corresponding state set for the raw state was not found " + item)
            for a in lx.alphabet:
                self.Init(self.SymbCl(a, c_states))
                self.EClosure()
                step_states_NDA = self.curr_states
                step_state_DFA = None
                for c in states_table:
                    if step_states_NDA == c[1]:
                        step_state_DFA = c[0]
                if step_state_DFA is None:
                    d_aut.AddState('st' + str(st_count))
                    states_table.append(['st' + str(st_count), self.curr_states])
                    unmarked_st.add('st' + str(st_count))
                    step_state_DFA = 'st' + str(st_count)
                    st_count += 1
                d_aut.AddTransition(item, step_state_DFA, a)
            unmarked_st.remove(item)
        for c in states_table:
            for st in c[1]:
                if st in self.acc_states:
                    d_aut.AddAcceptState(c[0])
        return d_aut

def Node(nod):
    if not isinstance(nod, lx.Node):
        raise Exception("Error in Node: input is not a node.")
    if nod.ntype == 'a-node':
        return ANode(nod)
    elif nod.ntype == 'e-node':
        return ENode(nod)
    elif nod.ntype == '.-node':
        return DotNode(nod)
    elif nod.ntype == '*-node':
        return KleeneNode(nod)
    elif nod.ntype == '|-node':
        return OrNode(nod)
    elif nod.ntype == 'concat_node':
        return ConcatNode(nod)
    elif nod.ntype == '?-node':
        return OptNode(nod)
    elif nod.ntype == 'grcap_node':
        return GrCapNode(nod)
    elif nod.ntype == 'grref_node':
        return GrRefNode(nod)

    else:
        raise Exception("Unexpected node type in Node(nod): " + str(nod))

def ANode(nod):
    global st_count
    if not isinstance(nod, lx.Node):
        raise Exception("Error in MakeFromANode: input is not a node.")
    if nod.ntype != 'a-node':
        raise Exception("Error in MakeFromANode: node is not an a-node.")
    nd_aut = NDA()
    nd_aut.AddState('s' + str(st_count))
    nd_aut.AddState('f' + str(st_count+1))
    nd_aut.AddTransition('s' + str(st_count), 'f' + str(st_count+1), nod.nval)
    nd_aut.AddAcceptState('f' + str(st_count+1))
    nd_aut.Init('s' + str(st_count))
    st_count += 2
    return nd_aut

def ENode(nod):
    global st_count
    if not isinstance(nod, lx.Node):
        raise Exception("Error in MakeFromANode: input is not a node.")
    if nod.ntype != 'e-node':
        raise Exception("Error in MakeFromANode: node is not an a-node.")
    nd_aut = NDA()
    nd_aut.AddState('s' + str(st_count))
    nd_aut.AddState('f' + str(st_count+1))
    nd_aut.AddTransition('s' + str(st_count), 'f' + str(st_count+1), '')
    nd_aut.AddAcceptState('f' + str(st_count+1))
    nd_aut.Init('s' + str(st_count))
    st_count += 2
    return nd_aut

def DotNode(nod):
    global st_count
    if not isinstance(nod, lx.Node):
        raise Exception("Error in MakeFromANode: input is not a node.")
    if nod.ntype != '.-node':
        raise Exception("Error in MakeFromANode: node is not a .-node.")
    nd_aut = NDA()
    nd_aut.AddState('s' + str(st_count))
    nd_aut.AddState('f' + str(st_count+1))
    for symb in lx.alphabet:
        nd_aut.AddTransition('s' + str(st_count), 'f' + str(st_count+1), symb)
    nd_aut.AddAcceptState('f' + str(st_count+1))
    nd_aut.Init('s' + str(st_count))
    st_count += 2
    return nd_aut

def OrNode(nod):
    global st_count
    if not isinstance(nod, lx.Node):
        raise Exception("Error in MakeFromOrNode: input is not a node.")
    if nod.ntype != '|-node':
        raise Exception("Error in MakeFromOrNode: node is not an |-node.")
    if len(nod.nchild) != 2:
        raise Exception("Error in MakeFromOrNode: not two children in |-node.")
    nd_aut_child1 = Node(nod.nchild[0])
    nd_aut_child2 = Node(nod.nchild[1])
    if len(nd_aut_child1.curr_states) != 1 or len(nd_aut_child2.curr_states) != 1:
        raise Exception("Error in MakeFromOrNode: quantity of initial states in children automat are not both 1.")
    nd_aut = NDA()
    nd_aut.states.update(nd_aut_child1.states)
    nd_aut.states.update(nd_aut_child2.states)
    nd_aut.transitions += nd_aut_child1.transitions + nd_aut_child2.transitions
    nd_aut.AddState('s' + str(st_count))
    nd_aut.AddState('f' + str(st_count+1))
    for st in (nd_aut_child1.curr_states | nd_aut_child2.curr_states):
        nd_aut.AddTransition('s' + str(st_count), st, '')
    for st in (nd_aut_child1.acc_states | nd_aut_child2.acc_states):
        nd_aut.AddTransition(st, 'f' + str(st_count+1), '')
    nd_aut.AddAcceptState('f' + str(st_count+1))
    nd_aut.Init('s' + str(st_count))
    st_count += 2
    nd_aut.grcap_list = nd_aut_child1.grcap_list + nd_aut_child2.grcap_list
    nd_aut.grref_list = nd_aut_child1.grref_list + nd_aut_child2.grref_list
    return nd_aut

def ConcatNode(nod):
    if not isinstance(nod, lx.Node):
        raise Exception("Error in MakeFromConcatNode: input is not a node.")
    if nod.ntype != 'concat_node':
        raise Exception("Error in MakeFromConcatNode: node is not a concat_node.")
    if len(nod.nchild) != 2:
        raise Exception("Error in MakeFromConcatNode: not two children in concat_node.")
    nd_aut_child1 = Node(nod.nchild[0])
    nd_aut_child2 = Node(nod.nchild[1])
    if len(nd_aut_child1.curr_states) != 1 or len(nd_aut_child2.curr_states) != 1:
        raise Exception("Error in MakeFromConcatNode: quantity of initial states in children automata are not both 1.")
    nd_aut = NDA()
    nd_aut.states.update(nd_aut_child1.states)
    nd_aut.states.update(nd_aut_child2.states)
    nd_aut.transitions += nd_aut_child1.transitions + nd_aut_child2.transitions
    for i in nd_aut_child1.curr_states:
        nd_aut.Init(i)
    for i in nd_aut_child1.acc_states:
        for j in nd_aut_child2.curr_states:
            nd_aut.AddTransition(i, j, '')
    for i in nd_aut_child2.acc_states:
        nd_aut.AddAcceptState(i)
    nd_aut.grcap_list = nd_aut_child1.grcap_list + nd_aut_child2.grcap_list
    nd_aut.grref_list = nd_aut_child1.grref_list + nd_aut_child2.grref_list
    return nd_aut

def KleeneNode(nod):
    global st_count
    if not isinstance(nod, lx.Node):
        raise Exception("Error in MakeFromKleeneNode: input is not a node.")
    if nod.ntype != '*-node':
        raise Exception("Error in MakeFromKleeneNode: node is not a *-node.")
    if len(nod.nchild) != 1:
        raise Exception("Error in MakeFromKleeneNode: not one children in *-node.")
    nd_aut_child = Node(nod.nchild[0])
    if len(nd_aut_child.curr_states) != 1:
        raise Exception("Error in MakeFromKleeneNode: quantity of initial states in child automaton is not 1.")
    nd_aut = NDA()
    nd_aut.states.update(nd_aut_child.states)
    nd_aut.transitions += nd_aut_child.transitions
    nd_aut.AddState('s' + str(st_count))
    nd_aut.AddState('f' + str(st_count+1))
    nd_aut.Init('s' + str(st_count))
    nd_aut.AddTransition('s' + str(st_count), 'f' + str(st_count+1), '')
    for st in nd_aut_child.curr_states:
        nd_aut.AddTransition('s' + str(st_count), st, '')
    for st in nd_aut_child.acc_states:
        nd_aut.AddTransition(st, 'f' + str(st_count+1), '')
    for st in nd_aut_child.curr_states:
        for ts in nd_aut_child.acc_states:
            nd_aut.AddTransition(ts, st, '')
    nd_aut.AddAcceptState('f' + str(st_count+1))
    st_count += 2
    nd_aut.grcap_list = nd_aut_child.grcap_list
    nd_aut.grref_list = nd_aut_child.grref_list
    return nd_aut

def OptNode(nod):
    global st_count
    if not isinstance(nod, lx.Node):
        raise Exception("Error in MakeFromOptNode: input is not a node.")
    if nod.ntype != '?-node':
        raise Exception("Error in MakeFromOptNode: node is not a ?-node.")
    if len(nod.nchild) != 1:
        raise Exception("Error in MakeFromOptNode: not one children in ?-node.")
    nd_aut_child = Node(nod.nchild[0])
    if len(nd_aut_child.curr_states) != 1:
        raise Exception("Error in MakeFromOptNode: quantity of initial states in child automaton is not 1.")
    nd_aut = NDA()
    nd_aut.states.update(nd_aut_child.states)
    nd_aut.transitions += nd_aut_child.transitions
    nd_aut.AddState('s' + str(st_count))
    nd_aut.AddState('f' + str(st_count+1))
    nd_aut.Init('s' + str(st_count))
    nd_aut.AddTransition('s' + str(st_count), 'f' + str(st_count+1), '')
    for st in nd_aut_child.curr_states:
        nd_aut.AddTransition('s' + str(st_count), st, '')
    for st in nd_aut_child.acc_states:
        nd_aut.AddTransition(st, 'f' + str(st_count+1), '')
    nd_aut.AddAcceptState('f' + str(st_count+1))
    st_count += 2
    nd_aut.grcap_list = nd_aut_child.grcap_list
    nd_aut.grref_list = nd_aut_child.grref_list
    return nd_aut

def GrCapNode(nod):
    global st_count
    nd_aut = Node(nod.nchild[0])
    nd_aut.AddState('s' + str(st_count))
    nd_aut.AddState('f' + str(st_count+1))
    for st in nd_aut.curr_states:
        nd_aut.AddTransition('s' + str(st_count), st, '')
    for st in nd_aut.acc_states:
        nd_aut.AddTransition(st, 'f' + str(st_count+1), '')
    nd_aut.Init('s' + str(st_count))
    nd_aut.acc_states = set()
    nd_aut.AddAcceptState('f' + str(st_count+1))
    st_count += 2
    gr_in_states  = list(nd_aut.states)
    gr_out_states = list(nd_aut.acc_states)
    nd_aut.grcap_list.append([nod.nval, gr_in_states, gr_out_states])
    return nd_aut

def GrRefNode(nod):
    global st_count
    nd_aut = NDA()
    nd_aut.grref_id = nod.nval
    nd_aut.AddState('s' + str(st_count))
    nd_aut.AddState('f' + str(st_count+1))
    nd_aut.AddState('unacceptable')
    nd_aut.Init('s' + str(st_count))
    nd_aut.AddAcceptState('f' + str(st_count+1))
    nd_aut.grref_list.append([nd_aut.grref_id, 's' + str(st_count), 'f' + str(st_count+1)])
    st_count += 2
    return nd_aut

def Regex(regex):
    lx.captured_groups = set()
    return Node(lx.Wrap(regex)).GetDFAFromNDA()
