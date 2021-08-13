import FSM as FA

def findall(regex, data):
    return findall_rec(regex, data, [])

def findall_rec(regex, data, match_list):
    for i in range(len(data)):
        for j in range(len(data)+1, i, -1):
            d_aut = FA.Regex(FA.lx.PreOpt(regex))
            d_aut.Step_By_Step(data[i:j])
            if d_aut.IsAccepting():
                match_list.append(data[i:j])
                data = data[:i] + data[j:]
                return findall_rec(regex, data, match_list)
    return match_list

# test = ["ababx","abaoabo", "aabaabxx", "ooxxx", "x", "ababxxadd", "ababxxlol", "oo", "aababxxxxadd", "aabaabxxaddlol",
#         "abadd", "ababadd", "oooo", "abx"]
# for string in test:
#     print(string)
#     res = findall(r'(<a>(c(b|bd)|o+))<a>(x){,3}(cbb)?', string)
#     if len(res) == 1 and res[0] == string:
#         print("Valid")
#     else:
#         print("Invalid")
#     print("------------------------------")

test = ["ababx","abaoabo", "aabaabxx", "ooxxx", "x", "ababxxadd", "ababxxlol", "oo", "aababxxxxadd", "aabaabxxaddlol", "abadd", "ababadd", "oooo", "abx"]
for string in test:
    print(string)
    print(findall(r"(<a>(c(b|bd)|o+))<a>(x){,3}(cbb)?", string))
    print("------------------------------")
