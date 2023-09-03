from typing import Dict



def to_str(input : Dict) -> str:
    type = input['type']

    if type == 'basic_int':
        return str(input['value'])
    elif type == 'fn_call':
        return to_str(input['function']) + " " + if_basic_dont_wrap(input['param'])
    elif type == 'anon_fn':
        return f"({input['bound_var']}->{to_str(input['inner'])})"
    elif type == 'basic_word_var':
        return input['value']
    elif type == 'core_fn':
        if ('evaluation_status' in input.keys()) and input['evaluation_status'] == 'partial':
            return f"{input['name']} {' '.join([if_basic_dont_wrap(p) for p in input['params']])}"
        else:
            return input['name']
    elif type == 'unknown_basic':
        return input['value']
    elif type == 'record':
        return "{" + f"{ ', '.join([ p['name'] + ' = ' + to_str(p['value']) for p in input['pairs'] ]) }" + "}"


def if_basic_dont_wrap(p):
    if "basic" in p['type'] or 'record' in p['type']:
        return to_str(p)
    else:
        return f'({to_str(p)})'