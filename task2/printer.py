from typing import Dict



def to_str(input : Dict) -> str:
    type = input['type']

    if type == 'basic_int':
        return str(input['value'])
    elif type == 'fn_call':
        return to_str(input['function']) + " " + to_str(input['param'])
    elif type == 'anon_fn':
        return f"({input['bound_var']}->{to_str(input['inner'])})"
    elif type == 'basic_word_var':
        return input['value']
    elif type == 'core_fn':
        if ('evaluation_status' in input.keys()) and input['evaluation_status'] == 'partial':
            return f"{input['name']} {' '.join([f'({to_str(p)})' for p in input['params']])}"