import os

CURRENT_FILE_DIR = os.path.dirname(__file__)
PRIMITIVES_PATH = os.path.join(CURRENT_FILE_DIR, '..', 'std', 'primitives.ren')

def heading(file):
    output = '''/*
    Auto generated with `generate_primitives.py`, do not edit manually.
*/
mod std

'''
    file.write(output)
    
def digit_to_ascii(file):
    output = '''/// Converts a `digit` to ascii.
/// The given `digit` is expected to be a single digit (0-9).
fn digit_to_ascii(digit: u8) -> u8
{
    if digit >= 10
    {
        @unreachable()
    }
    return digit + 48 // 48 == '0'
}

'''
    file.write(output)

def abs_fn(is_signed, name_lower, name_unsigned=None):
    if is_signed:
        return '''    pub fn abs(number: {0}) -> {1}
    {{
        if number >= 0
        {{
            return number as {1}
        }}
        else number == this::min()
        {{
            return ((number + 1) * (-1)) as {1} + 1
        }}
        else
        {{
            return (number * (-1)) as {1}
        }}
    }}'''.format(name_lower, name_unsigned)
    else:
        return '''    pub fn abs(number: {0}) -> {0}
    {{
        return number
    }}'''.format(name_lower)

# TODO: Can merge most of the logic in `to_string` and `to_string_view`.
def prim_struct(file, name, str_byte_size, min, max, name_unsigned=None):
    name_upper = name[0].upper() + name[1:]
    name_lower = name.lower()
    if not name_unsigned:
        name_unsigned = name_lower
    abs_output = abs_fn(name_lower != name_unsigned, name_lower, name_unsigned)

    output = '''pub struct {0};
impl {0}
{{
    pub fn to_string(number: {1}) -> Result<String, {{u8}}>
    {{
        var is_negative = number < 0
        var number_abs = this::abs(number)

        var buf: [u8: {2}]
        var buf_idx = 0

        while true
        {{
            var digit = number_abs % 10
            var ascii = digit_to_ascii(digit as u8)
            buf.[buf_idx] = ascii
            buf_idx.++

            number_abs /= 10
            if number_abs == 0
            {{
                break
            }}
        }}

        var str_len
        if is_negative
        {{
            str_len = buf_idx + 1
        }}
        else
        {{
            str_len = buf_idx
        }}

        var str_or_error = String::init_size(str_len)
        if err is str_or_error.error
        {{
            return Result::error(err)
        }}
        var str = str_or_error.&.get_success()

        if is_negative
        {{
            str.&.append_ascii(45) // 45 == '-'
        }}

        var idx = buf_idx - 1
        while true
        {{
            var ascii = buf.[idx]
            str.&.append_ascii(ascii)

            if idx == 0
            {{
                break
            }}

            idx.--
        }}

        return Result::success(str)
    }}

    pub fn to_string_view(number: {1}, data: {{[u8: {2}]}}) -> StringView
    {{
        var is_negative = number < 0
        var number_abs = this::abs(number)

        var buf: [u8: {2}]
        var buf_idx = 0

        while true
        {{
            var digit = number_abs % 10
            var ascii = digit_to_ascii(digit as u8)
            buf.[buf_idx] = ascii
            buf_idx.++

            number_abs /= 10
            if number_abs == 0
            {{
                break
            }}
        }}

        var data_idx = 0
        if is_negative
        {{
            data.*.[data_idx] = 45 // 45 == '-'
            data_idx.++
        }}

        buf_idx.--
        while true
        {{
            var ascii = buf.[buf_idx]
            data.*.[data_idx] = ascii
            data_idx.++

            if buf_idx == 0
            {{
                break
            }}

            buf_idx.--
        }}

        return StringView::new(data as {{u8}}, 0, data_idx)
    }}

    pub fn min() -> {1}
    {{
        return {3}
    }}

    pub fn max() -> {1}
    {{
        return {4}
    }}
    
{5}
}}

'''.format(name_upper, name_lower, str_byte_size, min, max, abs_output)
    file.write(output)

if __name__ == '__main__':
    with open(PRIMITIVES_PATH, 'w') as file:
        heading(file)
        digit_to_ascii(file)
        prim_struct(file, 'u8', 3, 0, 255)
        prim_struct(file, 'i8', 4, -128, 127, 'u8')
        prim_struct(file, 'u16', 5, 0, 65535)
        prim_struct(file, 'i16', 6, -32768, 32767, 'u16')
        prim_struct(file, 'u32', 10, 0, 4294967295)
        prim_struct(file, 'i32', 11, -2147483648, 2147483647, 'u32')
        prim_struct(file, 'u64', 19, 0, 18446744073709551615)
        prim_struct(file, 'i64', 20, -9223372036854775808, 9223372036854775807, 'u64')
        
        # TODO: Enable for 128 bit? Need to link to external symbol "__divti3" in that case.
        #prim_struct(file, 'u128', 39, 0, 340282366920938463463374607431768211455)
        #prim_struct(file, 'i128', 40, -170141183460469231731687303715884105728, 170141183460469231731687303715884105727, 'u128')
