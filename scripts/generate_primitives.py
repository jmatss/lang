import os

CURRENT_FILE_DIR = os.path.dirname(__file__)
STD_PRIMITIVE_PATH = os.path.join(CURRENT_FILE_DIR, '..', 'std', 'primitive')

def heading(file):
    output = '''/*
 * Auto generated with `generate_primitives.py`, do not edit manually.
 */
use std::Hash
use std::Result
use std::primitive::digit_to_ascii
use std::primitive::hash
use std::string::String
use std::string::StringView

'''
    file.write(output)

def abs_fn(is_signed, name, name_unsigned=None):
    if is_signed:
        return '''    pub fn this abs() -> {1} {{
        if this >= 0 {{
            return this as {1}
        }} else this == this::min() {{
            return ((this + 1) * (-1)) as {1} + 1
        }} else {{
            return (this * (-1)) as {1}
        }}
    }}'''.format(name, name_unsigned)
    else:
        return '''    pub fn this abs() -> {0} {{
        return this
    }}'''.format(name)

# TODO: Can merge most of the logic in `to_string` and `to_string_view`.
def prim_struct(file, name, str_byte_size, min, max, name_unsigned=None):
    if not name_unsigned:
        name_unsigned = name
    abs_output = abs_fn(name != name_unsigned, name, name_unsigned)

    output = '''pub struct {0} {{
    pub fn this to_string() -> Result<String, StringView> {{
        var is_negative = this < 0
        var num_abs = this.abs()

        var buf: [u8: {1}]
        var buf_idx = 0

        while true {{
            var digit = num_abs % 10
            var ascii = digit_to_ascii(digit as u8)
            buf.[buf_idx] = ascii
            buf_idx.++

            num_abs /= 10
            if num_abs == 0 {{
                break
            }}
        }}

        var str_len
        if is_negative {{
            str_len = buf_idx + 1
        }} else {{
            str_len = buf_idx
        }}

        var str_or_error = String::init(str_len)
        if err is str_or_error.error {{
            return Result::error(err)
        }}
        var str = str_or_error.get_success()

        if is_negative {{
            str.append_ascii(45) // 45 == '-'
        }}

        var idx = buf_idx - 1
        while true {{
            var ascii = buf.[idx]
            str.append_ascii(ascii)

            if idx == 0 {{
                break
            }}

            idx.--
        }}

        return Result::success(str)
    }}

    pub fn this to_string_view(data: {{[u8: {1}]}}) -> StringView {{
        var is_negative = this < 0
        var num_abs = this.abs()

        var buf: [u8: {1}]
        var buf_idx = 0

        while true {{
            var digit = num_abs % 10
            var ascii = digit_to_ascii(digit as u8)
            buf.[buf_idx] = ascii
            buf_idx.++

            num_abs /= 10
            if num_abs == 0 {{
                break
            }}
        }}

        var data_idx = 0
        if is_negative {{
            data.*.[data_idx] = 45 // 45 == '-'
            data_idx.++
        }}

        buf_idx.--
        while true {{
            var ascii = buf.[buf_idx]
            data.*.[data_idx] = ascii
            data_idx.++

            if buf_idx == 0 {{
                break
            }}

            buf_idx.--
        }}

        return StringView::new(data as {{u8}}, 0, data_idx)
    }}

{2}

    pub fn min() -> {0} {{
        return {3}
    }}

    pub fn max() -> {0} {{
        return {4}
    }}
}}

'''.format(name, str_byte_size, abs_output, min, max)
    file.write(output)

def hash(file, name):
    output = '''impl {0}: Hash {{
    pub fn this hash() -> u32 {{
        return hash(this as u32)
    }}
}}
'''.format(name)
    file.write(output)

if __name__ == '__main__':
    with open(os.path.join(STD_PRIMITIVE_PATH, "u8.ren"), 'w') as file:
        heading(file)
        prim_struct(file, 'u8', 3, 0, 255)
        hash(file, 'u8')
    with open(os.path.join(STD_PRIMITIVE_PATH, "i8.ren"), 'w') as file:
        heading(file)
        prim_struct(file, 'i8', 4, -128, 127, 'u8')
        hash(file, 'i8')
    with open(os.path.join(STD_PRIMITIVE_PATH, "u16.ren"), 'w') as file:
        heading(file)
        prim_struct(file, 'u16', 5, 0, 65535)
        hash(file, 'u16')
    with open(os.path.join(STD_PRIMITIVE_PATH, "i16.ren"), 'w') as file:
        heading(file)
        prim_struct(file, 'i16', 6, -32768, 32767, 'u16')
        hash(file, 'i16')
    with open(os.path.join(STD_PRIMITIVE_PATH, "u32.ren"), 'w') as file:
        heading(file)
        prim_struct(file, 'u32', 10, 0, 4294967295)
        hash(file, 'u32')
    with open(os.path.join(STD_PRIMITIVE_PATH, "i32.ren"), 'w') as file:
        heading(file)
        prim_struct(file, 'i32', 11, -2147483648, 2147483647, 'u32')
        hash(file, 'i32')
    with open(os.path.join(STD_PRIMITIVE_PATH, "u64.ren"), 'w') as file:
        heading(file)
        prim_struct(file, 'u64', 19, 0, 18446744073709551615)
        hash(file, 'u64')
    with open(os.path.join(STD_PRIMITIVE_PATH, "i64.ren"), 'w') as file:
        heading(file)
        prim_struct(file, 'i64', 20, -9223372036854775808, 9223372036854775807, 'u64')
        hash(file, 'i64')
