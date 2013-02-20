@visibility("public")

namespace std
{
 function<void> print(any) in "stdio.ssm";
 function<void> println(any) in "stdio.ssm";
 function<void> newline() in "stdio.ssm";
 function<bool> key_pressed() in "stdio.ssm";
 function<char> get_char() in "stdio.ssm";
 function<char> read_char() in "stdio.ssm";
 function<char> read_char_t(string) in "stdio.ssm";
 function<string> read_string() in "stdio.ssm";
 function<string> read_string_t(string) in "stdio.ssm";
 function<int> read_int() in "stdio.ssm";
 function<int> read_int_t(string) in "stdio.ssm";
 function<float> read_float() in "stdio.ssm";
 function<float> read_float_t(string) in "stdio.ssm";
 function<string> read_until(char) in "stdio.ssm";
 function<string> read_until_t(string, char) in "stdio.ssm";
 function<void> wait_for(char) in "stdio.ssm";
 function<void> wait_for_t(string, char) in "stdio.ssm";
 function<void> set_size_ex(int, int, int, int) in "stdio.ssm";
 function<void> set_size(int, int) in "stdio.ssm";
 function<void> set_buffered(bool) in "stdio.ssm";
 function<void> flush() in "stdio.ssm";
 function<void> clear() in "stdio.ssm";
 function<void> hide_cursor() in "stdio.ssm";
 function<void> show_cursor() in "stdio.ssm";
}
